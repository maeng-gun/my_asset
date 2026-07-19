# =============================================================================
# utils_analytics.R — 포트폴리오 순수 분석 함수 모음
# =============================================================================
# MyAssets R6 클래스에서 분리된 순수(stateless) 함수들
# 모든 함수는 데이터프레임/tbl 객체를 입력받아 결과를 반환
# DB 직접 쓰기 없음 (읽기는 dbplyr tbl 객체로 수행)
# =============================================================================


# 1. 기간별 종합거래내역 생성 ====
#'
#' @param assets_df 투자자산 마스터 (tibble)
#' @param pension_df 연금자산 마스터 (tibble)
#' @param assets_daily_tbl 투자자산 일별거래 dbplyr tbl
#' @param pension_daily_tbl 연금자산 일별거래 dbplyr tbl
#' @param dates 조회 기간 (Date 벡터, 길이 1 또는 2)
#' @return tibble
calc_total_trading <- function(assets_df, pension_df,
                               assets_daily_tbl, pension_daily_tbl,
                               dates) {
  if (length(dates) == 1) {
    start <- dates
    end <- dates
  } else {
    start <- dates[1]
    end <- dates[2]
  }

  df1 <- assets_df %>%
    bind_rows(pension_df)

  df2 <- assets_daily_tbl %>%
    filter(between(거래일자, start, end)) %>%
    collect() %>%
    bind_rows(
      pension_daily_tbl %>%
        filter(between(거래일자, start, end)) %>%
        collect()
    )

  df3 <- df2 %>%
    left_join(
      (df1 %>% transmute(계좌, 통화, 종목코드, 자산군, 세부자산군, 세부자산군2, 상품명)),
      by = c("계좌", "종목코드")
    ) %>%
    filter(자산군 != "현금성") %>%
    filter(매입액 != 0 | 매도액 != 0) %>%
    select(
      자산군, 세부자산군, 세부자산군2, 통화, 거래일자, 계좌, 상품명,
      매입수량, 매입액, 매도수량, 매도액
    ) %>%
    mutate(매입단가 = 매입액 / 매입수량, .after = 매입액) %>%
    mutate(매도단가 = 매도액 / 매도수량, .after = 매도액) %>%
    arrange(
      자산군, 세부자산군, 세부자산군2, 통화, 상품명, 거래일자,
      desc(매입액), desc(매도액)
    )

  df4 <- df3 %>% summarise(
    거래일자 = NA_Date_, 계좌 = "", 자산군 = "", 세부자산군 = "",
    세부자산군2 = "", 상품명 = "합계", 매도액 = sum(매도액),
    매입액 = sum(매입액), .groups = "drop"
  )

  df3 %>% bind_rows(df4)
}




# 2.종합손익 그래프용 데이터 생성====
#'
#' @param return_tbl return dbplyr tbl
#' @param start 시작일 (Date)
#' @param end 종료일 (Date)
#' @return tibble
build_profit_trend_data <- function(return_tbl, start, end) {
  df <- return_tbl %>%
    filter(자산군 == "주식", 세부자산군 == "국내", 세부자산군2 == "") %>%
    collect() %>%
    transmute(기준일 = as.Date(기준일), 평가금액, 총손익) %>%
    filter(기준일 >= start, 기준일 <= end) %>%
    arrange(기준일) %>%
    group_by(연도 = year(기준일)) %>%
    mutate(
      총손익_1 = lag(총손익, default = 0),
      일간손익 = if_else(기준일 == start, 0, 총손익 - 총손익_1) / 10000
    ) %>%
    ungroup() %>%
    mutate(
      손익누계 = cumsum(일간손익),
      일간수익률 = 일간손익 * 10000 / lag(평가금액, default = 0) * 100
    ) %>%
    slice(-1) %>%
    mutate(누적수익률 = (cumprod(1 + 일간수익률 / 100) - 1) * 100)

  return(df)
}





# 3. 벤치마크 타겟 일자 반환====
## 1) ㅇㅇ ====
#'
#' @param base_month 기준월 (Date)
#' @param today 오늘 날짜 (Date)
#' @return Date
get_target_date <- function(base_month, today) {
  sel_date <- as.Date(paste0(format(base_month, "%Y-%m"), "-01"))
  if (year(sel_date) == year(today) && month(sel_date) == month(today)) {
    return(today)
  } else {
    return(ceiling_date(sel_date, "month") - days(1))
  }
}


# 4. 벤치마크 수익률 종합 데이터 산출====
#'
#' @param return_tbl return dbplyr tbl
#' @param cash_in_out 입출금 tibble
#' @param allo_table_df 자산배분 테이블 (tibble)
#' @param base_month 기준월 (Date)
#' @param today 오늘 날짜 (Date)
#' @return tibble (wide format)
calc_benchmark_returns <- function(return_tbl, cash_in_out, allo_table_df,
                                   base_month, today) {
  # [지역 헬퍼 함수] 네이버 회사채 금리 크롤링
  get_naver_bond_yield <- function(start_date, end_date) {
    base_url <- "https://finance.naver.com/marketindex/interestDailyQuote.naver?marketindexCd=IRR_CORP03Y&page="
    page <- 1
    results <- list()

    repeat {
      url <- paste0(base_url, page)
      req <- httr::GET(url, httr::user_agent("Mozilla/5.0"))
      html <- rvest::read_html(req)
      tables <- rvest::html_table(html)

      if (length(tables) == 0) break
      df <- tables[[1]][, 1:2]
      names(df) <- c("date", "rate")

      df <- df %>% filter(!is.na(date) & date != "")
      if (nrow(df) == 0) break

      df$date <- as.Date(gsub("\\.", "-", df$date))
      df$rate <- as.numeric(df$rate)
      results[[page]] <- df

      if (min(df$date, na.rm = TRUE) <= as.Date(start_date)) break
      page <- page + 1
      Sys.sleep(0.1)
    }

    bind_rows(results) %>%
      filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
      arrange(date) %>%
      distinct(date, .keep_all = TRUE)
  }

  t_date <- get_target_date(base_month, today)
  s_ytd <- floor_date(t_date, "year") - days(1)
  fetch_start <- s_ytd - days(7)

  ## _1) 내 포트폴리오 ----
  pf_return <- return_tbl %>%
    filter(자산군 == "<합계>", 기준일 >= s_ytd, 기준일 <= t_date) %>%
    select(기준일, 평가금액, 총손익) %>%
    collect() %>%
    arrange(기준일) %>%
    group_by(연도 = year(기준일)) %>%
    mutate(
      총손익_1 = lag(총손익, default = 0),
      일간손익 = if_else(기준일 == s_ytd, 0, 총손익 - 총손익_1)
    ) %>%
    ungroup() %>%
    transmute(기준일, MyPF = 일간손익 / lag(평가금액) * 100) %>%
    filter(!is.na(MyPF))

  ## _2) 야후 파이낸스 벤치마크 ----
  tickers <- c("360200.KS", "278530.KS", "411060.KS", "329200.KS", "356540.KS")
  prices <- suppressWarnings(
    tidyquant::tq_get(tickers, get = "stock.prices", from = fetch_start, to = t_date)
  ) %>%
    select(date, symbol, adjusted) %>%
    filter(!is.na(adjusted)) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    arrange(date)

  ## _3) 네이버 회사채 크롤링 ----
  bond_yields <- get_naver_bond_yield(fetch_start, t_date)

  ## _4) 결측치 보간 ----
  merged_prices <- prices %>%
    left_join(bond_yields, by = "date") %>%
    fill(everything(), .direction = "downup")

  ## _5) 주식/실물 자산군 일별 수익률 ----
  bm_returns_long <- merged_prices %>%
    select(date, `360200.KS`, `278530.KS`, `411060.KS`, `329200.KS`, `356540.KS`) %>%
    pivot_longer(cols = -date, names_to = "symbol", values_to = "price") %>%
    drop_na(price) %>%
    group_by(symbol) %>%
    tidyquant::tq_transmute(
      select = price, mutate_fun = periodReturn,
      period = "daily", type = "arithmetic"
    ) %>%
    ungroup() %>%
    mutate(daily.returns = daily.returns * 100) %>%
    mutate(Asset = recode(symbol,
      "360200.KS" = "해외주식",
      "278530.KS" = "국내주식",
      "411060.KS" = "실물자산",
      "329200.KS" = "인컴자산",
      "356540.KS" = "시장형채권"
    )) %>%
    select(date, Asset, daily.returns)

  ## _6) 회사채 및 현금성 자산 생성 ----
  bond_returns_long <- merged_prices %>%
    filter(date >= fetch_start) %>%
    mutate(
      Asset = "만기보유채권",
      daily.returns = ((1 + (rate + 2.0) / 100)^(1 / 252) - 1) * 100
    ) %>%
    select(date, Asset, daily.returns)

  cash_returns_long <- data.frame(date = unique(merged_prices$date)) %>%
    filter(date >= fetch_start) %>%
    mutate(Asset = "현금성", daily.returns = 0)

  all_bm_returns_long <- bind_rows(bm_returns_long, bond_returns_long, cash_returns_long) %>%
    arrange(date, Asset)

  all_bm_returns_wide <- all_bm_returns_long %>%
    pivot_wider(names_from = Asset, values_from = daily.returns) %>%
    arrange(date) %>%
    replace(is.na(.), 0)

  ret_xts <- timetk::tk_xts(all_bm_returns_wide, date_var = date)
  asset_cols <- colnames(ret_xts)

  ## _7) SAA, TAA1, TAA2 포트폴리오 수익률 계산 ----
  weight_df <- allo_table_df %>%
    mutate(
      배분일자 = as.Date(배분일자),
      현금성 = 1 - (국내주식 + 해외주식 + 만기보유채권 + 시장형채권 + 실물자산 + 인컴자산)
    )

  months_grid <- tibble(date = seq(floor_date(as.Date(fetch_start) - months(1), "month"),
    ceiling_date(as.Date(t_date) + months(1), "month"),
    by = "month"
  ) + days(20))

  calc_pf_return <- function(pf_name) {
    w_raw <- weight_df %>%
      filter(구분 == pf_name) %>%
      select(date = 배분일자, 국내주식, 해외주식, 만기보유채권, 시장형채권, 실물자산, 인컴자산, 현금성)

    if (nrow(w_raw) == 0) {
      return(tibble(date = as.Date(character()), !!pf_name := numeric()))
    }
    if (min(w_raw$date) > min(months_grid$date)) {
      pad_w <- w_raw %>%
        filter(date == min(w_raw$date)) %>%
        mutate(date = min(months_grid$date))
      w_raw <- bind_rows(pad_w, w_raw)
    }

    w_monthly <- months_grid %>%
      left_join(w_raw, by = "date") %>%
      fill(all_of(asset_cols), .direction = "down") %>%
      drop_na() %>%
      select(date, all_of(asset_cols))

    suppressWarnings(
      w_xts <- timetk::tk_xts(w_monthly, date_var = date)
    )
    pf_ret_xts <- PerformanceAnalytics::Return.portfolio(R = ret_xts / 100, weights = w_xts)

    pf_ret <- timetk::tk_tbl(pf_ret_xts * 100, rename_index = "date") %>%
      rename(!!pf_name := portfolio.returns)
    return(pf_ret)
  }

  pf_SAA <- calc_pf_return("SAA")
  pf_TAA1 <- calc_pf_return("TAA1")
  pf_TAA2 <- calc_pf_return("TAA2")

  final_wide <- all_bm_returns_long %>% pivot_wider(names_from = Asset, values_from = daily.returns)
  if (nrow(pf_SAA) > 0) final_wide <- final_wide %>% left_join(pf_SAA, by = "date")
  if (nrow(pf_TAA1) > 0) final_wide <- final_wide %>% left_join(pf_TAA1, by = "date")
  if (nrow(pf_TAA2) > 0) final_wide <- final_wide %>% left_join(pf_TAA2, by = "date")

  final_wide <- final_wide %>%
    rename(
      기준일 = date, 코스피 = 국내주식, `S&P` = 해외주식,
      회사채 = 만기보유채권, 금현물 = 실물자산, 리츠 = 인컴자산
    ) %>%
    left_join(pf_return, by = "기준일") %>%
    arrange(기준일) %>%
    replace(is.na(.), 0)

  return(final_wide)
}


# 5. 만기도래자금 분석====
#'
#' @param bs_pl_mkt_a 투자자산 평가 tibble
#' @param bs_pl_mkt_p 연금자산 평가 tibble
#' @param assets_df 투자자산 마스터 tibble
#' @param pension_df 연금자산 마스터 tibble
#' @param today 오늘 날짜 (Date)
#' @return tibble
calc_maturity_analysis <- function(bs_pl_mkt_a, bs_pl_mkt_p,
                                   assets_df, pension_df, today) {
  bs_pl_mkt_a %>%
    bind_rows(bs_pl_mkt_p) %>%
    filter(
      자산군 == "채권", 세부자산군 == "만기보유",
      통화 == "원화", 평가금액 > 0
    ) %>%
    select(계좌, 종목명, 종목코드, 평가금액) %>%
    left_join(
      assets_df %>%
        bind_rows(pension_df) %>%
        select(종목코드, 만기일),
      by = "종목코드"
    ) %>%
    filter(만기일 > today) %>%
    select(계좌, 종목명, 평가금액, 만기일) %>%
    arrange(만기일)
}


# 6. 가용자금 분석====
#'
#' @param t_comm2 상품별/계좌별 보유현황 tibble
#' @param inflow_df 자금유출입 tibble
#' @param maturity_df 만기도래자금 tibble
#' @param today 오늘 날짜 (Date)
#' @param acct_order 계좌 순서 벡터
#' @return list(current_status, total_projection, cash_projection)
calc_liquidity_analysis <- function(t_comm2, inflow_df, maturity_df,
                                    today, acct_order) {
  # [Step 1] 현재 시점 계좌별 총자산/현금성자산 현황

  # 1-1. 계좌별 총자산
  df_total <- t_comm2 %>%
    filter(자산군 == "" | is.na(자산군)) %>%
    select(계좌, 평가금액) %>%
    rename(총자산 = 평가금액)

  # 1-2. 계좌별 현금성자산
  df_cash <- t_comm2 %>%
    filter(자산군 == "현금성") %>%
    group_by(계좌, 자산군) %>%
    summarise(평가금액 = sum(평가금액), .groups = "drop") %>%
    select(계좌, 평가금액) %>%
    rename(현금성자산 = 평가금액)

  # 1-3. 모든 계좌 리스트 확보
  all_accts <- factor(
    unique(c(df_total$계좌, df_cash$계좌)),
    levels = acct_order
  )

  current_status <- tibble(계좌 = all_accts) %>%
    left_join(df_total, by = "계좌") %>%
    left_join(df_cash, by = "계좌") %>%
    replace(is.na(.), 0) %>%
    pivot_longer(cols = -계좌, names_to = "구분", values_to = "금액") %>%
    pivot_wider(names_from = 계좌, values_from = 금액) %>%
    mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE)) %>%
    arrange(구분)

  # [Step 2] 공통 데이터 준비 (월별 피벗)

  # 2-1. 자금유출입 월별 집계
  inflow_monthly <- inflow_df %>%
    mutate(거래월 = format(as.Date(거래일자), "%Y-%m")) %>%
    filter(as.Date(거래일자) >= floor_date(today, "month")) %>%
    group_by(거래월, 계좌) %>%
    summarise(금액 = sum(자금유출입, na.rm = TRUE), .groups = "drop")

  # 2-2. 만기 자산 월별 집계
  maturity_data <- maturity_df %>%
    mutate(거래월 = format(as.Date(만기일), "%Y-%m")) %>%
    group_by(거래월, 계좌) %>%
    summarise(금액 = sum(평가금액, na.rm = TRUE), .groups = "drop")

  # 2-3. 미래 월 리스트 생성
  future_months <- sort(unique(c(inflow_monthly$거래월, maturity_data$거래월)))
  current_month <- format(today, "%Y-%m")

  if (length(future_months) == 0) {
    future_months <- current_month
  } else if (!(current_month %in% future_months)) {
    future_months <- sort(c(current_month, future_months))
  }

  base_proj <- tibble(거래월 = future_months)

  # [Step 3] 향후 총자산 추이 (누적)
  init_total <- df_total %>%
    pivot_wider(names_from = 계좌, values_from = 총자산) %>%
    mutate(거래월 = current_month)

  flow_pivot <- inflow_monthly %>%
    pivot_wider(names_from = 계좌, values_from = 금액, values_fill = 0)

  total_projection <- bind_rows(init_total, flow_pivot) %>%
    right_join(base_proj, by = "거래월") %>%
    group_by(거래월) %>%
    summarise(across(any_of(all_accts), \(x) sum(x, na.rm = TRUE))) %>%
    arrange(거래월) %>%
    mutate(across(any_of(all_accts), ~ cumsum(tidyr::replace_na(., 0)))) %>%
    mutate(합계 = rowSums(select(., -거래월), na.rm = TRUE))

  # [Step 4] 향후 가용자금 추이
  init_cash <- df_cash %>%
    pivot_wider(names_from = 계좌, values_from = 현금성자산) %>%
    mutate(거래월 = current_month)

  total_inflow <- bind_rows(inflow_monthly, maturity_data) %>%
    group_by(거래월, 계좌) %>%
    summarise(금액 = sum(금액, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = 계좌, values_from = 금액, values_fill = 0)

  cash_related_accts <- unique(c(names(init_cash), names(total_inflow)))
  cash_related_accts <- setdiff(cash_related_accts, "거래월")

  cash_projection <- bind_rows(init_cash, total_inflow) %>%
    right_join(base_proj, by = "거래월") %>%
    group_by(거래월) %>%
    summarise(across(any_of(cash_related_accts), \(x) sum(x, na.rm = TRUE))) %>%
    arrange(거래월) %>%
    mutate(across(any_of(cash_related_accts), ~ tidyr::replace_na(., 0))) %>%
    mutate(합계 = rowSums(select(., -거래월), na.rm = TRUE))

  return(list(
    current_status   = current_status,
    total_projection = total_projection,
    cash_projection  = cash_projection
  ))
}
