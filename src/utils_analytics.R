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
    filter(자산군 == "<합계>") %>%
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
    slice(-1)

  return(df)
}


# 2-1. 자산군별 손익누계 그래프용 데이터 생성====
#'
#' @param return_tbl return dbplyr tbl
#' @param start 시작일 (Date)
#' @param end 종료일 (Date)
#' @return list(선진국, 신흥국, 실물자산, 인컴자산, 채권, 현금성) — 각 tibble
build_asset_profit_data <- function(return_tbl, start, end) {
  # [헬퍼] 필터된 df_raw(기준일, 총손익)로 손익누계 산출
  calc_cumprofit <- function(df_raw, label) {
    df_raw %>%
      arrange(기준일) %>%
      group_by(연도 = year(기준일)) %>%
      mutate(
        총손익_1 = lag(총손익, default = 0),
        일간손익  = if_else(기준일 == start, 0, 총손익 - 총손익_1) / 10000
      ) %>%
      ungroup() %>%
      mutate(손익누계 = cumsum(일간손익)) %>%
      slice(-1) %>%
      transmute(기준일, 손익누계, 구분 = label)
  }

  # DB에서 해당 기간 전체 수집 (한 번만 collect)
  base <- return_tbl %>%
    filter(기준일 >= start, 기준일 <= end) %>%
    collect() %>%
    mutate(기준일 = as.Date(기준일))

  # 선진국: 자산군=주식, 세부자산군=선진국, 세부자산군2=""
  df_선진국 <- base %>%
    filter(자산군 == "주식", 세부자산군 == "선진국", 세부자산군2 == "") %>%
    transmute(기준일, 총손익) %>%
    calc_cumprofit("선진국")

  # 신흥국: 자산군=주식, 세부자산군 in(국내, 신흥국), 세부자산군2="" → 기준일별 합산
  df_신흥국 <- base %>%
    filter(
      자산군 == "주식",
      세부자산군 %in% c("국내", "신흥국"),
      세부자산군2 == ""
    ) %>%
    group_by(기준일) %>%
    summarise(총손익 = sum(총손익, na.rm = TRUE), .groups = "drop") %>%
    calc_cumprofit("신흥국")

  # 실물자산: 자산군=대체자산, 세부자산군=실물자산, 세부자산군2=""
  df_실물 <- base %>%
    filter(자산군 == "대체자산", 세부자산군 == "실물자산", 세부자산군2 == "") %>%
    transmute(기준일, 총손익) %>%
    calc_cumprofit("실물자산")

  # 인컴자산: 자산군=대체자산, 세부자산군=인컴자산, 세부자산군2=""
  df_인컴 <- base %>%
    filter(자산군 == "대체자산", 세부자산군 == "인컴자산", 세부자산군2 == "") %>%
    transmute(기준일, 총손익) %>%
    calc_cumprofit("인컴자산")

  # 채권: 자산군=채권, 세부자산군="", 세부자산군2=""
  df_채권 <- base %>%
    filter(자산군 == "채권", 세부자산군 == "", 세부자산군2 == "") %>%
    transmute(기준일, 총손익) %>%
    calc_cumprofit("채권")

  # 현금성: 자산군=현금성, 세부자산군="", 세부자산군2=""
  df_현금성 <- base %>%
    filter(자산군 == "현금성", 세부자산군 == "", 세부자산군2 == "") %>%
    transmute(기준일, 총손익) %>%
    calc_cumprofit("현금성")

  list(
    선진국 = df_선진국,
    신흥국 = df_신흥국,
    실물자산 = df_실물,
    인컴자산 = df_인컴,
    채권 = df_채권,
    현금성 = df_현금성
  )
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
  # [지역 헬퍼 함수] 네이버 회사채 금리 크롤링====
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

  ret_xts <- suppressWarnings(timetk::tk_xts(all_bm_returns_wide, date_var = date, silent = TRUE))
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


# 7. 자산군별 수익률 vs BM 데이터 생성 ====
#'
#' @param return_tbl return dbplyr tbl
#' @param start 시작일 (Date) — 누적수익률 기준점(0%)
#' @param end   종료일 (Date)
#' @return list(선진국, 국내, 실물자산, 인컴자산, 채권)
#'         각 요소: tibble(기준일, MyPF, BM, DD)
build_asset_bm_data <- function(return_tbl, start, end) {
  start <- as.Date(start)
  end   <- as.Date(end)

  all_dates_df <- tibble(기준일 = seq.Date(start, end, by = "day"))

  # [헬퍼] 네이버 회사채 금리 크롤링 ----
  get_bond_yield_local <- function(start_date, end_date) {
    base_url <- paste0(
      "https://finance.naver.com/marketindex/",
      "interestDailyQuote.naver?marketindexCd=IRR_CORP03Y&page="
    )
    page <- 1
    results <- list()
    repeat {
      url <- paste0(base_url, page)
      req <- httr::GET(url, httr::user_agent("Mozilla/5.0"))
      html <- rvest::read_html(req)
      tbls <- rvest::html_table(html)
      if (length(tbls) == 0) break
      df <- tbls[[1]][, 1:2]
      names(df) <- c("date", "rate")
      df <- df %>% filter(!is.na(date), date != "")
      if (nrow(df) == 0) break
      df$date <- as.Date(gsub("\\.", "-", df$date))
      df$rate <- as.numeric(df$rate)
      results[[page]] <- df
      if (min(df$date, na.rm = TRUE) <= as.Date(start_date)) break
      page <- page + 1
      Sys.sleep(0.1)
    }
    bind_rows(results) %>%
      filter(date >= as.Date(start_date), date <= as.Date(end_date)) %>%
      arrange(date) %>%
      distinct(date, .keep_all = TRUE)
  }

  # [헬퍼] 자산군별 MyPF 일간수익률 반환 ----
  calc_mypf_daily <- function(df_asset) {
    if (nrow(df_asset) == 0) {
      return(all_dates_df %>% mutate(r_mypf = 0.0))
    }

    res <- df_asset %>%
      arrange(기준일) %>%
      group_by(연도 = year(기준일)) %>%
      mutate(총손익_1 = lag(총손익, default = 0)) %>%
      ungroup() %>%
      mutate(
        일간손익 = if_else(
          기준일 == min(기준일, na.rm = TRUE), 0.0, as.numeric(총손익 - 총손익_1)
        ),
        r_mypf = if_else(
          기준일 == min(기준일, na.rm = TRUE) | is.na(lag(평가금액)) | lag(평가금액) == 0,
          0.0,
          as.numeric(일간손익 / lag(평가금액) * 100)
        )
      ) %>%
      select(기준일, r_mypf)

    all_dates_df %>%
      left_join(res, by = "기준일") %>%
      mutate(r_mypf = replace_na(r_mypf, 0.0))
  }

  # 1) MyPF 일간수익률 수집 (DB) ----
  base <- return_tbl %>%
    filter(기준일 >= start, 기준일 <= end) %>%
    collect() %>%
    mutate(기준일 = as.Date(기준일))

  mypf_선진국 <- base %>%
    filter(자산군 == "주식", 세부자산군 == "선진국", 세부자산군2 == "") %>%
    select(기준일, 총손익, 평가금액) %>%
    calc_mypf_daily()

  mypf_국내 <- base %>%
    filter(자산군 == "주식", 세부자산군 == "국내", 세부자산군2 == "") %>%
    select(기준일, 총손익, 평가금액) %>%
    calc_mypf_daily()

  mypf_실물 <- base %>%
    filter(자산군 == "대체자산", 세부자산군 == "실물자산", 세부자산군2 == "") %>%
    select(기준일, 총손익, 평가금액) %>%
    calc_mypf_daily()

  mypf_인컴 <- base %>%
    filter(자산군 == "대체자산", 세부자산군 == "인컴자산", 세부자산군2 == "") %>%
    select(기준일, 총손익, 평가금액) %>%
    calc_mypf_daily()

  mypf_채권 <- base %>%
    filter(자산군 == "채권", 세부자산군 == "", 세부자산군2 == "") %>%
    select(기준일, 총손익, 평가금액) %>%
    calc_mypf_daily()

  # 2) BM 가격 수집 (야후 파이낸스) ----
  bm_fetch_start <- start - days(7)
  tickers <- c("360200.KS", "305050.KS", "411060.KS", "329200.KS")

  prices <- suppressWarnings(
    tidyquant::tq_get(tickers,
      get = "stock.prices",
      from = bm_fetch_start, to = end
    )
  ) %>%
    select(date, symbol, adjusted) %>%
    filter(!is.na(adjusted)) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    arrange(date)

  # 3) 네이버 회사채 금리 ----
  bond_yields <- get_bond_yield_local(bm_fetch_start, end)

  # 4) 병합 및 결측 보간 ----
  all_data <- prices %>%
    left_join(bond_yields, by = "date") %>%
    fill(everything(), .direction = "downup") %>%
    arrange(date)

  # 5) BM 일별 수익률 ----
  bm_daily_raw <- all_data %>%
    mutate(
      r_선진국 = (`360200.KS` / lag(`360200.KS`) - 1) * 100,
      r_국내   = (`305050.KS` / lag(`305050.KS`) - 1) * 100,
      r_실물   = (`411060.KS` / lag(`411060.KS`) - 1) * 100,
      r_인컴   = (`329200.KS` / lag(`329200.KS`) - 1) * 100,
      r_채권   = ((1 + replace_na(rate, 0) / 100)^(1 / 252) - 1) * 100
    ) %>%
    filter(date >= start) %>%
    mutate(across(starts_with("r_"), ~ replace_na(.x, 0))) %>%
    rename(기준일 = date) %>%
    select(기준일, starts_with("r_"))

  bm_daily <- all_dates_df %>%
    left_join(bm_daily_raw, by = "기준일") %>%
    mutate(across(starts_with("r_"), ~ replace_na(.x, 0.0)))

  # 6) MyPF 일간수익률 + BM 일간수익률 병합 ----
  calc_cum <- function(r) (cumprod(1 + r / 100) - 1) * 100
  calc_dd  <- function(cum_r) {
    cr   <- 1 + cum_r / 100
    peak <- cummax(cr)
    (cr - peak) / peak * 100
  }

  join_asset <- function(mypf_daily_df, bm_r_col) {
    bm_r <- bm_daily %>% select(기준일, r_bm = !!sym(bm_r_col))

    joined <- mypf_daily_df %>%
      left_join(bm_r, by = "기준일") %>%
      mutate(
        r_mypf = replace_na(r_mypf, 0.0),
        r_bm   = replace_na(r_bm, 0.0)
      ) %>%
      arrange(기준일)

    if (nrow(joined) == 0) {
      return(tibble(기준일 = as.Date(character()),
                    MyPF = numeric(), BM = numeric(), DD = numeric()))
    }

    joined <- joined %>%
      mutate(
        r_mypf = if_else(row_number() == 1L, 0.0, as.numeric(r_mypf)),
        r_bm   = if_else(row_number() == 1L, 0.0, as.numeric(r_bm))
      )

    joined %>%
      mutate(
        MyPF = calc_cum(r_mypf),
        BM   = calc_cum(r_bm),
        DD   = calc_dd(BM)
      ) %>%
      select(기준일, MyPF, BM, DD)
  }

  list(
    선진국   = join_asset(mypf_선진국, "r_선진국"),
    국내     = join_asset(mypf_국내,   "r_국내"),
    실물자산 = join_asset(mypf_실물,   "r_실물"),
    인컴자산 = join_asset(mypf_인컴,   "r_인컴"),
    채권     = join_asset(mypf_채권,   "r_채권")
  )
}


# 8. 개별 종목 분석 데이터 생성 ====
#'
#' @param ticker 분석 대상 티커 (한국: "005930", 미국: "SPY")
#' @param bm_ticker 벤치마크 티커 ("226490.KS" or "SPY")
#' @param today 기준일 (Date). 기본값 Sys.Date()
#' @param ticker_name 한국어 종목명
#' @param bm_name 한국어 벤치마크명
#' @return list(cum_df, dd_df, monthly_ret, yearly_ret,
#'              rolling_vol, rolling_sharpe, stats_df,
#'              ticker_label, bm_label)
build_ticker_analysis_data <- function(ticker, bm_ticker, today = Sys.Date(), ticker_name = NULL, bm_name = NULL) {
  today <- as.Date(today)

  to_yf <- function(t) if (grepl("^[0-9]{6}$", t)) paste0(t, ".KS") else t

  yf_ticker <- to_yf(ticker)
  yf_bm     <- to_yf(bm_ticker)
  t_label   <- if (!is.null(ticker_name) && !is.na(ticker_name) && nchar(as.character(ticker_name)) > 0) as.character(ticker_name) else yf_ticker
  b_label   <- if (!is.null(bm_name) && !is.na(bm_name) && nchar(as.character(bm_name)) > 0) as.character(bm_name) else yf_bm

  analysis_start <- as.Date(paste0(year(today) - 10, "-01-01"))
  rolling_window <- 756L
  fetch_start    <- analysis_start - days(rolling_window + 90)

  raw <- tryCatch(
    suppressWarnings(
      tidyquant::tq_get(
        c(yf_ticker, yf_bm),
        get  = "stock.prices",
        from = fetch_start,
        to   = today
      )
    ),
    error = function(e) NULL
  )

  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  returns_all <- raw %>%
    select(date, symbol, adjusted) %>%
    filter(!is.na(adjusted)) %>%
    group_by(symbol) %>%
    tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = "daily",
      col_rename = "returns"
    ) %>%
    ungroup()

  target_ret <- returns_all %>% filter(symbol == yf_ticker) %>% arrange(date)
  bm_ret     <- returns_all %>% filter(symbol == yf_bm)     %>% arrange(date)

  if (nrow(target_ret) < 10 || nrow(bm_ret) < 10) return(NULL)

  data_span      <- nrow(target_ret)
  actual_window  <- as.integer(max(20L, min(756L, floor(data_span * 0.5))))

  target_analysis <- target_ret %>% filter(date >= analysis_start)
  bm_analysis     <- bm_ret     %>% filter(date >= analysis_start)

  joined <- target_analysis %>%
    select(date, ret_t = returns) %>%
    inner_join(bm_analysis %>% select(date, ret_b = returns), by = "date") %>%
    mutate(
      ret_t = replace_na(ret_t, 0),
      ret_b = replace_na(ret_b, 0)
    ) %>%
    arrange(date)

  if (nrow(joined) < 2) return(NULL)

  # 1) 누적수익률
  cum_df <- joined %>%
    mutate(
      ticker = (cumprod(1 + ret_t) - 1) * 100,
      bm     = (cumprod(1 + ret_b) - 1) * 100
    ) %>%
    mutate(
      ticker = ifelse(!is.finite(ticker), 0, ticker),
      bm     = ifelse(!is.finite(bm), 0, bm)
    ) %>%
    select(date, ticker, bm)

  # 2) DrawDown
  dd_df <- joined %>%
    mutate(
      wealth = cumprod(1 + ret_t),
      peak   = cummax(wealth),
      dd     = (wealth - peak) / peak * 100
    ) %>%
    mutate(dd = ifelse(!is.finite(dd), 0, dd)) %>%
    select(date, dd)

  # 3) 월별 수익률 히트맵용
  monthly_ret <- target_analysis %>%
    tq_transmute(
      select     = returns,
      mutate_fun = apply.monthly,
      FUN        = function(x) (prod(1 + x, na.rm = TRUE) - 1) * 100,
      col_rename = "ret"
    ) %>%
    mutate(year = year(date), month = month(date))

  # 4) 연도별 수익률
  yearly_ret <- joined %>%
    group_by(year = year(date)) %>%
    summarise(
      ticker = (prod(1 + ret_t, na.rm = TRUE) - 1) * 100,
      bm     = (prod(1 + ret_b, na.rm = TRUE) - 1) * 100,
      .groups = "drop"
    )

  # 5) 롤링 변동성
  rolling_vol_df <- target_ret %>%
    mutate(
      rolling_sd  = as.numeric(zoo::rollapplyr(returns, width = actual_window, FUN = sd, fill = NA, na.rm = TRUE)),
      rolling_vol = rolling_sd * sqrt(252) * 100
    ) %>%
    filter(!is.na(rolling_vol), date >= analysis_start) %>%
    select(date, rolling_vol)

  # 6) 롤링 샤프
  rolling_sharpe_fn <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NA_real_)
    vol_ann <- sd(x) * sqrt(252)
    if (is.na(vol_ann) || vol_ann == 0) return(NA_real_)
    ret_ann <- mean(x) * 252
    ret_ann / vol_ann
  }

  rolling_sharpe_df <- target_ret %>%
    mutate(
      rolling_sharpe = as.numeric(zoo::rollapplyr(returns, width = actual_window, FUN = rolling_sharpe_fn, fill = NA))
    ) %>%
    filter(!is.na(rolling_sharpe), date >= analysis_start) %>%
    select(date, rolling_sharpe)

  # 7) 성과 통계 테이블
  stats_df <- calc_single_stats(joined, t_label, b_label)

  list(
    cum_df         = cum_df,
    dd_df          = dd_df,
    monthly_ret    = monthly_ret,
    yearly_ret     = yearly_ret,
    rolling_vol    = rolling_vol_df,
    rolling_sharpe = rolling_sharpe_df,
    stats_df       = stats_df,
    ticker_label   = t_label,
    bm_label       = b_label
  )
}


# 8-1. 단일 종목 성과통계 계산 ====
#'
#' @param joined_df inner_join된 일별수익률 tibble (date, ret_t, ret_b)
#' @param ticker_label 종목 레이블 (열 이름에 사용)
#' @param bm_label BM 레이블 (열 이름에 사용)
#' @return tibble(지표, <ticker_label>, <bm_label>)
calc_single_stats <- function(joined_df, ticker_label, bm_label) {
  if (nrow(joined_df) < 10) {
    return(tibble(지표 = character(), .rows = 0))
  }

  if (ticker_label == bm_label) {
    bm_label <- paste0(bm_label, " (BM)")
  }

  ra_xts <- suppressWarnings(joined_df %>%
    select(date, ret_t) %>%
    timetk::tk_xts(select = ret_t, date_var = date, silent = TRUE))

  rb_xts <- suppressWarnings(joined_df %>%
    select(date, ret_b) %>%
    timetk::tk_xts(select = ret_b, date_var = date, silent = TRUE))

  rf_xts <- xts::xts(rep(0, nrow(ra_xts)), order.by = zoo::index(ra_xts))

  safe_calc <- function(expr) {
    tryCatch(as.numeric(expr), error = function(e) NA_real_)
  }

  ann_ret  <- safe_calc(Return.annualized(ra_xts, scale = 252)) * 100
  ann_vol  <- safe_calc(StdDev.annualized(ra_xts, scale = 252)) * 100
  mdd_val  <- safe_calc(maxDrawdown(ra_xts)) * 100
  calmar   <- if (!is.na(mdd_val) && mdd_val != 0 && !is.na(ann_ret)) round(ann_ret / mdd_val, 2) else NA_real_
  win_rate <- safe_calc(sum(joined_df$ret_t > 0, na.rm = TRUE) / sum(!is.na(joined_df$ret_t)) * 100)
  var95    <- safe_calc(VaR(ra_xts, p = 0.95, method = "historical")) * 100
  sharpe   <- safe_calc(SharpeRatio.annualized(ra_xts, Rf = rf_xts, scale = 252))
  sortino  <- safe_calc(SortinoRatio(ra_xts, MAR = rf_xts))

  capm_res <- tryCatch(
    table.CAPM(ra_xts, rb_xts, Rf = rf_xts, scale = 252),
    error = function(e) NULL
  )
  alpha_v <- if (!is.null(capm_res)) safe_calc(capm_res["Alpha", 1]) * 100 else NA_real_
  beta_v  <- if (!is.null(capm_res)) safe_calc(capm_res["Beta",  1])         else NA_real_
  ir_v    <- tryCatch(safe_calc(InformationRatio(ra_xts, rb_xts)), error = function(e) NA_real_)

  bm_ann_ret <- safe_calc(Return.annualized(rb_xts, scale = 252)) * 100
  bm_ann_vol <- safe_calc(StdDev.annualized(rb_xts, scale = 252)) * 100
  bm_mdd     <- safe_calc(maxDrawdown(rb_xts)) * 100
  bm_sharpe  <- safe_calc(SharpeRatio.annualized(rb_xts, Rf = rf_xts, scale = 252))

  metrics <- c(
    "연환산수익률(%)", "연환산변동성(%)", "Sharpe", "Sortino",
    "Calmar", "MDD(%)", "승률(%)", "VaR.95(%)",
    "Alpha(%)", "Beta", "IR"
  )
  t_vals <- c(ann_ret, ann_vol, sharpe, sortino, calmar, mdd_val,
              win_rate, var95, alpha_v, beta_v, ir_v)
  b_vals <- c(bm_ann_ret, bm_ann_vol, bm_sharpe, NA, NA, bm_mdd,
              NA, NA, NA, NA, NA)

  clean_val <- function(v) {
    res <- suppressWarnings(as.numeric(v))
    ifelse(is.na(res) | is.nan(res) | is.infinite(res), NA_real_, round(res, 2))
  }

  df <- tibble(
    지표  = metrics,
    t_col = clean_val(t_vals),
    b_col = clean_val(b_vals)
  )
  names(df)[2] <- ticker_label
  names(df)[3] <- bm_label

  df
}
