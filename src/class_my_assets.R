# =============================================================================
# MyAssets R6 클래스 — 포트폴리오 DB 동기화 & 상태 관리 전용
# =============================================================================
# MyData를 상속하며, 장부금액·평가금액·손익·배분 등 DB 연동 로직만 보유
# 순수 계산/시각화 로직은 src/utils_analytics.R로 분리됨
# pool 객체는 외부(app.R)에서 생성하여 주입받음
# =============================================================================

MyAssets <- R6Class(
  classname = "MyAssets",
  inherit = MyData,

  public = list(
    today = NULL, year = NULL, days = NULL,
    assets = NULL, pension = NULL, ex_usd = NULL, ex_jpy = NULL,
    bs_pl_mkt_a = NULL, bs_pl_mkt_p = NULL,
    bl = NULL, my = NULL, ks = NULL,
    t_class = NULL, t_comm = NULL, t_comm2 = NULL,
    t_comm3 = NULL, t_comm4 = NULL, t_comm5 = NULL, comm_profit = NULL,
    inflow = NULL, book_info = NULL, bs_pl_a = NULL, bs_pl_p = NULL,
    assets_last_num = NULL, assets_daily_last_num = NULL,
    pension_last_num = NULL, pension_daily_last_num = NULL,
    inflow_last_num = NULL, cash_in_out = NULL, acct_order = NULL,
    cur_order = NULL, class_order = NULL, class2_order = NULL,
    class3_order = NULL, t_allocation = NULL, account_allocation = NULL,
    y_num = NULL, grid = NULL, future_eval = NULL, closing_prices = NULL,
    account_allocation2 = NULL, comm_profit2 = NULL, t_comm10 = NULL,

## 1. 속성 초기화 ====
    initialize = function(pool) {

      super$initialize(pool)
      self$today <- today()
      self$year  <- year(self$today)

      tryCatch({
        if (dbExistsTable(self$con, 'inflow')) {
          dbExecute(self$con,
                    glue("DELETE FROM inflow WHERE \"거래일자\" < '{self$today}'"))
        }
      }, error = function(e) {
        # 테이블이 없거나 권한 문제 등 에러 발생 시 무시하고 진행
      })

      self$days <- seq(make_date(2024, 1, 1),
                       self$today,
                       by = 'day')

      if (is.null(self$bl)) {
        self$bl <- AutoInvest$new(pool = self$con, account = 'boolio')
      }

      self$acct_order <- c("한투", "불리오", "엔투하영", "금현물",
                           "한투ISA", "엔투ISA", "엔투저축연금",
                           "한투연금저축", "미래DC", "농협IRP", "엔투IRP")
      self$cur_order   <- c("원화", "달러", "엔화")
      self$class_order <- c("<합계>", '', "주식", "대체자산",
                            "채권", "현금성", "외화자산")
      self$class2_order <- c("", "선진국", "국내", "신흥국", "실물자산",
                             "인컴자산", "상품", "부동산인프라",
                             "만기보유", "시장형",
                             "국채", "투자등급", "하이일드", "만기무위험",
                             "만기회사채", "금융상품", "현금", "달러자산",
                             "엔화자산")
      self$class3_order <- c("", "인덱스", "종목", "테마", "귀금속", "원자재",
                             "에너지", "국내", "해외", "안전자산", "크레딧",
                             "부동산", "인프라", "선진국", "신흥국", "단기ETF",
                             "원화상품", "외화상품", "외환", "원화")
    },

## 2.(장부금액) 거래내역 기록 테이블 ====
    get_trading_record = function(table, acct, cur, limit_n) {

      if (table == "투자자산") {
        table_name <- 'assets'
      } else {
        table_name <- 'pension'
      }

      if (table == "투자자산") {
        table <- 'assets'
      } else {
        table <- 'pension'
      }

      suppressWarnings({
        df1 <- self$read_obj(table)
        df2 <- self$read_obj(paste0(table, '_daily'))
      })

      df2 %>% left_join(
        (df1 %>% transmute(계좌, 통화, 종목코드, 종목명)),
        by = c('계좌', '종목코드')) %>%
        filter(계좌 == acct, 통화 == cur) %>%
        dbplyr::window_order(거래일자, 행번호) %>%
        transmute(
          행번호, 계좌, 통화, 거래일자, 종목명,
          매입수량, 매입액, 현금지출,
          매입비용 = 현금지출 - 매입액,
          매도수량, 매도원금, 매도액,
          매매수익 = 매도액 - 매도원금,
          이자배당액, 현금수입,
          매도비용 = 매도액 + 이자배당액 - 현금수입,
          순수익 = 매매수익 + 이자배당액 - 매도비용 - 매입비용,
          입출금,
          순현금수입 = 입출금 + 현금수입 - 현금지출,
          잔액 = cumsum(순현금수입)) %>%
        select(-순현금수입) %>%
        arrange(desc(거래일자), desc(행번호)) %>%
        head(limit_n) %>%
        collect()
    },

## 3.(장부금액) 계좌거래 내역 전처리 ====
    get_daily_trading = function(ast_info, trade) {

      dt_info  <- as.data.table(ast_info)
      dt_trade <- as.data.table(trade)

      grid_dt <- unique(
        dt_info[, .(계좌, 종목코드)]
      )[, .(거래일자 = self$days),
        by = .(계좌, 종목코드)]

      grid_dt <- merge(grid_dt,
                       dt_info[, .(계좌, 종목코드, 종목명, 통화)],
                       by = c("계좌", "종목코드"),
                       all.x = TRUE)

      final_dt <- merge(grid_dt,
                        dt_trade,
                        by = c("계좌", "종목코드", "거래일자"),
                        all.x = TRUE)

      cols_to_fill <- c("매입수량", "매입액", "현금지출", "매도수량", "매도원금",
                        "매도액", "이자배당액", "현금수입", "입출금")

      for (col in cols_to_fill) {
        set(final_dt, which(is.na(final_dt[[col]])), col, 0)
      }

      setkey(final_dt, 계좌, 종목코드, 거래일자)

      final_dt[, `:=`(순매입수량 = 매입수량 - 매도수량,
                      수익 = 매도액 - 매도원금 + 이자배당액,
                      비용 = 현금지출 - 매입액 + 매도액 + 이자배당액 - 현금수입)]
      final_dt[, 실현손익 := 수익 - 비용]

      final_dt[, .(계좌, 종목코드, 종목명, 통화, 거래일자,
                   순매입수량, 매입액, 매도원금,
                   수익, 비용, 실현손익, 현금수입, 입출금, 현금지출)]
    },

## 4.(장부금액) 운용자산 잔액-손익 테이블 생성 ====
    get_bs_pl = function(mode = 'assets', trade_tbl) {

      trade <- copy(trade_tbl)

      if (mode == 'assets') {
        codes <- as.data.table(self$assets)
      } else {
        codes <- as.data.table(self$pension)
      }
      setkey(trade, 계좌, 종목코드, 거래일자)

      trade[, `:=`(보유수량 = cumsum(순매입수량),
                   장부금액 = cumsum(매입액 - 매도원금)),
            by = .(계좌, 종목코드)]
      trade[, 연도 := year(거래일자)]
      trade[, `:=`(평잔 = cummean(장부금액),
                   수익_누적 = cumsum(수익),
                   비용_누적 = cumsum(비용),
                   실현손익_누적 = cumsum(실현손익)),
            by = .(계좌, 종목코드, 연도)]

      bs_pl <- trade[, .(계좌, 종목코드, 거래일자, 종목명, 통화,
                         수익 = 수익_누적,
                         비용 = 비용_누적,
                         실현손익 = 실현손익_누적,
                         보유수량, 장부금액, 평잔)]
      bs_pl <- merge(bs_pl,
                     codes[, .(계좌, 종목코드, 자산군, 세부자산군,
                               세부자산군2)],
                     by = c('계좌', '종목코드'),
                     all.x = TRUE)

      setkey(bs_pl, 계좌, 종목코드, 거래일자)

      # 예수금 & 평잔 처리 (dcast 사용)
      if (mode == 'assets') {
        calc_cash_flow <- function(target_cur) {
          cash_dt <- trade[통화 == target_cur,
                           .(현금 = sum(현금수입 + 입출금 - 현금지출)),
                           by = .(거래일자, 계좌)]

          if (nrow(cash_dt) > 0) {
            cash_wide <- dcast(cash_dt, 거래일자 ~ 계좌, value.var = "현금", fill = 0)
            acct_cols <- setdiff(names(cash_wide), "거래일자")
            cash_b <- copy(cash_wide)
            cash_b[, (acct_cols) := lapply(.SD, cumsum),
                   .SDcols = acct_cols]
            cash_e <- copy(cash_b)
            cash_e[, 연도 := year(거래일자)]
            cash_e[, (acct_cols) := lapply(.SD, dplyr::cummean),
                   by = .(연도), .SDcols = acct_cols]
            cash_e[, 연도 := NULL]
            return(list(b = cash_b, e = cash_e))
          } else {
            return(NULL)
          }
        }

        cw <- calc_cash_flow('원화')
        cd <- calc_cash_flow('달러')
        cy <- calc_cash_flow('엔화')

        if (!is.null(cw)) {
          map_w <- list('엔투ISA예수금' = '엔투ISA',
                        '한투예수금' = '한투',
                        '한투ISA예수금' = '한투ISA',
                        '금현물계좌현금' = '금현물')
          for (nm in names(map_w)) {
            if (map_w[[nm]] %in% names(cw$b)) {
              bs_pl[종목명 == nm,
                    장부금액 := cw$b[.SD, get(map_w[[nm]]), on = "거래일자"]]
              bs_pl[종목명 == nm, 평잔 := cw$e[.SD, get(map_w[[nm]]), on = "거래일자"]]
            }
          }
        }

        if (!is.null(cd)) {
          map_d <- list('불리오달러' = '불리오',
                        '직접운용달러' = '한투')
          for (nm in names(map_d)) {
            if (map_d[[nm]] %in% names(cd$b)) {
              bs_pl[종목명 == nm, 장부금액 := cd$b[.SD, get(map_d[[nm]]), on = "거래일자"]]
              bs_pl[종목명 == nm, 평잔 := cd$e[.SD, get(map_d[[nm]]), on = "거래일자"]]
            }
          }
        }

        if (!is.null(cy) && '한투' %in% names(cy$b)) {
          bs_pl[종목명 == '직접운용엔', 장부금액 := cy$b[.SD, 한투, on = "거래일자"]]
          bs_pl[종목명 == '직접운용엔', 평잔 := cy$e[.SD, 한투, on = "거래일자"]]
        }
      }

      return(bs_pl)
    },

## 5.(장부금액) 장부금액 자료 산출 ====
    run_book = function() {

      # DB 원자료 캐싱
      self$assets  <- self$read('assets')
      self$pension <- self$read('pension')
      self$inflow  <- self$read('inflow')

      # DB 행번호
      self$assets_last_num <- self$read_obj('assets') %>%
        arrange(desc(행번호)) %>% head(1) %>% pull(행번호)
      self$assets_daily_last_num <- self$read_obj('assets_daily') %>%
        arrange(desc(행번호)) %>% head(1) %>% pull(행번호)

      self$pension_last_num <- self$read_obj('pension') %>%
        arrange(desc(행번호)) %>% head(1) %>% pull(행번호)
      self$pension_daily_last_num <- self$read_obj('pension_daily') %>%
        arrange(desc(행번호)) %>% head(1) %>% pull(행번호)

      self$inflow_last_num <- self$read_obj('inflow') %>%
        arrange(desc(행번호)) %>% head(1) %>% pull(행번호)

      assets_daily <-
        self$get_daily_trading(self$assets, self$read('assets_daily'))
      bs_pl_book_a <- self$get_bs_pl('assets', assets_daily)

      pension_daily <-
        self$get_daily_trading(self$pension, self$read('pension_daily'))
      bs_pl_book_p <- self$get_bs_pl('pension', pension_daily)

      self$cash_in_out <- pension_daily %>% as_tibble() %>%
        bind_rows(
          assets_daily %>% as_tibble()
        ) %>%
        filter(통화 == '원화') %>%
        group_by(거래일자) %>%
        summarise(입출금 = sum(입출금)) %>%
        rename(기준일 = 거래일자)

      self$bs_pl_a <- bs_pl_book_a[거래일자 == self$today]
      self$bs_pl_p <- bs_pl_book_p[거래일자 == self$today]

      self$book_info <- bs_pl_book_a %>% as_tibble() %>%
        filter(통화 == '원화') %>%
        filter((month(거래일자) == 12 & day(거래일자) == 31) |
                 거래일자 == self$today) %>%
        bind_rows(
          bs_pl_book_p %>% as_tibble() %>%
            filter((month(거래일자) == 12 & day(거래일자) == 31) |
                     거래일자 == self$today)
        ) %>%
        group_by(연도 = year(거래일자)) %>%
        summarise(장부금액 = sum(장부금액), 평잔 = sum(평잔),
                  실현손익 = sum(실현손익))
    },

## 6.(평가및손익) 가격 업데이트 ====
    update_new_price = function() {

      # 1) 환율
      get_exchange_rate <- function(cur = '달러') {
        num <- c('달러' = 1, '엔' = 2, '유로' = 3, '위안' = 4)
        suppressWarnings({
          (read_html("http://finance.naver.com/marketindex/") %>%
             html_nodes("div.head_info > span.value")
          )[num[cur]] %>%
             html_text() %>%
             readr::parse_number()
        })
      }

      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔') / 100

      # 2) 국내주식 종목/ETF 종가
      all_codes <- tibble(self$bs_pl_a) %>%
        bind_rows(
          tibble(self$bs_pl_p)
        ) %>%
        filter(보유수량 != 0) %>%
        .$종목코드

      target_codes <- unique(all_codes[str_detect(all_codes,
                                                  "^\\d[a-zA-Z0-9]{4}\\d$")])

      closing_prices <-
        tibble(종목코드 = target_codes,
               종가 = self$bl$get_current_price(target_codes))

      # 3) 금가격 종가
      url <- "https://api.stock.naver.com/marketindex/metals/M04020000"

      tryCatch({
        resp <- GET(url = url)
        json_data <- content(resp, as = 'text', encoding = 'UTF-8') %>%
          jsonlite::fromJSON()
        price <- json_data$closePrice %>%
          readr::parse_number()
        gold <- tibble(종목코드 = '04020000', 종가 = price)
      }, error = function(e) {
        message("금 시세 조회 실패")
        gold <<- tibble()
      })

      # 4) 펀드 기준가
      get_fund_price <- function(code) {
        map_dbl(code, function(x) {
          x %>%
            {
              paste0('https://www.funddoctor.co.kr/afn/fund/fprofile2.jsp?fund_cd=', .)
            } %>%
            read_html() %>%
            html_element(xpath = '/html/body/div[1]/div/div[3]/div[2]/div[1]/div[1]') %>%
            html_text() %>%
            stringr::str_remove(',') %>%
            as.numeric()
        })
      }

      fund_codes <- all_codes[(str_sub(all_codes, 1, 2) == 'K5')]
      if (length(fund_codes) > 0) {
        fund_prices <- tibble(종목코드 = fund_codes,
                              종가 = get_fund_price(fund_codes) / 1000)
      } else {
        fund_prices <- tibble()
      }

      # 5) 결합
      self$closing_prices <-
        bind_rows(closing_prices, gold, fund_prices)
    },

## 7.(평가및손익) 투자자산 평가반영 잔액-손익 테이블 생성 ====
    evaluate_bs_pl_assets = function() {

      price <- self$assets %>%
        select(계좌, 종목코드, 상품명, 평가금액) %>%
        filter(평가금액 != 0) %>%
        bind_rows(
          mutate(self$bl$inquire_balance_ovs(), 계좌 = '불리오')
        ) %>%
        select(계좌, 종목코드, 평가금액) %>% as.data.table()

      bs_pl <- self$bs_pl_a

      last_eval <- self$assets %>%
        select(계좌, 종목코드, 기초평가손익) %>%
        as.data.table()

      bs_pl <- merge(bs_pl, price, by = c("계좌", "종목코드"), all.x = TRUE)
      bs_pl <- merge(bs_pl, self$closing_prices, by = '종목코드', all.x = TRUE)
      bs_pl <- merge(bs_pl, last_eval, by = c("계좌", "종목코드"), all.x = TRUE)

      bs_pl <- bs_pl[평잔 > 0.02]
      bs_pl[, 장부금액 := fifelse(장부금액 < 1, 0, 장부금액)]
      bs_pl[, 기초평가손익 := fifelse(is.na(기초평가손익), 0, 기초평가손익)]
      bs_pl[, 평가금액 := fcase(!is.na(평가금액), 평가금액,
                            !is.na(종가), 종가 * 보유수량,
                            rep(TRUE, .N), 장부금액)]

      dollar <- round(sum(bs_pl[통화 == '달러']$평가금액, na.rm = TRUE) * self$ex_usd)
      yen    <- round(sum(bs_pl[통화 == '엔화']$평가금액, na.rm = TRUE) * self$ex_jpy)

      bs_pl[종목명 == '달러자산', 평가금액 := dollar]
      bs_pl[종목명 == '엔화자산', 평가금액 := yen]

      bs_pl[, `:=`(평가손익 = 평가금액 - 장부금액)]
      bs_pl[, `:=`(평가손익증감 = 평가손익 - 기초평가손익)]
      bs_pl[, `:=`(총손익 = 실현손익 + 평가손익증감)]
      bs_pl <- bs_pl[order(-통화, -평가금액)]

      return(as_tibble(bs_pl))
    },

## 8.(평가및손익) 연금 평가반영 잔액-손익 테이블 생성 ====
    evaluate_bs_pl_pension = function() {

      price <- self$pension %>%
        select(계좌, 종목코드, 평가금액) %>%
        filter(평가금액 != 0) %>% as.data.table()

      bs_pl <- self$bs_pl_p

      last_eval <- self$pension %>%
        select(계좌, 종목코드, 기초평가손익) %>%
        as.data.table()

      bs_pl <- merge(bs_pl, price, by = c("계좌", "종목코드"), all.x = TRUE)
      bs_pl <- merge(bs_pl, self$closing_prices, by = '종목코드', all.x = TRUE)
      bs_pl <- merge(bs_pl, last_eval, by = c("계좌", "종목코드"), all.x = TRUE)

      bs_pl <- bs_pl[평잔 > 0.02]
      bs_pl[, 장부금액 := fifelse(장부금액 < 1, 0, 장부금액)]

      bs_pl[, 기초평가손익 := fifelse(is.na(기초평가손익), 0, 기초평가손익)]
      bs_pl[, 평가금액 := fcase(!is.na(평가금액), 평가금액,
                            !is.na(종가), 종가 * 보유수량,
                            rep(TRUE, .N), 장부금액)]

      bs_pl[, `:=`(평가손익 = 평가금액 - 장부금액)]
      bs_pl[, `:=`(평가손익증감 = 평가손익 - 기초평가손익)]
      bs_pl[, `:=`(총손익 = 실현손익 + 평가손익증감)]

      bs_pl[order(-통화, -평가금액)]
      return(as_tibble(bs_pl))
    },

## 9.(평가및손익) 자산군별/상품별 보유현황 생성 ====
    compute_total = function() {
      df <- self$bs_pl_mkt_a
      usd_bs   <- round(filter(df, 통화 == '달러')$장부금액 * self$ex_usd, 0)
      jpy_bs   <- round(filter(df, 통화 == '엔화')$장부금액 * self$ex_jpy, 0)
      usd_eval <- round(filter(df, 통화 == '달러')$평가금액 * self$ex_usd, 0)
      jpy_eval <- round(filter(df, 통화 == '엔화')$평가금액 * self$ex_jpy, 0)

      # 1) 계좌/자산군/상품까지 원재료
      df00 <- self$assets %>%
        bind_rows(self$pension) %>%
        distinct(통화, 계좌, 종목코드, 자산군, 세부자산군, 세부자산군2, 상품명) %>%
        right_join(
          df %>%
            mutate(
              장부금액 = replace(장부금액, 통화 == '달러', usd_bs),
              장부금액 = replace(장부금액, 통화 == '엔화', jpy_bs),
              평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
              평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval)) %>%
            bind_rows(self$bs_pl_mkt_p) %>%
            filter(장부금액 != 0) %>%
            group_by(계좌, 종목코드) %>%
            summarise(
              보유수량 = sum(보유수량),
              장부금액 = sum(장부금액),
              평가금액 = sum(평가금액), .groups = 'drop'),
          by = c('계좌', '종목코드'))

      tryCatch({
        if (dbExistsTable(self$con, 'eval_profit')) {
          dbExecute(self$con, glue("DELETE FROM eval_profit WHERE \"연도\" = '{self$year}'"))
        }
      }, error = function(e) {
      })

      df00 %>%
        filter(통화 == '원화') %>%
        transmute(
          연도 = self$year, 계좌, 종목코드,
          평가손익 = 평가금액 - 장부금액
        ) %>%
        filter(평가손익 != 0) %>%
        self$upsert('eval_profit', c('연도', '계좌', '종목코드'))

      # 1) 계좌없는 자산군~상품까지
      df0 <- df00 %>%
        group_by(종목코드) %>%
        summarise(통화 = last(통화),
                  자산군 = last(자산군),
                  세부자산군 = last(세부자산군),
                  세부자산군2 = last(세부자산군2),
                  상품명 = last(상품명),
                  보유수량 = sum(보유수량),
                  장부금액 = sum(장부금액),
                  평가금액 = sum(평가금액),
                  .groups = 'drop') %>%
        select(-종목코드)

      # 3) 합계
      df2 <- df0 %>%
        filter(통화 == '원화') %>%
        select(-통화) %>%
        summarise(자산군 = "<합계>", 세부자산군 = '',
                  세부자산군2 = '', 상품명 = '', 보유수량 = 0,
                  장부금액 = sum(장부금액),
                  평가금액 = sum(평가금액), .groups = 'drop')

      # 4) 상품까지(외화빼고)
      df1 <- df0 %>%
        select(-통화) %>%
        filter(자산군 != "외화자산")

      # 5) 자산군 소계
      df3 <- df1 %>%
        group_by(자산군) %>%
        summarise(세부자산군 = '', 세부자산군2 = '', 상품명 = '', 보유수량 = 0,
                  장부금액 = sum(장부금액),
                  평가금액 = sum(평가금액))

      # 6) 세부자산군 소계
      df4 <- df1 %>%
        group_by(자산군, 세부자산군) %>%
        summarise(세부자산군2 = '', 상품명 = '', 보유수량 = 0,
                  장부금액 = sum(장부금액),
                  평가금액 = sum(평가금액), .groups = 'drop')

      # 7) 세부자산군2 소계
      df5 <- df1 %>%
        group_by(자산군, 세부자산군, 세부자산군2) %>%
        summarise(상품명 = "", 보유수량 = 0, 장부금액 = sum(장부금액),
                  평가금액 = sum(평가금액), .groups = 'drop')

      # 8) 환차손익 계산
      df6 <- tibble_row(
        자산군 = '환차손익', 세부자산군 = '', 세부자산군2 = '',
        보유수량 = 0,
        평가금액 = 0,
        평가손익 = (sum(df3$장부금액) - df2$장부금액),
        평가수익률 = round(평가손익 / df2$장부금액 * 100, 2)
      )

      # 합치기(df2,3,4,5,6)
      df7 <- bind_rows(df2, df3, df4, df5) %>%
        select(-보유수량) %>%
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액, 0),
          평가수익률 = round(평가손익 / 장부금액 * 100, 2)
        ) %>%
        select(!c(상품명, 장부금액)) %>%
        bind_rows(df6 %>%
                    select(-보유수량))

      # 상품별 보유현황테이블1 최종
      self$t_comm <- bind_rows(df1, df2, df3, df4, df5) %>%
        mutate(자산군 = factor(자산군, levels = self$class_order),
               세부자산군 = factor(세부자산군, levels = self$class2_order),
               세부자산군2 = factor(세부자산군2, levels = self$class3_order)
        ) %>%
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액), 상품명) %>%
        mutate(
          평단가 = round(장부금액 / 보유수량, 0),
          평단가 = replace(평단가, is.infinite(평단가),0),
          현재가 = round(평가금액 / 보유수량, 0),
          현재가 = replace(현재가, is.infinite(현재가),0),
          평가손익 = round(평가금액 - 장부금액, 0),
          평가수익률 = round(평가손익 / 장부금액 * 100, 2)
        ) %>%
        bind_rows(df6)

      # 상품별/계좌별 보유현황테이블2 최종
      self$t_comm2 <- df00 %>%
        select(계좌, 자산군, 세부자산군, 세부자산군2, 통화, 상품명, 보유수량,
               장부금액, 평가금액) %>%
        bind_rows(
          df00  %>%
            filter(자산군 != '외화자산') %>%
            group_by(계좌) %>%
            summarise(
              자산군 = '', 세부자산군 = '', 세부자산군2 = '',
              상품명 = '', 보유수량 = 0, 장부금액 = sum(장부금액),
              평가금액 = sum(평가금액))
        ) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액, 0),
          평가수익률 = round(평가손익 / 장부금액 * 100, 2),
          계좌 = factor(계좌, levels = self$acct_order),
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order)) %>%
        select(-장부금액) %>%
        arrange(계좌, 자산군, 세부자산군, 세부자산군2,
                desc(평가수익률), 상품명) %>%
        filter(자산군 != '외화자산')

      # 상품별 보유현황테이블3 최종
      self$t_comm10 <- df00 %>%
        select(자산군, 세부자산군, 세부자산군2, 상품명, 평가금액, 계좌) %>%
        filter(자산군 != '외화자산') %>%
        mutate(
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
          계좌 = factor(계좌, levels = self$acct_order)
        ) %>%
        arrange(자산군, 세부자산군, 세부자산군2, 계좌)
    },

## 10.(평가및손익) 자산배분 생성 ====
    compute_total_allocation = function() {

      self$account_allocation <- self$read('groups') %>%
        left_join(
          self$t_comm2 %>%
            select(계좌, 자산군, 세부자산군, 세부자산군2, 평가금액) %>%
            mutate(자산군 = if_else(자산군 == "" | is.na(자산군),
                                 "합계", as.character(자산군))) %>%
            group_by(계좌, 자산군, 세부자산군, 세부자산군2) %>%
            summarise(평가금액 = sum(평가금액), .groups = 'drop') %>%
            pivot_wider(names_from = 계좌, values_from = 평가금액) %>%
            select(자산군, 세부자산군, 세부자산군2,
                   한투연금저축, 엔투저축연금, 미래DC, 엔투IRP, 농협IRP,
                   엔투ISA, 한투ISA, 엔투하영, 불리오, 금현물, 한투) %>%
            mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE)),
          by = c('자산군', '세부자산군', '세부자산군2')
        ) %>%
        mutate(비중 = 합계 / last(합계) * 100)
    },

## 11.(평가및손익) 자산군별/계좌별 손익현황 생성 ====
    compute_asset_profit = function() {

      df1 <- self$bs_pl_mkt_a %>%
        filter(통화 == '원화') %>%
        bind_rows(self$bs_pl_mkt_p) %>%
        mutate(
          is_target = (자산군 == '외화자산' & 세부자산군 == '달러자산'),
          자산군 = if_else(is_target, '주식', 자산군),
          세부자산군 = if_else(is_target, '선진국', 세부자산군),
          세부자산군2 = if_else(is_target, '종목', 세부자산군2)
        ) %>%
        select(-is_target)

      summ_fun <- function(df) {
        df %>%
          summarise(
            장부금액 = sum(장부금액),
            평잔 = sum(평잔),
            비용 = sum(비용),
            평가금액 = sum(평가금액),
            평가손익 = sum(평가손익),
            실현손익 = sum(실현손익),
            평가손익증감 = sum(평가손익증감),
            총손익 = sum(총손익),
            .groups = 'drop'
          )
      }

      df2 <- df1 %>%
        group_by(자산군, 세부자산군, 세부자산군2) %>%
        summ_fun()

      df3 <- df1 %>%
        group_by(자산군, 세부자산군) %>%
        summ_fun() %>%
        mutate(세부자산군2 = "", .after = 2)

      df4 <- df1 %>%
        group_by(자산군) %>%
        summ_fun() %>%
        mutate(세부자산군 = "", 세부자산군2 = "", .after = 1)

      df5 <- df1 %>%
        summ_fun() %>%
        mutate(자산군 = "<합계>", 세부자산군 = "", 세부자산군2 = "", .before = 1)

      # 자산군별 손익현황
      self$t_comm3 <- bind_rows(df2, df3, df4, df5) %>%
        mutate(
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>%
        select(-비용) %>%
        arrange(자산군, 세부자산군, 세부자산군2)

      # 자산군별 손익현황 DB 업로드
      tryCatch({
        if (dbExistsTable(self$con, 'return')) {
          dbExecute(self$con, glue("DELETE FROM return WHERE \"기준일\" = '{self$today}'"))
        }
      }, error = function(e) {
      })

      self$t_comm3 %>%
        select(자산군:세부자산군2, 평가금액, 총손익, 총수익률) %>%
        mutate(기준일 = self$today, .before = 1) %>%
        self$upsert('return', c('기준일', '자산군', '세부자산군', '세부자산군2'))

      df7 <- df1 %>%
        group_by(계좌, 자산군) %>%
        summ_fun()

      df8 <- df1 %>% group_by(계좌) %>%
        summ_fun() %>%
        mutate(자산군 = '', .after = 1)

      # 계좌별 손익현황
      self$t_comm4 <- bind_rows(df7, df8) %>%
        mutate(
          계좌 = factor(계좌, levels = self$acct_order),
          자산군 = factor(자산군, levels = self$class_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>%
        select(-비용) %>%
        arrange(계좌, 자산군)
    },

## 12.(평가및손익) 상품별 손익현황 계산 ====
    compute_comm_profit = function() {

      df_a <- self$bs_pl_mkt_a
      df_p <- self$bs_pl_mkt_p

      self$comm_profit <- bind_rows(df_a, df_p) %>%
        mutate(
          계좌 = factor(계좌, levels = self$acct_order),
          통화 = factor(통화, levels = self$cur_order),
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>%
        arrange(계좌, 통화, 자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        select(계좌, 통화, 자산군, 세부자산군, 세부자산군2, 종목명, 보유수량,
               장부금액, 평잔, 평가금액, 평가손익, 실현손익, 평가손익증감,
               총손익, 비용률, 실현수익률, 평가증감률, 총수익률)

      self$comm_profit2 <- bind_rows(df_a, df_p) %>%
        mutate(
          계좌 = factor(계좌, levels = self$acct_order),
          통화 = factor(통화, levels = self$cur_order),
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>%
        arrange(자산군, 세부자산군, 세부자산군2, 통화, 종목명, 계좌, desc(평가금액)) %>%
        select(자산군, 세부자산군, 세부자산군2, 종목명, 계좌, 통화, 보유수량,
               장부금액, 평잔, 평가금액, 평가손익, 실현손익, 평가손익증감,
               총손익, 비용률, 실현수익률, 평가증감률, 총수익률)
    },

## 13.(평가및손익) 평가금액 계산 ====
    run_valuation = function() {
      self$update_new_price()
      self$bs_pl_mkt_a <- self$evaluate_bs_pl_assets()
      self$bs_pl_mkt_p <- self$evaluate_bs_pl_pension()
      self$compute_total()
      self$compute_total_allocation()
      self$compute_asset_profit()
      self$compute_comm_profit()
    },

## 14.(연초갱신) 기초평가손익 갱신 ====
    renew_last_eval_profit = function() {
      self$assets %>%
        select(-기초평가손익) %>%
        left_join(
          self$read('eval_profit') %>%
            filter(연도 == self$year - 1) %>%
            select(-연도) %>%
            rename(기초평가손익 = 평가손익),
          by = c('계좌', '종목코드')
        ) %>%
        arrange(행번호) %>%
        self$add_table('assets')

      self$pension %>%
        select(-기초평가손익) %>%
        left_join(
          self$read('eval_profit') %>%
            filter(연도 == self$year - 1) %>%
            select(-연도) %>%
            rename(기초평가손익 = 평가손익),
          by = c('계좌', '종목코드')
        ) %>%
        arrange(행번호) %>%
        self$add_table('pension')
    }

  )
)
