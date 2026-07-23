# =============================================================================
# AutoInvest R6 클래스 — 한국투자증권 OpenAPI 연동 및 크롤링(환율/금/펀드) 계층
# =============================================================================
# MyData를 상속하며, 토큰 관리·국내/해외 주식 잔고 조회·개별 현재가 조회 등
# 한국투자증권 REST API 및 범용 가격 크롤링 통신을 전담
# httr2 기반 비동기 병렬 처리 적용
# pool 객체는 외부에서 주입받음
# =============================================================================

library(httr2)
library(rvest)

AutoInvest <- R6Class(
  classname = "AutoInvest",
  inherit = MyData,
  public = list(
    ## 속성 선언 ====
    token_tmp = NULL, APP_KEY = NULL, APP_SECRET = NULL, ACCT = NULL,
    URL_BASE = NULL, MY_AGENT = NULL, base_headers = NULL,
    token_headers = NULL,

    ## 속성 초기화 ====
    initialize = function(pool, account = "my") {
      super$initialize(pool)
      cfg <- split(self$config$value, self$config$token)

      self$token_tmp <- paste0("KIS", account)

      self$APP_KEY <- cfg[[paste0(account, "_app")]]
      self$APP_SECRET <- cfg[[paste0(account, "_sec")]]
      self$ACCT <- cfg[[paste0(account, "_acct")]]
      self$URL_BASE <- cfg$prod
      self$MY_AGENT <- cfg$agent
      self$base_headers <- list(
        "Content-Type" = "application/json",
        "Accept"       = "text/plain",
        "charset"      = "UTF-8",
        "User-Agent"   = self$MY_AGENT
      )
      self$token_headers <- c(
        self$base_headers,
        list(
          "authorization" = paste0("Bearer ", self$auth()),
          "appkey"        = self$APP_KEY,
          "appsecret"     = self$APP_SECRET
        )
      )
    },

    ## 메서드(1) — 토큰 저장하기 ====
    save_token = function(my_token, my_expired) {
      valid_date <-
        as.POSIXct(my_expired, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Seoul")

      dbWriteTable(self$con, self$token_tmp,
        tibble(
          token = my_token,
          valid_date = format(valid_date, "%Y-%m-%d %H:%M:%S")
        ),
        overwrite = TRUE
      )
    },

    ## 메서드(2) — 토큰 불러오기 ====
    read_token = function() {
      tryCatch(
        {
          tkg_tmp <- self$read(self$token_tmp)

          # 토큰 만료 일,시간
          exp_dt <- as.POSIXct(tkg_tmp$valid_date,
            format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Seoul"
          )
          # 현재일자,시간
          now_dt <- lubridate::now(tzone = "Asia/Seoul")

          # 저장된 토큰 만료일자 체크 (만료일시 > 현재일시 인경우 보관 토큰 리턴)
          if (exp_dt > now_dt) {
            return(tkg_tmp$token)
          } else {
            cat("Need new token: ", tkg_tmp$valid_date, "\n")
            return(NULL)
          }
        },
        error = function(e) {
          return(NULL)
        }
      )
    },

    ## 메서드(3) — 인증하기 ====
    auth = function() {
      data <- list(
        "grant_type" = "client_credentials",
        "appkey" = self$APP_KEY,
        "appsecret" = self$APP_SECRET
      )

      # 기존 발급된 토큰이 있는지 확인
      saved_token <- self$read_token()

      if (is.null(saved_token)) {
        path <- "oauth2/tokenP"
        res <- self$POST_json(path, data)

        if (!is.null(res)) {
          my_token <- res$access_token
          my_expired <- res$access_token_token_expired
          self$save_token(my_token, my_expired)
        } else {
          cat("Get Authentification token fail!\nYou have to restart your app!!!\n")
          return(NULL)
        }
      } else {
        my_token <- saved_token
      }

      return(my_token)
    },

    ## 메서드(4) — 해시키 얻기 ====
    hashkey = function(data) {
      path <- "uapi/hashkey"
      headers <- list(
        appKey = self$APP_KEY,
        appSecret = self$APP_SECRET
      )
      self$POST_json(path, data, headers)$HASH
    },

    ## 메서드(5-1) — GET 명령 (httr2 기반) ====
    GET_tbl = function(path, data, headers) {
      URL <- glue("{self$URL_BASE}/{path}")

      token_hdrs <- c(
        self$base_headers,
        list(
          "authorization" = paste0("Bearer ", self$auth()),
          "appkey"        = self$APP_KEY,
          "appsecret"     = self$APP_SECRET
        )
      )

      req <- request(URL) %>%
        req_headers(!!!token_hdrs) %>%
        req_headers(!!!headers) %>%
        req_url_query(!!!data) %>%
        req_retry(max_tries = 3, backoff = ~1)

      resp <- req_perform(req)
      resp %>%
        resp_body_string(encoding = "UTF-8") %>%
        fromJSON()
    },

    ## 메서드(5-2) — POST 명령 (httr2 기반) ====
    POST_json = function(path, data, headers = NULL) {
      URL <- glue("{self$URL_BASE}/{path}")

      req <- request(URL) %>%
        req_headers(!!!self$base_headers)

      if (!is.null(headers)) {
        req <- req %>% req_headers(!!!headers)
      }

      req <- req %>%
        req_body_json(data) %>%
        req_retry(max_tries = 3)

      tryCatch(
        {
          resp <- req_perform(req)
          if (resp_status(resp) == 200) {
            resp %>%
              resp_body_string(encoding = "UTF-8") %>%
              fromJSON()
          } else {
            return(NULL)
          }
        },
        error = function(e) {
          return(NULL)
        }
      )
    },

    ## 메서드(6) — 자산별 잔고 ====
    inquire_account_balance = function() {
      path <- "uapi/domestic-stock/v1/trading/inquire-account-balance"
      data <- list(
        CANO = self$ACCT,
        ACNT_PRDT_CD = "01",
        INQR_DVSN_1 = "",
        BSPR_BF_DT_APLY_YN = ""
      )
      headers <- list(
        "tr_id" = "CTRP6548R",
        "custtype" = "P"
      )
      asset <- c(
        "주식", "펀드_MMW", "채권", "ELS_DLS", "WRAP",
        "신탁_퇴직연금_외화신탁", "RP_발행어음", "해외주식",
        "해외채권", "금현물", "CD_CP", "단기사채",
        "타사상품", "외화단기사채", "외화ELS_DLS", "외화",
        "예수금+CMA", "청약자예수", "합계"
      )

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        rename_with(~ c(
          "매입금액", "평가금액", "평가손익",
          "신용대출", "순자산", "비중"
        )) %>%
        mutate(자산구분 = asset, .before = 1) %>%
        mutate(across(매입금액:비중, as.numeric)) %>%
        filter(비중 != 0)
    },

    ## 메서드(7) — 국내주식 잔고 ====
    inquire_balance = function() {
      path <- "/uapi/domestic-stock/v1/trading/inquire-balance"

      data <- list(
        CANO = self$ACCT,
        ACNT_PRDT_CD = "01",
        AFHR_FLPR_YN = "N",
        OFL_YN = "N",
        INQR_DVSN = "01",
        UNPR_DVSN = "01",
        FUND_STTL_ICLD_YN = "N",
        FNCG_AMT_AUTO_RDPT_YN = "N",
        PRCS_DVSN = "01",
        CTX_AREA_FK100 = "",
        CTX_AREA_NK100 = ""
      )

      headers <- list(
        "tr_id" = "TTTC8434R",
        "custtype" = "P"
      )

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        select(pdno, prdt_name, evlu_amt) %>%
        rename_with(~ c("종목코드", "상품명", "평가금액")) %>%
        mutate(평가금액 = as.numeric(평가금액))
    },

    ## 메서드(8) — 해외주식 잔고 ====
    inquire_balance_ovs = function(cur = "USD") {
      path <- "uapi/overseas-stock/v1/trading/inquire-balance"
      exc <- list(USD = "NASD", JPY = "TKSE")
      data <- list(
        CANO = self$ACCT,
        ACNT_PRDT_CD = "01",
        OVRS_EXCG_CD = exc[[cur]],
        TR_CRCY_CD = cur,
        CTX_AREA_FK200 = "",
        CTX_AREA_NK200 = ""
      )
      headers <- list("tr_id" = "TTTS3012R", "custtype" = "P")

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        select(
          ovrs_pdno, ovrs_item_name,
          ovrs_stck_evlu_amt
        ) %>%
        rename_with(~ c("종목코드", "상품명", "평가금액")) %>%
        mutate(평가금액 = as.numeric(평가금액))
    },

    ## 메서드(9) — 개별종목 현재가 (순차 처리 + 개별 재시도 로직 추가) ====
    get_current_price = function(sym_cd) {
      if (length(sym_cd) == 0) {
        return(numeric(0))
      }

      path <- "/uapi/domestic-stock/v1/quotations/inquire-price"
      URL <- glue("{self$URL_BASE}{path}")
      headers <- list("tr_id" = "FHKST01010100", "custtype" = "P")

      result_prices <- numeric(length(sym_cd))

      for (i in seq_along(sym_cd)) {
        code <- sym_cd[i]
        data <- list(
          FID_COND_MRKT_DIV_CODE = "J",
          FID_INPUT_ISCD = code
        )

        req <- request(URL) %>%
          req_headers(!!!self$token_headers) %>%
          req_headers(!!!headers) %>%
          req_url_query(!!!data)

        # 응답 지연이나 HTTP 200으로 반환되는 한투 API 내부 에러(트래픽 초과 메시지 등) 방어
        price_val <- NA_real_
        for (attempt in 1:3) {
          parsed <- tryCatch(
            {
              resp <- req_perform(req)
              if (resp_status(resp) == 200) {
                resp %>%
                  resp_body_string(encoding = "UTF-8") %>%
                  fromJSON()
              } else {
                NULL
              }
            },
            error = function(e) NULL
          )

          if (!is.null(parsed) && !is.null(parsed$output$stck_prpr)) {
            price_val <- as.numeric(parsed$output$stck_prpr)
            break # 정상 응답 시 재시도 루프 탈출
          }

          # 값 누락 발생 시 0.5초 대기 후 재시도 (서버가 진정할 시간을 줌)
          Sys.sleep(0.1)
        }

        result_prices[i] <- price_val

        # 다음 종목 조회 전 넉넉하게 0.07초 대기
        Sys.sleep(0.05)
      }

      return(result_prices)
    },

    ## 메서드(10) — 환율 수집 ====
    get_exchange_rate = function() {
      get_rate <- function(cur = "달러") {
        num <- c("달러" = 1, "엔" = 2, "유로" = 3, "위안" = 4)
        suppressWarnings({
          (read_html("http://finance.naver.com/marketindex/") %>%
            html_nodes("div.head_info > span.value")
          )[num[cur]] %>%
            html_text() %>%
            readr::parse_number()
        })
      }

      tryCatch(
        {
          usd <- get_rate("달러")
          jpy <- get_rate("엔") / 100
          list(USD = usd, JPY = jpy)
        },
        error = function(e) {
          list(USD = NA_real_, JPY = NA_real_)
        }
      )
    },

    ## 메서드(11) — 금 시세 수집 (httr2) ====
    get_gold_price = function() {
      url <- "https://api.stock.naver.com/marketindex/metals/M04020000"
      tryCatch(
        {
          req <- request(url) %>% req_retry(max_tries = 3)
          resp <- req_perform(req)
          json_data <- resp %>% resp_body_json()
          price <- readr::parse_number(json_data$closePrice)
          tibble(종목코드 = "04020000", 종가 = price)
        },
        error = function(e) {
          message("금 시세 조회 실패")
          tibble()
        }
      )
    },

    ## 메서드(12) — 펀드 기준가 수집 (httr2 비동기 병렬 - 청크 단위) ====
    get_fund_price = function(codes) {
      if (length(codes) == 0) {
        return(tibble())
      }

      chunks <- split(codes, ceiling(seq_along(codes) / 5))
      all_prices <- numeric(length(codes))
      idx <- 1

      for (chunk in chunks) {
        reqs <- purrr::map(chunk, function(code) {
          url <- paste0("https://www.funddoctor.co.kr/afn/fund/fprofile2.jsp?fund_cd=", code)
          request(url) %>% req_retry(max_tries = 3)
        })

        resps <- req_perform_parallel(reqs, on_error = "continue")

        chunk_prices <- purrr::map_dbl(resps, function(resp) {
          if (!inherits(resp, "httr2_response") || resp_status(resp) != 200) {
            return(NA_real_)
          }

          val <- tryCatch(
            {
              resp %>%
                resp_body_html() %>%
                html_element(xpath = "/html/body/div[1]/div/div[3]/div[2]/div[1]/div[1]") %>%
                html_text() %>%
                stringr::str_remove(",") %>%
                as.numeric()
            },
            error = function(e) NA_real_
          )
          return(val)
        })

        all_prices[idx:(idx + length(chunk) - 1)] <- chunk_prices
        idx <- idx + length(chunk)

        Sys.sleep(0.3)
      }

      tibble(
        종목코드 = codes,
        종가 = all_prices / 1000
      )
    }
  )
)
