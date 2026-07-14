# =============================================================================
# AutoInvest R6 클래스 — 한국투자증권 OpenAPI 연동 계층
# =============================================================================
# MyData를 상속하며, 토큰 관리·국내/해외 주식 잔고 조회·개별 현재가 조회 등
# 한국투자증권 REST API와의 통신을 전담
# pool 객체는 외부에서 주입받음
# =============================================================================

AutoInvest <- R6Class(

  classname = 'AutoInvest',
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

      self$APP_KEY    <- cfg[[paste0(account, '_app')]]
      self$APP_SECRET <- cfg[[paste0(account, '_sec')]]
      self$ACCT       <- cfg[[paste0(account, '_acct')]]
      self$URL_BASE   <- cfg$prod
      self$MY_AGENT   <- cfg$agent
      self$base_headers <- c(
        "Content-Type" = "application/json",
        "Accept"       = "text/plain",
        "charset"      = "UTF-8",
        'User-Agent'   = self$MY_AGENT
      )
      self$token_headers <- c(
        self$base_headers,
        "authorization" = paste0("Bearer ", self$auth()),
        "appkey"        = self$APP_KEY,
        "appsecret"     = self$APP_SECRET
      )
    },

    ## 메서드(1) — 토큰 저장하기 ====
    save_token = function(my_token, my_expired) {
      valid_date <-
        as.POSIXct(my_expired, format = '%Y-%m-%d %H:%M:%S', tz = 'Asia/Seoul')

      dbWriteTable(self$con, self$token_tmp,
                   tibble(token = my_token,
                          valid_date = format(valid_date, '%Y-%m-%d %H:%M:%S')),
                   overwrite = TRUE)
    },

    ## 메서드(2) — 토큰 불러오기 ====
    read_token = function() {
      tryCatch({
        tkg_tmp <- self$read(self$token_tmp)

        # 토큰 만료 일,시간
        exp_dt <- as.POSIXct(tkg_tmp$valid_date,
                             format = '%Y-%m-%d %H:%M:%S', tz = 'Asia/Seoul')
        # 현재일자,시간
        now_dt <- lubridate::now(tzone = "Asia/Seoul")

        # 저장된 토큰 만료일자 체크 (만료일시 > 현재일시 인경우 보관 토큰 리턴)
        if (exp_dt > now_dt) {
          return(tkg_tmp$token)
        } else {
          cat('Need new token: ', tkg_tmp$valid_date, '\n')
          return(NULL)
        }
      }, error = function(e) {
        return(NULL)
      })
    },

    ## 메서드(3) — 인증하기 ====
    auth = function() {
      data <- c("grant_type" = "client_credentials",
                "appkey"     = self$APP_KEY,
                "appsecret"  = self$APP_SECRET)

      # 기존 발급된 토큰이 있는지 확인
      saved_token <- self$read_token()

      if (is.null(saved_token)) {
        path <- "oauth2/tokenP"
        res  <- self$POST_json(path, data)

        if (!is.null(res)) {
          my_token   <- res$access_token
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
      headers <- c(appKey = self$APP_KEY,
                   appSecret = self$APP_SECRET)
      self$POST_json(path, data, headers)$HASH
    },

    ## 메서드(5-1) — GET 명령 ====
    GET_tbl = function(path, data, headers) {
      URL <- glue("{self$URL_BASE}/{path}")
      headers2 <- c(self$token_headers, headers)

      GET(URL, query = data, add_headers(.headers = headers2)) %>%
        content("text", encoding = 'UTF-8') %>%
        fromJSON()
    },

    ## 메서드(5-2) — POST 명령 ====
    POST_json = function(path, data, headers = NULL) {

      URL <- glue("{self$URL_BASE}/{path}")
      jbody <- toJSON(as.list(data), auto_unbox = TRUE)
      headers2 <- c(self$base_headers, headers)
      res <- POST(URL, body = jbody, encode = "json",
                  add_headers(.headers = headers2))

      if (res$status_code == 200) {
        res <- res %>%
          content("text", encoding = 'UTF-8') %>%
          fromJSON()
        return(res)
      } else {
        return(NULL)
      }
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
      headers <- c("tr_id" = "CTRP6548R",
                    "custtype" = "P")
      asset <- c("주식", "펀드_MMW", "채권", "ELS_DLS", "WRAP",
                 "신탁_퇴직연금_외화신탁", "RP_발행어음", "해외주식",
                 "해외채권", "금현물", "CD_CP", "단기사채",
                 "타사상품", "외화단기사채", "외화ELS_DLS", "외화",
                 "예수금+CMA", "청약자예수", "합계")

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        rename_with(~c('매입금액', '평가금액', '평가손익',
                       '신용대출', '순자산', '비중')) %>%
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

      headers <- c("tr_id" = "TTTC8434R",
                    "custtype" = "P")

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        select(pdno, prdt_name, evlu_amt) %>%
        rename_with(~c('종목코드', '상품명', '평가금액')) %>%
        mutate(평가금액 = as.numeric(평가금액))
    },

    ## 메서드(8) — 해외주식 잔고 ====
    inquire_balance_ovs = function(cur = 'USD') {
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
      headers <- c("tr_id" = "TTTS3012R", "custtype" = "P")

      self$GET_tbl(path, data, headers)$output1 %>%
        tibble() %>%
        select(ovrs_pdno, ovrs_item_name,
               ovrs_stck_evlu_amt) %>%
        rename_with(~c('종목코드', '상품명', '평가금액')) %>%
        mutate(평가금액 = as.numeric(평가금액))
    },

    ## 메서드(9) — 개별종목 현재가 ====
    get_current_price = function(sym_cd) {
      path <- "/uapi/domestic-stock/v1/quotations/inquire-price"
      headers <- c("tr_id" = "FHKST01010100", "custtype" = "P")

      fetch_single <- function(code) {
        data <- list(
          FID_COND_MRKT_DIV_CODE = "J",
          FID_INPUT_ISCD = code
        )

        # API 호출
        res <- self$GET_tbl(path, data, headers)

        # 정상적으로 응답이 오면 현재가(stck_prpr)를 숫자로 변환하여 반환
        tryCatch({
          if (!is.null(res$output$stck_prpr)) {
            as.numeric(res$output$stck_prpr)
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)
      }

      result_prices <- purrr::map_dbl(sym_cd, function(code) {
        # API 제한(초당 건수)을 피하기 위한 딜레이
        Sys.sleep(0.2)
        fetch_single(code)
      })

      return(result_prices)
    }
  )
)
