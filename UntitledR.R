library(dplyr)
library(R6)
library(glue)
library(httr)
library(jsonlite)

get_config <- function(){
  yaml::read_yaml(file = 'config.yaml', 
                  readLines.warn = F)
}

jbody <- function(body){
  toJSON(as.list(body), auto_unbox = T)
}

#AutoInvest 클래스 정의====
AutoInvest <- R6Class(
  
  classname = 'AutoInvest',
  public=list(
    
    # 속성 선언 ====
    token_tmp=NULL, APP_KEY=NULL, APP_SECRET=NULL, ACCT=NULL, 
    URL_BASE=NULL, MY_AGENT=NULL, base_headers=NULL,
    token_headers=NULL,
    
    # 속성 초기화 ====
    initialize = function(account="my"){
      cfg <- get_config()
      self$token_tmp <- paste0("KIS", account)
      
      if (!file.exists(self$token_tmp)) {
        file.create(self$token_tmp)
      }
      
      self$APP_KEY <- cfg[[paste0(account, '_app')]]
      self$APP_SECRET <- cfg[[paste0(account, '_sec')]]
      self$ACCT <- cfg[[paste0(account, '_acct')]]
      self$URL_BASE <- cfg$prod
      self$MY_AGENT <- cfg$agent
      self$base_headers <- c(
        "Content-Type" = "application/json",
        "Accept" = "text/plain",
        "charset" = "UTF-8",
        'User-Agent' = self$MY_AGENT
      )
      self$token_headers <- c(
        self$base_headers,
        "authorization" = paste0("Bearer ", self$auth()),
        "appkey" = self$APP_KEY,
        "appsecret" = self$APP_SECRET
      )
    },
    
    #메서드(1) - 토큰 저장하기 ====
    save_token = function(my_token, my_expired) {
      valid_date <- 
        as.POSIXct(my_expired, format='%Y-%m-%d %H:%M:%S', tz='UTC')
      writeLines(c(paste('token:', my_token),
                   paste('valid-date:', 
                         format(valid_date, '%Y-%m-%d %H:%M:%S'))),
                 self$token_tmp)
    },
    
    #메서드(2) - 토큰 불러오기 ====
    read_token = function() {
      tryCatch({
        # 토큰이 저장된 파일 읽기
        tkg_tmp <- yaml::read_yaml(self$token_tmp, fileEncoding = 'UTF-8')
        
        # 토큰 만료 일,시간
        exp_dt <- as.POSIXct(tkg_tmp$`valid-date`)
        # 현재일자,시간
        now_dt <- Sys.time()
        
        # 저장된 토큰 만료일자 체크 (만료일시 > 현재일시 인경우 보관 토큰 리턴)
        if (exp_dt > now_dt) {
          return(tkg_tmp$token)
        } else {
          cat('Need new token: ', tkg_tmp$`valid-date`, '\n')
          return(NULL)
        }
      }, error = function(e) {
        return(NULL)
      })
    },
    
    #메서드(3) - 인증하기 ====
    auth = function() {
      body <- c("grant_type" = "client_credentials",
                "appkey" = self$APP_KEY,
                "appsecret" = self$APP_SECRET)
      
      # 기존 발급된 토큰이 있는지 확인
      saved_token <- self$read_token()
      
      if (is.null(saved_token)) {  # 기존 발급 토큰 확인이 안되면 발급 처리
        path <- "oauth2/tokenP"
        URL <- glue("{self$URL_BASE}/{path}")
        res <- POST(URL, body = jbody(body), encode = "json", 
                    add_headers(.headers = self$base_headers))
        
        if (res$status_code == 200) {  # 토큰 정상 발급
          my_token <- content(res)$access_token
          my_expired <- content(res)$access_token_token_expired
          self$save_token(my_token, my_expired)  # 새로 발급 받은 토큰 저장
        } else {
          cat("Get Authentification token fail!\nYou have to restart your app!!!\n")
          return(NULL)
        }
      } else {
        my_token <- saved_token  # 기존 발급 토큰 확인되어 기존 토큰 사용
      }
      
      return(my_token)
    },
    
    #메서드(4) - 해시키 얻기 ====
    hashkey = function(datas) {
      path <- "uapi/hashkey"
      URL <- glue("{self$URL_BASE}/{path}")
      headers <- c(self$base_headers, 
                   appKey = self$APP_KEY, 
                   appSecret = self$APP_SECRET)
      res <- POST(URL, body = jbody(datas), encode = "json",
                  add_headers(.headers = headers))
      
      content(res)$HASH
    },
    
    #메서드(5) - 자산별 잔고 ====
    inquire_account_balance = function() {
      path <- "uapi/domestic-stock/v1/trading/inquire-account-balance"
      URL <- glue("{self$URL_BASE}/{path}")
      
      data <- list(
        CANO = self$ACCT,
        ACNT_PRDT_CD = "01",
        INQR_DVSN_1 = "",
        BSPR_BF_DT_APLY_YN = ""
      )
      
      headers <- c(self$token_headers, "tr_id" = "CTRP6548R",
                   "custtype" = "P")
      res <- 
        GET(URL, query = data, add_headers(.headers = headers)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
      
      asset <- c("주식", "펀드_MMW", "채권", "ELS_DLS", "WRAP",
                 "신탁_퇴직연금_외화신탁", "RP_발행어음", "해외주식",
                 "해외채권", "금현물", "CD_CP", "단기사채",
                 "타사상품", "외화단기사채", "외화ELS_DLS", "외화",
                 "예수금+CMA", "청약자예수", "합계")
      
      res$output1 |> 
        tibble() |> 
        rename_with(~c('매입금액', '평가금액', '평가손익', 
                       '신용대출', '순자산', '비중')) |> 
        mutate(자산구분 = asset, .before = 1) |> 
        mutate(across(매입금액:비중, as.numeric)) |> 
        filter(비중!=0)
    },
    
    #메서드(6) - 국내주식 잔고 ====
    inquire_balance = function() {
      path <- "/uapi/domestic-stock/v1/trading/inquire-balance"
      URL <- glue("{self$URL_BASE}/{path}")
      
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
      
      headers <- c(self$token_headers, 
                   "tr_id" = "TTTC8434R", 
                   "custtype" = "P")
      
      res <- GET(URL, query = data,
                 add_headers(.headers = headers)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
      
      res$output1 |> 
        tibble() |> 
        select(pdno, prdt_name, evlu_amt) |>
        rename_with(~c('종목코드', '상품명', '평가금액'))

    },
    
    #메서드(7) - 해외주식 잔고 ====
    inquire_balance_ovs = function(cur = 'USD') {
      path <- "uapi/overseas-stock/v1/trading/inquire-balance"
      URL <- glue("{self$URL_BASE}/{path}")
      
      exc <- list(USD = "NASD", JPY = "TKSE")
      data <- list(
        CANO = self$ACCT,
        ACNT_PRDT_CD = "01",
        OVRS_EXCG_CD = exc[[cur]],
        TR_CRCY_CD = cur,
        CTX_AREA_FK200 = "",
        CTX_AREA_NK200 = ""
      )
      
      headers <- c(self$token_headers, "tr_id" = "TTTS3012R", "custtype" = "P")
      
      res <- GET(URL, query = data,
                 add_headers(.headers = headers)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
      
      res$output1 |> 
        tibble() |> 
        select(ovrs_pdno, ovrs_item_name, 
               ovrs_stck_evlu_amt) |> 
        rename_with(~c('종목코드', '상품명', '평가금액'))
    },
    
    #메서드(8) - 개별종목 현재가 ====
    get_current_price = function(sym_cd) {
      path <- "/uapi/domestic-stock/v1/quotations/inquire-price"
      URL <- glue("{self$URL_BASE}/{path}")
      
      data <- list(
        FID_COND_MRKT_DIV_CODE = "J",
        FID_INPUT_ISCD = sym_cd
      )
      
      headers <- c(self$token_headers, 
                   "tr_id" = "FHKST01010100", 
                   "custtype" = "P")
      
      res <- GET(URL, query = data,
                 add_headers(.headers = headers)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
      
      res$output
    }
  )
)

ai_my <- AutoInvest$new('my')
ai_bool <- AutoInvest$new('boolio')
