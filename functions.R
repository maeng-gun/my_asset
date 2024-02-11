library(dplyr)
library(R6)
library(ecos)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)
library(rvest)
library(readxl)

import::from(stringr, str_detect, str_extract)

get_config <- function(){
  yaml::read_yaml(file = 'config.yaml', 
                  readLines.warn = F)
}

get_exchange_rate <- function(cur='달러'){
  
  num <- c('달러'= 1, '엔'= 2, '유로'=3, '위안'=4)
  
  (read_html("http://finance.naver.com/marketindex/") |> 
      html_nodes("div.head_info > span.value")
  )[num[cur]] |>
    html_text() |> 
    readr::parse_number()
}

Ecos <- R6Class(
  "Ecos",
  
  public = list(
    table_list = NULL,
    
    #속성 초기화
    initialize = function() {
      #인증키 설정
      get_config()$ecos |> ecos.setKey()
      #(속성) 통계표 목록
      self$table_list <- statTableList() |> tibble()
    },
    
    #(메서드)통계표 검색
    find_stat = function(name=''){
      
      if(is.null(name)||name == '') {
        self$table_list
      }
      else{
        self$table_list |> 
          filter(str_detect(stat_name, name), 
                 srch_yn=='Y')
      }
    },
    
    #(메서드)아이템 검색
    find_items = function(code='전체'){
      
      if(is.null(code)||code == '전체') {
        tibble(item_name='',stat_code='',item_code='',cycle='')
      }
      else {
        statItemList(code) |> 
          tibble() |> 
          select(stat_name, item_name, stat_code, item_code, cycle:data_cnt) |> 
          mutate(stat_name = stringr::str_extract(stat_name,"(?<=\\.\\s).*"))
      }
    },
    
    #(메서드)저장된 아이템 읽기
    read_items = function(){
      readRDS('ecos_items.rds')
    },
    
    #(메서드)아이템 저장
    save_items = function(df, name){
      
      df2 <- df |> mutate(new_name = name)
      
      self$read_items() |> 
        bind_rows(df2) |> 
        distinct() |> 
        arrange(stat_code,item_code) |> 
        saveRDS('ecos_items.rds')
    }
  )
)


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
      data <- c("grant_type" = "client_credentials",
                "appkey" = self$APP_KEY,
                "appsecret" = self$APP_SECRET)
      
      # 기존 발급된 토큰이 있는지 확인
      saved_token <- self$read_token()
      
      if (is.null(saved_token)) {  # 기존 발급 토큰 확인이 안되면 발급 처리
        path <- "oauth2/tokenP"
        res <- self$POST_json(path, data)
        
        if (!is.null(res)) {  # 토큰 정상 발급
          my_token <- res$access_token
          my_expired <- res$access_token_token_expired
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
    hashkey = function(data) {
      path <- "uapi/hashkey"
      headers <- c(appKey = self$APP_KEY, 
                   appSecret = self$APP_SECRET)
      self$POST_json(path, data, headers)$HASH
    },
    
    #메서드(5-1) - GET 명령 ====
    GET_tbl = function(path, data, headers){
      URL <- glue("{self$URL_BASE}/{path}")
      headers2 <- c(self$token_headers, headers)
      
      GET(URL, query = data, add_headers(.headers = headers2)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
    },
    
    # 메서드(5-2) - POST 명령 ====
    POST_json = function(path, data, headers=NULL){
      
      URL <- glue("{self$URL_BASE}/{path}")
      jbody <- toJSON(as.list(data), auto_unbox = T)
      headers2 <- c(self$base_headers, headers)
      res <- POST(URL, body = jbody, encode = "json",
           add_headers(.headers = headers2))

      if (res$status_code == 200) {
        res <- res |> 
          content("text", encoding = 'UTF-8') |> 
          fromJSON()
        return(res)
      } else {
        return(NULL)
      }
    },
    
    #메서드(6) - 자산별 잔고 ====
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
      
      self$GET_tbl(path, data, headers)$output1 |> 
        tibble() |> 
        rename_with(~c('매입금액', '평가금액', '평가손익', 
                       '신용대출', '순자산', '비중')) |> 
        mutate(자산구분 = asset, .before = 1) |> 
        mutate(across(매입금액:비중, as.numeric)) |> 
        filter(비중!=0)
    },
    
    #메서드(7) - 국내주식 잔고 ====
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
      
      self$GET_tbl(path, data, headers)$output1 |> 
        tibble() |> 
        select(pdno, prdt_name, evlu_amt) |>
        rename_with(~c('종목코드', '상품명', '평가금액')) |> 
        mutate(평가금액 = as.numeric(평가금액))
      
    },
    
    #메서드(8) - 해외주식 잔고 ====
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
      
      self$GET_tbl(path, data, headers)$output1 |> 
        tibble() |>
        select(ovrs_pdno, ovrs_item_name, 
               ovrs_stck_evlu_amt) |> 
        rename_with(~c('종목코드', '상품명', '평가금액')) |> 
        mutate(평가금액 = as.numeric(평가금액))
    },
    
    #메서드(9) - 개별종목 현재가 ====
    get_current_price = function(sym_cd) {
      path <- "/uapi/domestic-stock/v1/quotations/inquire-price"
      data <- list(
        FID_COND_MRKT_DIV_CODE = "J",
        FID_INPUT_ISCD = sym_cd
      )
      headers <- c("tr_id" = "FHKST01010100", "custtype" = "P")
      
      self$GET_tbl(path, data, headers)$output
    }
  )
)

MyAssets <- R6Class(
  classname = "MyAssets",
  
  public = list(
    today = NULL, year = NULL,
    assets = NULL, pension = NULL, ex_usd = NULL,
    ex_jpy = NULL, daily_trading = NULL,
    bs_pl_book = NULL, bs_pl_mkt = NULL,
    bl = NULL, my = NULL, allo0 = NULL,
    allo1 = NULL, allo2 = NULL,
    allo3 = NULL, allo4 = NULL,
    allo5 = NULL, ret = NULL,
    
    #속성 초기화
    initialize = function(base_dt=NULL) {
      if (!is.null(base_dt)) {
        self$today <- ymd(base_dt)
      } else {
        self$today <- today()
      }
      self$year <- year(self$today)
      self$assets <- read_excel('trade.xlsx', sheet = '자산정보')
      self$pension <- read_excel('trade.xlsx', sheet = '연금종목정보')
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      self$daily_trading <- self$get_daily_trading()|> 
        mutate(거래일자=as.Date(거래일자))
      self$bs_pl_book <- self$get_bs_pl()
      self$bs_pl_mkt <- self$evaluate_bs_pl()
      self$compute_allocation()
      self$ret <- self$get_class_returns()
      
    },
    
    #(메서드) 연금 일일거래내역 산출==== 
    get_pension_daily = function(){
      days <- seq(make_date(self$year,1,1), make_date(self$year,12,31),by='day')
      nhb <- read_excel('trade.xlsx', sheet = '농협IRP')
      shi <- read_excel('trade.xlsx', sheet = '삼성DC')
      nhi <- read_excel('trade.xlsx', sheet = '엔투저축연금')
    },
    
    #(메서드) 일일거래내역 산출====
    get_daily_trading = function(){
      days <- seq(make_date(self$year,1,1), make_date(self$year,12,31),by='day')
      
      usd1 <- read_excel('trade.xlsx', sheet = '불리오달러')
      usd2 <- read_excel('trade.xlsx', sheet = '한투달러')
      jpy <- read_excel('trade.xlsx', sheet = '한투엔화')
      # krw1 <- read_excel('trade.xlsx', sheet = '나무원화')
      krw2 <- read_excel('trade.xlsx', sheet = '한투원화')
      # krw3 <- read_excel('trade.xlsx', sheet = '한투CMA')
      krw4 <- read_excel('trade.xlsx', sheet = '한투ISA')
      krw5 <- read_excel('trade.xlsx', sheet = '별도원화')
      fiw <- read_excel('trade.xlsx', sheet = '외화자산평가') %>% select(-c('외화입출금'))
      
      as_list <- expand_grid(종목코드 = self$assets$종목코드, 거래일자 = days)
      
      trade_raw <- bind_rows(usd1, usd2, jpy, krw2, krw4, krw5, fiw) %>%
        mutate(across(매입수량:누적,as.numeric)) |> 
        select(-종목명, -상품명)
      
      as_list %>%
        left_join(select(self$assets, 종목코드, 종목명, 통화, 계좌), by = "종목코드") %>%
        left_join(trade_raw, by = c("종목코드", "거래일자")) %>%
        mutate(across(매입수량:누적, ~coalesce(.,0)))%>%
        arrange(종목코드, 거래일자) |> 
        mutate(
          순매입수량 = 매입수량 - 매도수량,
          수익 = 매매수익 + 이자배당액,
          비용 = 매입비용 + 매도비용,
          실현손익 = 수익 - 비용
        ) %>%
        select(종목코드:계좌, 순매입수량, 매입액, 매도원금, 수익, 비용, 실현손익, 현금수입, 입출금, 현금지출)
    },
    
    #(메서드)운용자산 잔액-손익 테이블 생성====
    get_bs_pl = function() {
      
      trade <- self$daily_trading
      
      #(1) 기본 테이블 생성
      
      bs_pl1 <- trade %>%
        select(종목코드, 거래일자, 종목명, 통화, 계좌)
      
      bs_pl2 <- trade |> 
        group_by(종목코드) %>%
        transmute(
          보유수량 = cumsum(순매입수량),
          장부금액 = cumsum(매입액 - 매도원금),
          평잔 = cummean(장부금액)
        ) %>%
        ungroup() |> 
        select(-종목코드)
      
      bs_pl3 <- trade %>%
        group_by(종목코드) %>%
        transmute(
          수익 = cumsum(수익),
          비용 = cumsum(비용),
          실현손익 = cumsum(실현손익)
        )%>%
        ungroup() |> 
        select(-종목코드)
      
      bs_pl <- bind_cols(bs_pl1, bs_pl2, bs_pl3)  
      
      #(2) 계좌별 현금성자산 산출
      
      cash_w <- trade %>%
        filter(통화 == '원화') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출),
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) |> 
        ungroup()
      
      cash_w_b <- cash_w %>%
        mutate(across(-거래일자, cumsum))
      
      cash_w_e <- cash_w_b %>%
        mutate(across(-거래일자, cummean))
      
      
      cash_d <- trade %>%
        filter(통화 == '달러') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) |> 
        ungroup()
      
      cash_d_b <- cash_d %>%
        mutate(across(-거래일자, cumsum))
      
      cash_d_e <- cash_d_b %>%
        mutate(across(-거래일자, cummean))
      
      cash_y <- trade %>%
        filter(통화 == '엔화') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) %>%
        ungroup()
      
      cash_y_b <- cash_y %>%
        mutate(across(-거래일자, cumsum))
      
      cash_y_e <- cash_y_b %>%
        mutate(across(-거래일자, cummean))
      
      bs_pl %>%
        mutate(
          장부금액 = replace(장부금액, 종목명=='나무예수금', cash_w_b$나무),
          장부금액 = replace(장부금액, 종목명=='한투예수금', cash_w_b$한투),
          장부금액 = replace(장부금액, 종목명=='한투CMA예수금', cash_w_b$한투CMA),
          장부금액 = replace(장부금액, 종목명=='한투ISA예수금', cash_w_b$한투ISA),
          평잔 = replace(평잔, 종목명=='나무예수금', cash_w_e$나무),
          평잔 = replace(평잔, 종목명=='한투예수금', cash_w_e$한투),
          평잔 = replace(평잔, 종목명=='한투CMA예수금', cash_w_e$한투CMA),
          평잔 = replace(평잔, 종목명=='한투ISA예수금', cash_w_e$한투ISA),
          장부금액 = replace(장부금액, 종목명=='불리오달러', cash_d_b$불리오),
          장부금액 = replace(장부금액, 종목명=='직접운용달러', cash_d_b$한투),
          평잔 = replace(평잔, 종목명=='불리오달러', cash_d_e$불리오),
          평잔 = replace(평잔, 종목명=='직접운용달러', cash_d_e$한투), 
          장부금액 = replace(장부금액, 종목명=='직접운용엔', cash_y_b$한투),
          평잔 = replace(평잔, 종목명=='직접운용엔', cash_y_e$한투),
          실현수익률 = 실현손익 / 평잔 * 100) |> 
        left_join(select(self$assets, 종목코드, 자산군, 세부자산군, 세부자산군2), 
                  by = '종목코드') %>%
        arrange(종목명, 거래일자) |>
        filter(평잔!=0)
      
    },
    
    #(메서드)평가금액 반영 잔액-손익 테이블 생성====
    evaluate_bs_pl = function() {
      
      if (is.null(self$bl) && is.null(self$my)) {
        self$bl <- AutoInvest$new('boolio')
        self$my <- AutoInvest$new('my')
      }
      
      price <- self$assets |> 
        filter(평가금액!=0) |> 
        select(종목코드, 상품명, 평가금액) |> 
        bind_rows(
          self$my$inquire_balance(),
          self$my$inquire_balance_ovs(),
          self$my$inquire_balance_ovs('JPY'),
          self$bl$inquire_balance_ovs()) |> 
        select(종목코드,평가금액)
      
      p_lotte <- as.integer(self$bl$get_current_price("011170")$stck_prpr)
      q_lotte <- filter(self$bs_pl_book, 
                        종목명=='롯데케미칼', 
                        거래일자 == self$today)$보유수량
      
      bs_pl <- self$bs_pl_book |> 
        filter(거래일자 == self$today) |> 
        left_join(price, by="종목코드") |> 
        mutate(
          평가금액 = replace(평가금액, 종목명 == '롯데케미칼', p_lotte*q_lotte),
          평가금액 = ifelse(is.na(평가금액), 장부금액, 평가금액)) |> 
        left_join(
          (self$assets |> 
             select(종목코드, 기초평가손익)),
          by="종목코드"
        )
      
      dollar <-
        (sum(filter(bs_pl, 통화 == '달러')$평가금액) * self$ex_usd) |> 
        round()
      
      yen <-   
        (sum(filter(bs_pl, 통화 == '엔화')$평가금액) * self$ex_jpy) |> 
        round()
      
      bs_pl |> 
        mutate(
          평가금액 = replace(평가금액, 종목명 == '달러자산', dollar),
          평가금액 = replace(평가금액, 종목명 == '엔화자산', yen),
          평가손익증감 = 평가금액 - 장부금액,
          운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
          평가손익 = 기초평가손익 + 평가손익증감, 
          평가수익률 = 평가손익 / (평가금액-평가손익) * 100
        ) |> 
        arrange(desc(통화), desc(평가금액))
    },
    
    #(메서드)자산군별 수익률 현황====
    get_class_returns = function() {
      
      df <- self$bs_pl_mkt
      krw <- df %>% filter(통화 == '원화')
      usd <- df %>% filter(통화 == '달러')
      jpy <- df %>% filter(통화 == '엔화')
      
      get_class_returns_cur <- function(asset_returns) {
        cur <- asset_returns$통화[1]
        
        class_returns <- asset_returns %>%
          select(-(종목코드:보유수량), 
                 -세부자산군2, -실현수익률, -운용수익률, -평가수익률) %>%
          group_by(자산군, 세부자산군) %>%
          summarise(across(everything(), ~sum(.x, na.rm = TRUE)),.groups='drop') |> 
          mutate(통화 = cur, .before=1)
        
        class_returns |> 
          add_row(통화 = cur, 자산군 = '전체', 세부자산군 = NA, 
                  !!(class_returns |>
                       select(-통화,-자산군,-세부자산군) |> 
                       summarise(across(everything(),sum, na.rm=T)))) |> 
          mutate(실현수익률 = 실현손익 / 평잔 * 100,
                 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
                 평가수익률 = 평가손익 / (평가금액-평가손익) * 100)
      }
      
      krw_ret <- get_class_returns_cur(krw)
      usd_ret <- get_class_returns_cur(usd)
      jpy_ret <- get_class_returns_cur(jpy)
      
      bind_rows(krw_ret, usd_ret, jpy_ret)
    },
    
    #(메서드)자산군별 배분 현황====
    compute_allocation = function() {
      
      df <- self$bs_pl_mkt
      usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)                    
      jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)                    
      
      df <- df |> 
        filter(자산군 != '외화자산') %>%
        mutate(평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
               평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval)) %>%
        group_by(자산군, 세부자산군, 통화) %>%
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = 평가금액 / sum(평가금액) * 100)
      
      self$allo0 <- df %>%
        group_by(자산군) %>%
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(자산군='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      self$allo1 <- df %>%
        add_row(자산군='합계', 평가금액=sum(df$평가금액), 투자비중=100) %>%
        group_by(자산군) %>%
        mutate(자산별비중 = 평가금액 / sum(평가금액) * 100)
      
      self$allo2 <- df %>%
        group_by(통화) %>%
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(통화='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      
      self$allo3 <- df %>%
        group_by(통화, 자산군) %>%
        summarize(평가금액 = sum(평가금액), 
                  투자비중 = sum(투자비중), .groups = 'drop') %>%
        add_row(통화='합계', 평가금액 = sum(df$평가금액), 투자비중=100) %>%
        group_by(통화) |> 
        mutate(통화별비중 = 평가금액 / sum(평가금액) * 100)
      
      self$allo4 <- self$bs_pl_mkt %>%
        filter(계좌 == '불리오') %>%
        group_by(세부자산군, 세부자산군2) %>%
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = 평가금액 / sum(평가금액) * 100) %>%
        add_row(세부자산군='합계', 평가금액 = sum(df$평가금액), 투자비중=100)
      
      self$allo5 <- self$allo4 %>%
        group_by(세부자산군) %>%
        summarize(평가금액 = sum(평가금액),
                  투자비중 = sum(투자비중)) |> 
        add_row(세부자산군='합계', 평가금액 = sum(df$평가금액), 투자비중=100)
    }
  )
)
