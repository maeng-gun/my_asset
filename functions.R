library(dplyr)
library(purrr)
library(R6)
library(ecos)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)
library(rvest)
library(readxl)
library(RSQLite)
library(dbx)
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




#[클래스] Ecos====
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


#[클래스] AutoInvest====
AutoInvest <- R6Class(
  
  classname = 'AutoInvest',
  public=list(
    
    ## 속성 선언 ====
    token_tmp=NULL, APP_KEY=NULL, APP_SECRET=NULL, ACCT=NULL, 
    URL_BASE=NULL, MY_AGENT=NULL, base_headers=NULL,
    token_headers=NULL,
    
    ## 속성 초기화 ====
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
    
    ##메서드(1) - 토큰 저장하기 ====
    
    save_token = function(my_token, my_expired) {
      valid_date <- 
        as.POSIXct(my_expired, format='%Y-%m-%d %H:%M:%S', tz='UTC')
      writeLines(c(paste('token:', my_token),
                   paste('valid-date:', 
                         format(valid_date, '%Y-%m-%d %H:%M:%S'))),
                 self$token_tmp)
    },
    
    ##메서드(2) - 토큰 불러오기 ====
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
    
    ##메서드(3) - 인증하기 ====
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
    
    ##메서드(4) - 해시키 얻기 ====
    hashkey = function(data) {
      path <- "uapi/hashkey"
      headers <- c(appKey = self$APP_KEY, 
                   appSecret = self$APP_SECRET)
      self$POST_json(path, data, headers)$HASH
    },
    
    ##메서드(5-1) - GET 명령 ====
    GET_tbl = function(path, data, headers){
      URL <- glue("{self$URL_BASE}/{path}")
      headers2 <- c(self$token_headers, headers)
      
      GET(URL, query = data, add_headers(.headers = headers2)) |> 
        content("text", encoding = 'UTF-8') |> 
        fromJSON()
    },
    
    ## 메서드(5-2) - POST 명령 ====
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
    
    ##메서드(6) - 자산별 잔고 ====
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
    
    ##메서드(7) - 국내주식 잔고 ====
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
    
    ##메서드(8) - 해외주식 잔고 ====
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
    
    ##메서드(9) - 개별종목 현재가 ====
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

#[클래스] MyAssets====
MyAssets <- R6Class(
  classname = "MyAssets",
  
  public = list(
    md = NULL,
    today = NULL, year = NULL, days = NULL,
    assets = NULL, pension = NULL, ex_usd = NULL, ex_jpy = NULL, 
    assets_daily = NULL, pension_daily = NULL,
    bs_pl_book_a = NULL, bs_pl_book_p = NULL, 
    bs_pl_mkt_a = NULL, bs_pl_mkt_p = NULL,
    bl = NULL, my = NULL, 
    ret_a = NULL, ret_p = NULL, ret_a2 = NULL, ret_p2 = NULL, 
    allo0 = NULL, allo1 = NULL, allo2 = NULL,
    allo3 = NULL, allo4 = NULL, allo5 = NULL, 
    allo6 = NULL, allo7 = NULL, allo8 = NULL, 
    allo9 = NULL,
    
    ## 1. 속성 초기화====
    initialize = function(base_dt=NULL) {
      if (!is.null(base_dt)) {
        self$today <- ymd(base_dt)
      } else {
        self$today <- today()
      }
      self$md <- MyData$new('mydata.sqlite')
      self$year <- year(self$today)
      self$days <- seq(make_date(self$year,1,1), 
                       make_date(self$year,12,31),by='day')
      self$assets <- self$md$read('assets')
      self$pension <- self$md$read('pension')
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      self$assets_daily <- self$get_daily_trading(
        self$assets, self$md$read('assets_daily')
      )
      self$pension_daily <- self$get_daily_trading(
        self$pension, self$md$read('pension_daily')
      )
      self$bs_pl_book_a <- self$get_bs_pl('assets')
      self$bs_pl_book_p <- self$get_bs_pl('pension')
    },
    
    ## 2.(메서드) 거래내역 기록 테이블====
    get_trading_record = function(table, acct, cur){
      
      if(table=="투자자산"){table <- 'assets'}
      else {table <- 'pension'}
      
      df1 <- self$md$read(table)
      df2 <- self$md$read(paste0(table,'_daily'))
      
      df2 |> left_join(
        (df1 |> transmute(계좌, 통화, 종목코드, 종목명)), 
        by = '종목코드') |> 
        filter(계좌==acct, 통화==cur) |> 
        mutate(매입비용 = 현금지출-매입액,
               매매수익 = 매도액 - 매도원금,
               매도비용 = 매도액 + 이자배당액 - 현금수입,
               순수익 = 매매수익 + 이자배당액 - 매도비용 - 매입비용,
               순현금수입 = 입출금 + 현금수입 - 현금지출,
               잔액 = cumsum(순현금수입)) |> 
        select(행번호, 계좌, 통화, 거래일자, 종목명, 
               매입수량:현금지출, 매입비용, 매도수량, 매도원금, 
               매도액, 매매수익, 이자배당액, 현금수입, 매도비용, 
               순수익, 입출금, 잔액)
    },
    
    ## 3.(메서드) 계좌거래 내역 전처리 ====
    get_daily_trading = function(ast, trade){
      
      expand_grid(종목코드 = ast$종목코드, 거래일자 = self$days) |> 
        left_join(select(ast, 종목코드, 종목명, 통화, 계좌), 
                  by = "종목코드") |> 
        left_join(trade, by = c("종목코드", "거래일자")) |> 
        mutate(across(매입수량:입출금, ~if_else(is.na(.x),0,.x))) |> 
        arrange(종목코드, 거래일자) |> 
        mutate(
          순매입수량 = 매입수량 - 매도수량,
          수익 =매도액 - 매도원금 + 이자배당액,
          비용 = 현금지출 - 매입액 + 매도액 + 이자배당액 - 현금수입,
          실현손익 = 수익 - 비용
        ) |> 
        select(종목코드:계좌, 순매입수량, 매입액, 매도원금, 
               수익, 비용, 실현손익, 현금수입, 입출금, 현금지출)
    },
    
    
    ## 4.(메서드)운용자산 잔액-손익 테이블 생성====
    get_bs_pl = function(mode = 'assets') {
      
      ###(1) 기본 테이블 생성====
      
      if(mode == 'assets') {
        trade <- self$assets_daily
        codes <- self$assets
      }
      else {
        trade <- self$pension_daily
        codes <- self$pension
      }
      
      bs_pl1 <- trade |>
        select(종목코드, 거래일자, 종목명, 통화, 계좌)
      
      bs_pl2 <- trade |> 
        group_by(종목코드) |>
        transmute(
          보유수량 = cumsum(순매입수량),
          장부금액 = cumsum(매입액 - 매도원금),
          평잔 = cummean(장부금액)
        ) |>
        ungroup() |> 
        select(-종목코드)
      
      bs_pl3 <- trade |>
        group_by(종목코드) |>
        transmute(
          수익 = cumsum(수익),
          비용 = cumsum(비용),
          실현손익 = cumsum(실현손익)
        )|>
        ungroup() |> 
        select(-종목코드)
      
      bs_pl <- bind_cols(bs_pl1, bs_pl2, bs_pl3) |> 
        left_join(select(codes, 종목코드, 자산군, 세부자산군, 세부자산군2), 
                  by = '종목코드') |>
        arrange(종목명, 거래일자)
      
      
      ###(2) 예수금 & 평잔 처리====
      if(mode=='assets'){
        cash_w <- trade |>
          filter(통화 == '원화') |>
          group_by(거래일자, 계좌) |>
          summarise(현금 = sum(현금수입 + 입출금 - 현금지출),
                    .groups = 'keep') |>
          pivot_wider(names_from = 계좌, values_from = 현금) |> 
          ungroup()
        
        cash_w_b <- cash_w |>
          mutate(across(-거래일자, cumsum))
        
        cash_w_e <- cash_w_b |>
          mutate(across(-거래일자, cummean))
        
        
        cash_d <- trade |>
          filter(통화 == '달러') |>
          group_by(거래일자, 계좌) |>
          summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                    .groups = 'keep') |>
          pivot_wider(names_from = 계좌, values_from = 현금) |> 
          ungroup()
        
        cash_d_b <- cash_d |>
          mutate(across(-거래일자, cumsum))
        
        cash_d_e <- cash_d_b |>
          mutate(across(-거래일자, cummean))
        
        cash_y <- trade |>
          filter(통화 == '엔화') |>
          group_by(거래일자, 계좌) |>
          summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                    .groups = 'keep') |>
          pivot_wider(names_from = 계좌, values_from = 현금) |>
          ungroup()
        
        cash_y_b <- cash_y |>
          mutate(across(-거래일자, cumsum))
        
        cash_y_e <- cash_y_b |>
          mutate(across(-거래일자, cummean))
        
        bs_pl |>
          mutate(
            장부금액 = replace(장부금액, 종목명=='나무예수금', 
                               cash_w_b$나무),
            장부금액 = replace(장부금액, 종목명=='한투예수금', 
                               cash_w_b$한투),
            장부금액 = replace(장부금액, 종목명=='한투CMA예수금',
                               cash_w_b$한투CMA),
            장부금액 = replace(장부금액, 종목명=='한투ISA예수금',
                               cash_w_b$한투ISA),
            평잔 = replace(평잔, 종목명=='나무예수금', cash_w_e$나무),
            평잔 = replace(평잔, 종목명=='한투예수금', cash_w_e$한투),
            평잔 = replace(평잔, 종목명=='한투CMA예수금', cash_w_e$한투CMA),
            평잔 = replace(평잔, 종목명=='한투ISA예수금', cash_w_e$한투ISA),
            장부금액 = replace(장부금액, 종목명=='불리오달러', 
                               cash_d_b$불리오),
            장부금액 = replace(장부금액, 종목명=='직접운용달러', 
                               cash_d_b$한투),
            평잔 = replace(평잔, 종목명=='불리오달러', cash_d_e$불리오),
            평잔 = replace(평잔, 종목명=='직접운용달러', cash_d_e$한투), 
            장부금액 = replace(장부금액, 종목명=='직접운용엔', 
                               cash_y_b$한투),
            평잔 = replace(평잔, 종목명=='직접운용엔', cash_y_e$한투),
            실현수익률 = 실현손익 / 평잔 * 100)
        
      } else {
        bs_pl |> 
          mutate(실현수익률 = 실현손익 / 평잔 * 100)
      }
      
    },
    
    ## 5.(메서드)투자자산 평가반영 잔액-손익 테이블 생성====
    evaluate_bs_pl_assets = function() {
      
      if (is.null(self$bl) && is.null(self$my)) {
        self$bl <- AutoInvest$new('boolio')
        self$my <- AutoInvest$new('my')
      }
      
      
      p_lotte <- as.integer(self$bl$get_current_price("011170")$stck_prpr)
      q_lotte <- filter(self$bs_pl_book_a, 
                        종목명=='롯데케미칼', 
                        거래일자 == self$today)$보유수량
      
      price <- self$assets |>
        select(종목코드, 상품명, 평가금액) |> 
        mutate(평가금액 = replace(평가금액, 
                              상품명 == '우리사주 롯데케미칼', 
                              p_lotte*q_lotte)) |> 
        filter(평가금액!=0) |> 
        bind_rows(
          self$my$inquire_balance(),
          self$my$inquire_balance_ovs(),
          self$my$inquire_balance_ovs('JPY'),
          self$bl$inquire_balance_ovs()) |> 
        select(종목코드,평가금액)

      
      bs_pl <- self$bs_pl_book_a |> 
        filter(거래일자 == self$today) |> 
        left_join(price, by="종목코드") |> 
        mutate(
          평가금액 = ifelse(is.na(평가금액), 장부금액, 평가금액)) 
        # left_join(
        #   (self$assets |> select(종목코드, 기초평가손익)), 
        #   by="종목코드")
      
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
          # 평가손익증감 = 평가금액 - 장부금액,
          # 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
          평가손익 = 평가금액 - 장부금액, 
          평가수익률 = 평가손익 / 장부금액 * 100
        ) |> 
        arrange(desc(통화), desc(평가금액)) |> 
        filter(평잔!=0)
    },
    
    ## 6.(메서드)연금 평가반영 잔액-손익 테이블 생성========
    evaluate_bs_pl_pension = function(){

      price <- self$pension |>
        select(종목코드, 평가금액, 기초평가손익)
      
      self$bs_pl_book_p |> 
        filter(거래일자 == self$today) |> 
        left_join(price, by="종목코드") |> 
        mutate(
          # 평가손익증감 = 평가금액 - 장부금액,
          # 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
          평가손익 = 평가금액 - 장부금액,
          평가수익률 = 평가손익 / 장부금액 * 100
        ) |> 
        arrange(desc(통화), desc(평가금액)) |> 
        filter(평잔!=0)
    },
    
    ## 7.(메서드) 자산군별 수익률 현황====
    get_class_returns = function(mode='assets', depth=2) {
      
      ### 1) 통화별/계좌별 수익률 산출함수====
      get_class_returns <- function(asset_returns, total=F) {
        
        if(mode=='assets') {
          dist <- asset_returns$통화[1]
          if(dist=="달러"){ 
            ex <- self$ex_usd 
          } else if(dist=="엔화"){ 
            ex <- self$ex_jpy 
          } else { 
            ex <- 1
          }
        } else {
          if(total){
            dist <- '전체'
          } else {
            dist <- asset_returns$계좌[1]
          }
          ex = 1
        }
        
        if(depth==2){
          grp <- c('자산군','세부자산군')
          grp_list <- list(자산군='전체', 세부자산군=NA)
          del <- NULL
        } else {
          grp <- c('자산군')
          grp_list <- list(자산군='전체')
          del <- c('세부자산군')
        }
        
        class_returns <- asset_returns |> 
          select(-(종목코드:보유수량), -세부자산군2, 
                 -실현수익률, -평가수익률, -all_of(del)) |> 
          mutate(across(-all_of(grp), ~round(.x * ex,0))) |> 
          group_by(across(all_of(grp))) |> 
          summarise(across(everything(), ~sum(.x, na.rm = TRUE)),
                    .groups='drop') |> 
          mutate(구분 = dist, .before=1)
          
        class_returns |> 
          add_row(구분 = dist, !!!grp_list, 
                  !!(class_returns |>
                       select(-구분,-all_of(grp)) |> 
                       summarise(across(everything(), 
                                        ~sum(.x, na.rm=T))))
                  )|> 
          mutate(실현수익률 = 실현손익 / 평잔 * 100,
                 # 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
                 평가수익률 = 평가손익 / 장부금액 * 100)
      }
      
    
      ### 2) 자산 수익률====
      if(mode=='assets'){
        
        df <- self$bs_pl_mkt_a
        
        krw_ret <- df |>  
          filter(통화 == '원화') |> 
          get_class_returns()
        
        usd_ret <- df |> 
          filter(통화 == '달러') |> 
          get_class_returns()
        
        jpy_ret <- df |> 
          filter(통화 == '엔화') |> 
          get_class_returns()
        
        bind_rows(krw_ret, usd_ret, jpy_ret)
        
      } else {
        
        df <- self$bs_pl_mkt_p
        
        nhb_ret <- df |>  
          filter(계좌 == '농협IRP') |> 
          get_class_returns()
        
        shi_ret <- df |> 
          filter(계좌 == '삼성DC') |> 
          get_class_returns()
        
        nhi_ret <- df |> 
          filter(계좌 == '엔투저축연금') |> 
          get_class_returns()
        
        ret <- df |> get_class_returns(total=T)
        
        bind_rows(nhb_ret, shi_ret, nhi_ret, ret)
        
      }
      
    },
    
    ## 8.(메서드) 투자자산 자산군별 배분 현황====
    compute_allocation_a = function() {
      
      df <- self$bs_pl_mkt_a
      usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)                    
      jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)                    
      
      df <- df |> 
        filter(자산군 != '외화자산') |>
        mutate(평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
               평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval)) |>
        group_by(자산군, 세부자산군, 통화) |>
        summarize(평가금액 = sum(평가금액), .groups = 'drop') |>
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      ### 자산군별 배분현황
      self$allo0 <- df |>
        group_by(자산군) |>
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(자산군='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      self$allo1 <- df |>
        add_row(자산군='합계', 평가금액=sum(df$평가금액), 투자비중=100) |>
        group_by(자산군) |>
        mutate(자산별비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      ### 통화화별 배분현황
      self$allo2 <- df |>
        group_by(통화) |>
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(통화='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      
      self$allo3 <- df |>
        group_by(통화, 자산군) |>
        summarize(평가금액 = sum(평가금액), 
                  투자비중 = sum(투자비중), .groups = 'drop') |>
        add_row(통화='합계', 평가금액 = sum(df$평가금액), 투자비중=100) |>
        group_by(통화) |> 
        mutate(통화별비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      
      ### 불리오 배분현황
      df_b <- self$bs_pl_mkt_a |>
        filter(계좌 == '불리오') %>% 
        mutate(평가금액 = round(평가금액*self$ex_usd,0))
      
      self$allo4 <- df_b |>
        group_by(세부자산군, 세부자산군2) |>
        summarize(평가금액 = sum(평가금액), .groups = 'drop') |>
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2)) |>
        add_row(세부자산군='합계', 
                평가금액 = sum(df_b$평가금액), 투자비중=100)
      
      self$allo5 <- df_b |>
        group_by(세부자산군) |>
        summarize(평가금액 = sum(평가금액), .groups = 'drop') |>
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2)) |>
        add_row(세부자산군='합계', 
                평가금액 = sum(df_b$평가금액), 투자비중=100)
    },
    
    ## 9.(메서드) 연금 자산군별 배분 현황====
    compute_allocation_p = function() {
      df <- self$bs_pl_mkt_p |> 
        group_by(계좌, 자산군, 세부자산군) |>
        summarize(평가금액 = sum(평가금액), .groups = 'drop') |>
        mutate(투자비중 = 평가금액 / sum(평가금액) * 100)
      
      self$allo6 <- df |>
        group_by(자산군) |>
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(자산군='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      
      self$allo7 <- df |>
        group_by(자산군, 세부자산군)  |> 
        summarize(평가금액 = sum(평가금액), 
                  투자비중 = sum(투자비중), .groups = 'drop') |>
        add_row(자산군='합계', 평가금액 = sum(df$평가금액), 
                투자비중=100) |> 
        group_by(자산군) |> 
        mutate(자산별비중 = 평가금액 / sum(평가금액) * 100)
      
      self$allo8 <- df |>
        group_by(계좌) |>
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) |> 
        add_row(계좌='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      self$allo9 <- df |>
        add_row(계좌='합계', 평가금액=sum(df$평가금액), 투자비중=100) |>
        group_by(계좌) |>
        mutate(자산별비중 = 평가금액 / sum(평가금액) * 100)
    },
    ## 10.(메서드) 평가금액 포함 자료 산출====
    run_valuation = function(){
      self$bs_pl_mkt_a <- self$evaluate_bs_pl_assets()
      self$bs_pl_mkt_p <- self$evaluate_bs_pl_pension()
      self$ret_a <- self$get_class_returns('assets')
      self$ret_p <- self$get_class_returns('pension')
      self$ret_a2 <- self$get_class_returns('assets', depth = 1)
      self$ret_p2 <- self$get_class_returns('pension', depth = 1)
      self$compute_allocation_a()
      self$compute_allocation_p()
    }
  )
)

#[클래스] MyData ====
MyData <- R6Class(
  
  classname = 'MyData',
  public=list(
    
    con=NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(file){
      
      self$con <- dbConnect(SQLite(), file, bigint = 'numeric',
                            extended_types=T)
      
    },
    
    ##2.(메서드) 테이블 추가 ====
    add_table = function(name, table){
      dbWriteTable(self$con, name, table)
    },
    
    ##3.(메서드) 테이블 읽기 ====
    
    read = function(name){
        dbReadTable(self$con, name) |> tibble()
    },
    
    ##4.(메서드) 테이블 읽기(dbplyr 객체) ====
    read_obj = function(name){
      tbl(self$con, name)
    },
    
    ##5.(메서드) 테이블 목록 ====
    table_list = function(){
      dbListTables(self$con)
    }
  )
)

get_holidays <- function(start_year, end_year){
  service_key <- get_config()$holiday
  request_url <- 'http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo'
  
  out <- tibble()
  for (request_year in start_year:end_year) {
    for (request_month in sprintf("%02d", 1:12)) {
      x <- GET(url = request_url,
               query = list(serviceKey = I(service_key),
                            solYear = request_year,
                            solMonth = request_month))
      
      # export data from json
      x <- x %>% 
        content(as = "text", encoding = "UTF-8") %>% 
        fromJSON()
      
      if (x$response$body$items != "") {
        x <- as_tibble(x$response$body$items$item)|> 
          transmute(
            기준일=as.Date(as.character(locdate), 
                        format='%Y%m%d'), 
            공휴일명=dateName)
        
        out <- out %>% 
          bind_rows(x)
      }
    }
  }
  return(out)
}
