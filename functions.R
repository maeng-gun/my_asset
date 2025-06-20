library(tidyverse)
library(timetk)
library(R6)
library(ecos)
library(glue)
library(httr)
library(jsonlite)
library(rvest)
library(RSQLite)
library(dbx)
library(tidyquant)


get_config <- function(){
  yaml::read_yaml(file = 'config.yaml', 
                  readLines.warn = F)
}

get_exchange_rate <- function(cur='달러'){
  
  num <- c('달러'= 1, '엔'= 2, '유로'=3, '위안'=4)
  
  (read_html("http://finance.naver.com/marketindex/") %>% 
      html_nodes("div.head_info > span.value")
  )[num[cur]] %>%
    html_text() %>% 
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
      get_config()$ecos %>% ecos.setKey()
      #(속성) 통계표 목록
      self$table_list <- statTableList() %>% tibble()
    },
    
    #(메서드)통계표 검색
    find_stat = function(name=''){
      
      if(is.null(name)||name == '') {
        self$table_list
      }
      else{
        self$table_list %>% 
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
        statItemList(code) %>% 
          tibble() %>% 
          select(stat_name, item_name, stat_code, item_code, cycle:data_cnt) %>% 
          mutate(stat_name = stringr::str_extract(stat_name,"(?<=\\.\\s).*"))
      }
    },
    
    #(메서드)저장된 아이템 읽기
    read_items = function(){
      readRDS('ecos_items.rds')
    },
    
    #(메서드)아이템 저장
    save_items = function(df, name){
      
      df2 <- df %>% mutate(new_name = name, .before=1)
      
      self$read_items() %>% 
        bind_rows(df2) %>% 
        distinct() %>% 
        arrange(stat_code,item_code) %>% 
        saveRDS('ecos_items.rds')
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
      dbReadTable(self$con, name) %>% tibble()
    },
    
    ##4.(메서드) 테이블 읽기(dbplyr 객체) ====
    read_obj = function(name){
      tbl(self$con, name)
    },
    
    ##5.(메서드) 테이블 목록 ====
    table_list = function(){
      dbListTables(self$con)
    },
    
    ##6.(메서드) 추가/갱신하기 ====
    upsert = function(df, name, cols){
      dbxUpsert(conn = self$con, table = name, records = df,
                where_cols = cols)
    }
  )
)

#[클래스] KrxStocks ====
KrxStocks <- R6Class(
  
  classname = 'KrxStocks',
  public=list(
    
    user.agent = NULL, referer = NULL,
    stock_list = NULL, last_wd = NULL,
    this_wd = NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(date=NULL){
      if(is.null(date)){
        if(hour(now())>=9){date <- today()}
        else {date <- today() - 1}
      }
      self$user.agent <- 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36 '
      self$referer <- 'http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201'
      self$this_wd <- self$get_workdays(year(date))
      self$last_wd <- self$this_wd %>% 
        filter(기준일<=date) %>% 
        pull(기준일) %>% last()
      self$stock_list <- self$get_stock_list()
    },
    
    ##2.(메서드) KRX POST ====
    post_krx = function(site, params){
      
      url <- list(
        data='http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd',
        open='http://open.krx.co.kr/contents/OPN/99/OPN99000001.jspx')
      
      res <- POST(url=url[site],
                  query=params, 
                  user_agent(self$user.agent), 
                  add_headers(referer=self$referer)) %>% 
        content('t') %>% 
        jsonlite::fromJSON()
      
      res[[ names(res)[1] ]] %>% 
        as_tibble()
    },
    
    ##3.(메서드) 영업일 얻기 ====
    get_workdays = function(year){
      
      unix_time <- 
        (as.numeric(Sys.time()) * 1000) %>% round() %>% as.character()
      
      otp_params <- list(
        bld = 'MKD/01/0110/01100305/mkd01100305_01',
        name = 'form',
        '_' = unix_time)

      otp_code <- 
        GET(url='http://open.krx.co.kr/contents/COM/GenerateOTP.jspx',
            query=otp_params,
            user_agent(self$user.agent)) %>% 
        content('t')
      
      view_params <- list(
        search_bas_yy = as.character(year),
        gridTp = 'KRX',
        pagePath = 'contents/MKD/01/0110/01100305/MKD01100305.jsp',
        code = otp_code)
      
      holidays <- 
        self$post_krx('open',view_params)$calnd_dd %>% 
        as.Date()
      
      start <- glue('{year}-01-01')
      end <- glue('{year}-12-31')
      
      cal <- bizdays::create.calendar(
        name = 'mycal',
        holidays = holidays,
        weekdays = c('saturday','sunday'),
        start.date = start,
        end.date = end)
      
      tibble(기준일=bizdays::bizseq(start, end, cal))
    },
    
    ##4.(메서드) 주식 종목(ETF포함) 리스트 얻기====
    get_stock_list = function(date=NULL){
      
      if(is.null(date)){ 
        yyyymmdd <- strftime(self$last_wd, '%Y%m%d')
      } else {
        yyyymmdd <- strftime(date, '%Y%m%d')
      }
      
      params1 <- list(bld = "dbms/MDC/STAT/standard/MDCSTAT01501",
                      mktId = "ALL",
                      trdDd = yyyymmdd)
      
      params2 <- list(bld = "dbms/MDC/STAT/standard/MDCSTAT04301",
                      mktId = "ALL",
                      trdDd = yyyymmdd)
      
      self$post_krx('data', params1) %>% 
        bind_rows(
          self$post_krx('data', params2) %>% 
            mutate(MKT_ID='STK')
        ) %>% 
        select(종목코드=ISU_SRT_CD, 종목명=ISU_ABBRV, 
               종가=TDD_CLSPRC, 시장구분=MKT_ID) %>% 
        mutate(시장구분=case_match(시장구분, 'STK'~'코스피', 'KSQ'~'코스닥', 
                               'KNX'~'코넥스'),
               종가 = readr::parse_number(종가)) %>% 
        filter(str_ends(종목코드,'0'))
    },
    
    ##4.(메서드) 종목코드 찾기 ====
    find_code = function(name=NULL, mkt=NULL){
      
      df <- self$get_stock_list() %>% 
        mutate(종목코드=paste0('A',종목코드))
      
      if(!is.null(mkt)){df <- df %>% filter(시장구분 %in% mkt)}
      if(!is.null(name)){
        df <- df %>% 
          filter(str_detect(종목명, str_c(name, collapse = '|')))
      }
      
      df
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
      
      GET(URL, query = data, add_headers(.headers = headers2)) %>% 
        content("text", encoding = 'UTF-8') %>% 
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
        res <- res %>% 
          content("text", encoding = 'UTF-8') %>% 
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
      
      self$GET_tbl(path, data, headers)$output1 %>% 
        tibble() %>% 
        rename_with(~c('매입금액', '평가금액', '평가손익', 
                       '신용대출', '순자산', '비중')) %>% 
        mutate(자산구분 = asset, .before = 1) %>% 
        mutate(across(매입금액:비중, as.numeric)) %>% 
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
      
      self$GET_tbl(path, data, headers)$output1 %>% 
        tibble() %>% 
        select(pdno, prdt_name, evlu_amt) %>%
        rename_with(~c('종목코드', '상품명', '평가금액')) %>% 
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
      
      self$GET_tbl(path, data, headers)$output1 %>% 
        tibble() %>%
        select(ovrs_pdno, ovrs_item_name, 
               ovrs_stck_evlu_amt) %>% 
        rename_with(~c('종목코드', '상품명', '평가금액')) %>% 
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


#[클래스] MyAssets ====
MyAssets <- R6Class(
  classname = "MyAssets",
  inherit = MyData,
  
  public = list(
    md = NULL,
    today = NULL, year = NULL, days = NULL,
    assets = NULL, pension = NULL, ex_usd = NULL, ex_jpy = NULL, 
    assets_daily = NULL, pension_daily = NULL,
    bs_pl_book_a = NULL, bs_pl_book_p = NULL, 
    bs_pl_mkt_a = NULL, bs_pl_mkt_p = NULL,
    bl = NULL, my = NULL, ks = NULL,
    ret_a = NULL, ret_p = NULL, ret_a2 = NULL, ret_p2 = NULL, 
    allo0 = NULL, allo1 = NULL, allo2 = NULL,
    allo3 = NULL, allo4 = NULL, allo5 = NULL, 
    allo6 = NULL, allo7 = NULL, allo8 = NULL, 
    allo9 = NULL, inflow_table = NULL, inflow_plot= NULL,
    inflow_bal=NULL, t_class=NULL, t_comm=NULL, t_comm2=NULL,
    
    ## 1. 속성 초기화====
    initialize = function(base_dt=NULL) {
      
      self$con <- dbConnect(SQLite(), 'mydata.sqlite', bigint = 'numeric',
                            extended_types=T)
      
      if (!is.null(base_dt)) {
        self$today <- ymd(base_dt)
      } else {
        self$today <- today()
      }
      
      self$year <- year(self$today)
      self$days <- seq(make_date(2024,1,1), 
                       make_date(self$year,12,31),by='day')
      self$run_book()
      self$update_new_price()
      self$run_valuation()
      # self$get_inflow()
    },
    
    ## 2.(메서드) 거래내역 기록 테이블====
    #   - '거래내역' 화면에 들어갈 테이블 산출(회계/계좌/통화별)
    get_trading_record = function(table, acct, cur){
      
      if(table=="투자자산"){table <- 'assets'}
      else {table <- 'pension'}
      
      df1 <- self$read(table)
      df2 <- self$read(paste0(table,'_daily'))
      
      df2 %>% left_join(
        (df1 %>% transmute(계좌, 통화, 종목코드, 종목명)), 
        by = c('계좌','종목코드')) %>% 
        filter(계좌==acct, 통화==cur) %>% 
        mutate(매입비용 = 현금지출-매입액,
               매매수익 = 매도액 - 매도원금,
               매도비용 = 매도액 + 이자배당액 - 현금수입,
               순수익 = 매매수익 + 이자배당액 - 매도비용 - 매입비용,
               순현금수입 = 입출금 + 현금수입 - 현금지출,
               잔액 = cumsum(순현금수입)) %>% 
        select(행번호, 계좌, 통화, 거래일자, 종목명, 
               매입수량:현금지출, 매입비용, 매도수량, 매도원금, 
               매도액, 매매수익, 이자배당액, 현금수입, 매도비용, 
               순수익, 입출금, 잔액)
    },
    
    ## 3.(메서드) 계좌거래 내역 전처리 ====
    #   - 회계/계좌/통화별로 각각 기록된 거래내역들을 통합하여
    #   - 모든 일자에 대한 통합된 거래기록을 산출
    #   - 잔액/손익 테이블(bs_pl)을 만들기 위한 전단계
    
    get_daily_trading = function(ast_info, trade){
      
      expand_grid(select(ast_info,계좌,종목코드), 
                  거래일자 = self$days) %>% 
        left_join(select(ast_info, 계좌, 종목코드,종목명, 통화), 
                  by = c("계좌","종목코드")) %>% 
        left_join(trade, by = c("계좌", "종목코드", "거래일자")) %>% 
        mutate(across(매입수량:입출금, ~if_else(is.na(.x),0,.x))) %>% 
        arrange(계좌, 종목코드, 거래일자) %>% 
        mutate(
          순매입수량 = 매입수량 - 매도수량,
          수익 =매도액 - 매도원금 + 이자배당액,
          비용 = 현금지출 - 매입액 + 매도액 + 이자배당액 - 현금수입,
          실현손익 = 수익 - 비용
        ) %>% 
        select(계좌:통화, 순매입수량, 매입액, 매도원금, 
               수익, 비용, 실현손익, 현금수입, 입출금, 현금지출)
    },
    
    
    ## 4.(메서드)운용자산 잔액-손익 테이블 생성====
    #   - 거래내역에 기초해 모든 시점의 장부금액/평잔/손익을 산출
    get_bs_pl = function(mode = 'assets') {
      
      ###(1) 기본 테이블 생성====
      
      if(mode == 'assets') {
        trade <- self$assets_daily %>% 
          arrange(계좌, 종목코드, 거래일자)
        codes <- self$assets
      }
      else {
        trade <- self$pension_daily
        codes <- self$pension
      }
      
      #거래상에 존재하는 모든 종목들의 테이블 형식 세팅
      bs_pl1 <- trade %>%
        select(계좌, 종목코드, 거래일자, 종목명, 통화)
      
      #거래기록을 시점별 잔액 기록으로 변환
      bs_pl2 <- trade %>% 
        group_by(계좌, 종목코드) %>%
        transmute(
          계좌, 종목코드, 거래일자, 수익, 비용, 실현손익,
          보유수량 = cumsum(순매입수량),
          장부금액 = cumsum(매입액 - 매도원금),
          # 평잔 = cummean(장부금액),
          # 수익 = cumsum(수익),
          # 비용 = cumsum(비용),
          # 실현손익 = cumsum(실현손익)
        ) %>%
        ungroup() %>% 
        arrange(계좌, 종목코드, 거래일자)
        # select(-계좌, -종목코드)
      
      bs_pl3 <- bs_pl2 %>% 
        group_by(계좌, 종목코드,연도=year(거래일자)) %>% 
        transmute(
          계좌, 종목코드, 거래일자, 
          평잔 = cummean(장부금액),
          수익 = cumsum(수익),
          비용 = cumsum(비용),
          실현손익 = cumsum(실현손익)
        ) %>% 
        ungroup() %>% 
        arrange(계좌, 종목코드, 거래일자)
      

      bs_pl <- bind_cols(
        bs_pl1,
        bs_pl2 %>% select(-계좌:-실현손익),
        bs_pl3 %>% select(-연도:-거래일자)
      ) %>% 
        left_join(select(codes, 계좌, 종목코드, 자산군, 세부자산군, 세부자산군2), 
                  by = c('계좌','종목코드')) %>%
        arrange(계좌, 종목코드, 거래일자)
      
      
      ###(2) 예수금 & 평잔 처리====
      if(mode=='assets'){
        cash_w <- trade %>%
          filter(통화 == '원화') %>%
          group_by(거래일자, 계좌) %>%
          summarise(현금 = sum(현금수입 + 입출금 - 현금지출),
                    .groups = 'keep') %>%
          pivot_wider(names_from = 계좌, values_from = 현금) %>% 
          ungroup()
        
        cash_w_b <- cash_w %>%
          mutate(across(-거래일자, cumsum))
        
        cash_w_e <- cash_w_b %>%
          group_by(연도=year(거래일자)) %>% 
          mutate(across(-거래일자, cummean)) %>% 
          ungroup() %>% select(-연도)
        
        
        cash_d <- trade %>%
          filter(통화 == '달러') %>%
          group_by(거래일자, 계좌) %>%
          summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                    .groups = 'keep') %>%
          pivot_wider(names_from = 계좌, values_from = 현금) %>% 
          ungroup()
        
        cash_d_b <- cash_d %>%
          mutate(across(-거래일자, cumsum))
        
        cash_d_e <- cash_d_b %>%
          group_by(연도=year(거래일자)) %>% 
          mutate(across(-거래일자, cummean)) %>% 
          ungroup() %>% select(-연도)
        
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
          group_by(연도=year(거래일자)) %>% 
          mutate(across(-거래일자, cummean)) %>% 
          ungroup() %>% select(-연도)
        
        bs_pl %>%
          mutate(
            # 장부금액 = replace(장부금액, 종목명=='나무예수금', 
            #                    cash_w_b$나무),
            장부금액 = replace(장부금액, 종목명=='한투예수금', 
                               cash_w_b$한투),
            장부금액 = replace(장부금액, 종목명=='한투CMA예수금',
                               cash_w_b$한투CMA),
            장부금액 = replace(장부금액, 종목명=='한투ISA예수금',
                               cash_w_b$한투ISA),
            # 평잔 = replace(평잔, 종목명=='나무예수금', cash_w_e$나무),
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
        bs_pl %>% 
          mutate(실현수익률 = if_else(is.na(실현손익 / 평잔 * 100), 0, 실현손익 / 평잔 * 100))
      }
      
    },
    
    ## 5.(메서드)투자자산 평가반영 잔액-손익 테이블 생성====
    evaluate_bs_pl_assets = function() {
      
      # if (is.null(self$bl) && is.null(self$my)) {
        # self$bl <- AutoInvest$new('boolio')
        # self$my <- AutoInvest$new('my')
      # }
      
      
      # p_lotte <- as.integer(self$bl$get_current_price("011170")$stck_prpr)
      # q_lotte <- filter(self$bs_pl_book_a, 
      #                   종목명=='롯데케미칼', 
      #                   거래일자 == self$today)$보유수량

      price <- self$assets %>%
        select(계좌, 종목코드, 상품명, 평가금액) %>% 
        # mutate(평가금액 = replace(평가금액, 
        #                       상품명 == '우리사주 롯데케미칼', 
        #                       p_lotte*q_lotte)) %>% 
        filter(평가금액!=0) %>% 
        # bind_rows(
          # mutate(self$my$inquire_balance(), 계좌 = '한투'),
          # mutate(self$my$inquire_balance_ovs(), 계좌 = '한투'),
          # mutate(self$my$inquire_balance_ovs('JPY'), 계좌 = '한투'),
          # mutate(self$bl$inquire_balance_ovs(), 계좌 = '불리오')
          # ) %>%
        select(계좌, 종목코드,평가금액)

      
      bs_pl <- self$bs_pl_book_a %>% 
        filter(거래일자 == self$today) %>%  
        left_join(price, by=c("계좌","종목코드")) %>% 
        left_join(
          self$ks$stock_list %>% 
            select(종목코드, 종가),
          by='종목코드'
        ) %>% 
        filter(평잔!=0) %>% 
        mutate(
          장부금액 = if_else(장부금액<1, 0, 장부금액),
          평가금액 = case_when(
            !is.na(평가금액) ~ 평가금액,
            !is.na(종가) ~ 종가*보유수량,
            TRUE ~ 장부금액)) %>% 
        select(-종가) 
        # left_join(
        #   (self$assets %>% select(종목코드, 기초평가손익)), 
        #   by="종목코드")
      
      dollar <-
        (sum(filter(bs_pl, 통화 == '달러')$평가금액) * self$ex_usd) %>% 
        round()
      
      yen <-   
        (sum(filter(bs_pl, 통화 == '엔화')$평가금액) * self$ex_jpy) %>% 
        round()
      
      bs_pl %>% 
        mutate(
          평가금액 = replace(평가금액, 종목명 == '달러자산', dollar),
          평가금액 = replace(평가금액, 종목명 == '엔화자산', yen),
          # 평가손익증감 = 평가금액 - 장부금액,
          # 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
          평가손익 = 평가금액 - 장부금액, 
          평가수익률 = 평가손익 / 장부금액 * 100
        ) %>% 
        arrange(desc(통화), desc(평가금액))
    },
    
    ## 6.(메서드)연금 평가반영 잔액-손익 테이블 생성========
    evaluate_bs_pl_pension = function(){

      price <- self$pension %>%
        select(계좌, 종목코드, 평가금액) %>% 
        filter(평가금액!=0)
      
      get_fund_price <- function(code){
        map_dbl(code, function(x){
          x %>% 
            {
              paste0('https://www.funddoctor.co.kr/afn/fund/fprofile2.jsp?fund_cd=',.)
            } %>% 
            read_html() %>%
            html_element(xpath='/html/body/div[1]/div/div[3]/div[2]/div[1]/div[1]') %>% 
            html_text() %>% 
            stringr::str_remove(',') %>% 
            as.numeric()
        })
      }
      
      bs_pl <- self$bs_pl_book_p %>% 
        filter(거래일자 == self$today) %>% 
        left_join(price, by=c("계좌","종목코드")) %>% 
        left_join(
          self$ks$stock_list %>% 
            select(종목코드, 종가),
          by='종목코드'
        ) %>% 
        filter(평잔!=0) %>% 
        mutate(
          장부금액 = if_else(장부금액<1, 0, 장부금액))
      
      bs_pl %>% 
        left_join(
          bs_pl %>% 
            filter(str_sub(종목코드,1,2)=='K5') %>% 
            select(종목코드) %>% 
            mutate(
              기준가 = get_fund_price(종목코드)
            ),
          by='종목코드'
        ) %>% 
        mutate(
          평가금액 = case_when(
            !is.na(평가금액) ~ 평가금액,
            !is.na(종가) ~ 종가*보유수량,
            !is.na(기준가) ~ 기준가*보유수량/1000,
            TRUE ~ 장부금액),
          평가손익증감 = 평가금액 - 장부금액,
          운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
          평가손익 = 평가금액 - 장부금액,
          평가수익률 = 평가손익 / 장부금액 * 100
        ) %>% 
        select(-종가) %>% 
        arrange(desc(통화), desc(평가금액))
    },
    
    ## 7.(메서드) 자산군별 수익률 현황====
    get_class_returns = function(mode='assets', depth=2) {
      
      ### 1) 통화별/계좌별 수익률 산출함수====
      get_class_returns <- function(asset_returns, total=F) {
        
        if(nrow(asset_returns)==0){
          return(tibble())
        } else {
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
          
          class_returns <- asset_returns %>% 
            select(-(계좌:보유수량), -세부자산군2, 
                   -실현수익률, -평가수익률, -all_of(del)) %>% 
            mutate(across(-all_of(grp), ~.x * ex)) %>% 
            group_by(across(all_of(grp))) %>% 
            summarise(across(everything(), ~sum(.x, na.rm = TRUE)),
                      .groups='drop') %>% 
            mutate(구분 = dist, .before=1)
          
          class_returns %>% 
            add_row(구분 = dist, !!!grp_list, 
                    !!(class_returns %>%
                         select(-구분,-all_of(grp)) %>% 
                         summarise(across(everything(), 
                                          ~sum(.x, na.rm=T))))
            )%>% 
            mutate(실현수익률 = 실현손익 / 평잔 * 100,
                   # 운용수익률 = (실현손익 + 평가손익증감) / 평잔 * 100,
                   평가수익률 = 평가손익 / 장부금액 * 100)
        }
      }
      
    
      ### 2) 자산 수익률====
      if(mode=='assets'){
        
        df <- self$bs_pl_mkt_a
        
        krw_ret <- df %>%  
          filter(통화 == '원화') %>% 
          get_class_returns()
        
        usd_ret <- df %>% 
          filter(통화 == '달러') %>% 
          get_class_returns()
        
        jpy_ret <- df %>% 
          filter(통화 == '엔화') %>% 
          get_class_returns()
        
        bind_rows(krw_ret, usd_ret, jpy_ret)
        
      } else {
        
        df <- self$bs_pl_mkt_p
        
        nhb_ret <- df %>%  
          filter(계좌 == '농협IRP') %>% 
          get_class_returns()
        
        shi_ret <- df %>% 
          filter(계좌 == '미래DC') %>% 
          get_class_returns()
        
        nhi_ret <- df %>% 
          filter(계좌 == '엔투저축연금') %>% 
          get_class_returns()
        
        kis_ret <- df %>% 
          filter(계좌 == '한투연금저축') %>% 
          get_class_returns()
        
        nhjyirp_ret <- df %>% 
          filter(계좌 == '엔투IRP') %>% 
          get_class_returns()
        
        hay_ret <- df %>% 
          filter(계좌 == '엔투하영') %>% 
          get_class_returns()
        
        
        ret <- df %>% get_class_returns(total=T)
        
        bind_rows(ret, nhb_ret, shi_ret, nhi_ret, 
                  kis_ret, nhjyirp_ret, hay_ret)
        
      }
      
    },
    
    ## 8.(메서드) 투자자산 자산군별 배분 현황====
    compute_allocation_a = function() {
      
      df <- self$bs_pl_mkt_a
      usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)
      jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)  
      
      df <- df %>% 
        filter(자산군 != '외화자산') %>%
        mutate(평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
               평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval)) %>%
        group_by(자산군, 세부자산군, 통화) %>%
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      df2 <- self$ret_a %>% 
        filter(구분=="원화"|자산군!="전체") %>% 
        group_by(자산군, 세부자산군) %>% 
        summarize(
          평가금액 = sum(평가금액),
          장부금액 = sum(장부금액),
          평가손익 = sum(평가손익),
          .groups = 'drop')
      
      sum_eval <- df2 %>% filter(자산군=='외화자산') %>% .$평가금액 %>% sum()
      sum_bal <- df2 %>% filter(자산군=='외화자산') %>% .$장부금액 %>% sum()
      sum_prof <- (df2 %>% filter(자산군=="전체") %>% .$평가손익 %>% sum()) - (
        df2 %>% filter(!(자산군 %in% c("전체", "외화자산"))) %>% .$평가손익 %>% sum()
      )
      sum_t <- df2 %>% filter(자산군=="전체") %>% .$평가금액 %>% sum()
      
      df_t <- df2 %>% 
        filter(자산군=='전체') %>% 
        mutate(
          평가수익률 = round(평가손익 / 장부금액 * 100,2),
               투자비중=100) %>% 
        select(자산군, 세부자산군, 평가금액, 평가수익률, 투자비중) %>% 
        add_row(
          자산군="<환차익>", 세부자산군='합산', 평가금액=sum_eval, 
          평가수익률=round(sum_prof/sum_bal*100,2), 
          투자비중=round(sum_eval/sum_t*100,2)
        )
      
      df2 <- df2 %>% 
        filter(!(자산군 %in% c("전체","외화자산"))) %>% 
        mutate(
          투자비중 = round(평가금액 / sum(평가금액) * 100,2),
          자산군=factor(자산군, 
                     levels=c("채권","주식","대체자산","현금성", "환차익"))) %>% 
        arrange(자산군)
      
      
      ### 자산군별 배분현황
      self$allo0 <- df2 %>%
        group_by(자산군) %>%
        summarize(across(-세부자산군, ~sum(.x))) %>% 
        mutate(평가수익률 = round(평가손익 / 장부금액 * 100,2)) %>% 
        select(자산군,평가금액,평가수익률,투자비중) %>% 
        bind_rows(df_t %>% select(-세부자산군))
      
      self$allo1 <- df2 %>% 
        mutate(평가수익률 = round(평가손익 / 장부금액 * 100,2)) %>% 
        select(자산군,세부자산군, 평가금액,평가수익률,투자비중) %>% 
        bind_rows(df_t) %>% 
        group_by(자산군) %>% 
        mutate(자산별비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      ### 통화화별 배분현황
      self$allo2 <- df %>%
        group_by(통화) %>%
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) %>% 
        add_row(통화='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      
      self$allo3 <- df %>%
        group_by(통화, 자산군) %>%
        summarize(평가금액 = sum(평가금액), 
                  투자비중 = sum(투자비중), .groups = 'drop') %>%
        add_row(통화='합계', 평가금액 = sum(df$평가금액), 투자비중=100) %>%
        group_by(통화) %>% 
        mutate(통화별비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      
      ### 불리오 배분현황
      df_b <- self$bs_pl_mkt_a %>%
        filter(계좌 == '불리오') %>% 
        mutate(평가금액 = round(평가금액*self$ex_usd,0))
      
      self$allo4 <- df_b %>%
        group_by(세부자산군, 세부자산군2) %>%
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2)) %>%
        add_row(세부자산군='합계', 
                평가금액 = sum(df_b$평가금액), 투자비중=100)
      
      self$allo5 <- df_b %>%
        group_by(세부자산군) %>%
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2)) %>%
        add_row(세부자산군='합계', 
                평가금액 = sum(df_b$평가금액), 투자비중=100)
    },
    
    ## 9.(메서드) 연금 자산군별 배분 현황====
    compute_allocation_p = function() {
      df <- self$bs_pl_mkt_p %>% 
        group_by(계좌, 자산군, 세부자산군)  %>% 
        summarize(평가금액 = sum(평가금액), .groups = 'drop') %>%
        mutate(투자비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      df2 <- self$ret_p %>%
        filter(구분!='전체') %>% 
        group_by(자산군, 세부자산군) %>% 
        summarize(
          평가금액 = sum(평가금액),
          장부금액 = sum(장부금액),
          평가손익 = sum(평가손익),
          .groups = 'drop') %>% 
        filter(자산군!="전체") %>% 
        mutate(
          투자비중 = round(평가금액 / sum(평가금액) * 100,2),
          자산군=factor(자산군, 
                     levels=c("채권","주식","대체자산","현금성", "전체"))) %>% 
        arrange(자산군)
      
      self$allo6 <-  
        df2 %>% 
        group_by(자산군) %>% 
        summarize(across(-세부자산군, ~sum(.x))) %>% 
        add_row(자산군='합계', 평가금액=sum(.$평가금액), 
                장부금액=sum(.$장부금액), 평가손익=sum(.$평가손익),
                투자비중=100) %>% 
        mutate(평가수익률 = round(평가손익 / 장부금액 * 100,2)) %>% 
        select(자산군,평가금액,평가수익률,투자비중)
      
      self$allo7 <-  
        df2 %>% 
        add_row(자산군='합계', 평가금액=sum(.$평가금액), 
                장부금액=sum(.$장부금액), 평가손익=sum(.$평가손익),
                투자비중=100) %>% 
        mutate(평가수익률 = round(평가손익 / 장부금액 * 100,2)) %>% 
        select(자산군,세부자산군, 평가금액,평가수익률,투자비중) %>% 
        group_by(자산군) %>% 
        mutate(자산별비중 = round(평가금액 / sum(평가금액) * 100,2))
      
      self$allo8 <- df %>%
        group_by(계좌) %>%
        summarize(평가금액 = sum(평가금액), 투자비중 = sum(투자비중)) %>% 
        add_row(계좌='합계', 평가금액=sum(df$평가금액), 투자비중=100)
      
      self$allo9 <- df %>%
        add_row(계좌='합계', 평가금액=sum(df$평가금액), 투자비중=100) %>%
        group_by(계좌) %>%
        mutate(자산별비중 = round(평가금액 / sum(평가금액) * 100,2))
    },
    
    compute_total2 = function(){
      
      
      self$bs_pl_book_a %>% 
        bind_rows(self$bs_pl_book_p) %>% 
        group_by(연월=tsibble::yearmonth(거래일자)) %>% 
        filter(거래일자 == max(거래일자)) %>% 
        ungroup() %>% 
        group_by(거래일자, 통화, 자산군, 세부자산군) %>% 
        summarise(평잔=sum(평잔), 실현손익=sum(실현손익)) %>% 
        group_by(거래일자,통화) %>% 
        mutate(통화별평잔= sum(평잔)) %>% 
        group_by(거래일자) %>% 
        mutate(달러자산 = 평잔[세부자산군=='달러자산'],
               엔화자산 = 평잔[세부자산군=='엔화자산'],
               평잔 = case_when(
                 통화=='달러' ~ 달러자산 * 평잔 / 통화별평잔,
                 통화=='엔화' ~ 엔화자산 * 평잔 / 통화별평잔,
                 TRUE ~ 평잔
               )
        )
        
               
        
    
        
    },
    
    compute_total = function(){
      df <- self$bs_pl_mkt_a
      
      usd_bs <- round(filter(df, 통화=='달러')$장부금액 * self$ex_usd,0)
      jpy_bs <- round(filter(df, 통화=='엔화')$장부금액 * self$ex_jpy,0)
      usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)
      jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)

      
      df000 <- df %>%
        mutate(
          장부금액 = replace(장부금액, 통화 == '달러', usd_bs),
          장부금액 = replace(장부금액, 통화 == '엔화', jpy_bs),
          평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
          평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval))%>%
        bind_rows(self$bs_pl_mkt_p) %>%
        filter(장부금액!=0) %>%
        group_by(계좌, 종목코드)
      
      df00 <- self$assets %>%
        bind_rows(self$pension) %>%
        distinct(통화, 계좌, 종목코드, 자산군, 세부자산군, 세부자산군2, 상품명) %>%
        right_join(
           df000 %>%
            summarise(장부금액 = sum(장부금액), 평가금액=sum(평가금액),.groups = 'drop'),
          by=c('계좌','종목코드'))
      
      df01 <- self$assets %>%
        bind_rows(self$pension) %>%
        distinct(통화, 계좌, 종목코드, 자산군, 세부자산군, 세부자산군2, 상품명) %>%
        right_join(
          df000 %>%
            summarise(평잔 = sum(평잔), 실현손익 = sum(실현손익), 장부금액 = sum(장부금액), 평가금액=sum(평가금액),.groups = 'drop'),
          by=c('계좌','종목코드'))%>% 
        group_by(종목코드) %>%
        summarise(통화=last(통화),
                  자산군=last(자산군),
                  세부자산군=last(세부자산군),
                  세부자산군2=last(세부자산군2),
                  상품명 = last(상품명),
                  평잔 = sum(평잔),
                  실현손익 = sum(실현손익),
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액),
                  .groups = 'drop') %>% 
        select(-종목코드)
      
      df02 <- df01 %>% group_by(통화, 자산군, 세부자산군, 세부자산군2) %>%
             summarise(평잔 = sum(평잔), 실현손익 = sum(실현손익), 장부금액=sum(장부금액), 평가금액=sum(평가금액),
                       .groups = 'drop') %>% mutate(장부금액 = if_else(통화=="달러", 0, 장부금액))
      
      df0 <- df00 %>% 
        group_by(종목코드) %>%
        summarise(통화=last(통화),
                  자산군=last(자산군),
                  세부자산군=last(세부자산군),
                  세부자산군2=last(세부자산군2),
                  상품명 = last(상품명),
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액),
                  .groups = 'drop') %>% 
        select(-종목코드)
      
      df2 <- df0 %>% 
        filter(통화=='원화') %>% 
        select(-통화) %>% 
        summarise(자산군="<합계>", 세부자산군 = '',
                  세부자산군2 = '',상품명 = '', 장부금액=sum(장부금액), 
                  평가금액=sum(평가금액), .groups = 'drop')
      
      df1 <- df0 %>% 
        select(-통화) %>% 
        filter(자산군!="외화자산")
      
      df3 <- df1 %>%
        group_by(자산군) %>%
        summarise(세부자산군='', 세부자산군2 = '', 상품명 = '', 장부금액=sum(장부금액), 
                  평가금액=sum(평가금액)) %>%
        mutate(비중1 = round(평가금액/df2$평가금액*100, 1))
      
      df4 <- df1 %>%
        group_by(자산군, 세부자산군) %>%
        summarise(세부자산군2 = '', 상품명 = '', 장부금액=sum(장부금액), 
                  평가금액=sum(평가금액), .groups = 'drop') %>%
        mutate(비중2 = round(평가금액/df2$평가금액*100, 1))
      
      df5 <- df1 %>%
        group_by(자산군, 세부자산군, 세부자산군2) %>%
        summarise(상품명 = "", 장부금액=sum(장부금액), 평가금액=sum(평가금액),
                  .groups = 'drop') %>%
        mutate(비중3 = round(평가금액/df2$평가금액*100, 1))
      
      df6 <- tibble_row(
        자산군='환차손익', 세부자산군='', 세부자산군2 = '',
        평가금액=0,
        평가손익= (sum(df3$장부금액) - df2$장부금액),
        평가수익률 = round(평가손익/df2$평가금액*100,2)
      )
      
       df7 <- bind_rows(df2,df3,df4,df5) %>%
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액,0),
          평가수익률 = round(평가손익 / 장부금액 * 100,2)
        ) %>% 
        select(!c(상품명, 장부금액)) %>% 
        bind_rows(df6)
      
      df7 %>% 
        select(!c(비중1, 비중2, 비중3)) %>% 
        mutate(기준일=self$today, .before=1) %>% 
        self$upsert('return', c('기준일','자산군','세부자산군','세부자산군2'))
      
      dates <- self$read_obj('return') %>% 
        distinct(기준일) %>% 
        arrange(desc(기준일)) %>% 
        pull()
      
      start <- dates[1]
      end <- dates[2]
      
      df8 <- self$read_obj('return') %>% 
        filter(기준일 %in% c(end, start)) %>% 
        collect() %>% 
        select(-평가금액,-평가수익률) %>% 
        spread(key=기준일, value=평가손익, drop=T) %>% 
        rename_with(~c("전일","당일"), .cols=4:5) %>% 
        mutate(`전일대비(손익)` = .[[5]] - .[[4]]) %>% 
        select(!전일:당일)
      
      df9 <- self$read_obj('return') %>% 
        filter(기준일 %in% c(end, start)) %>% 
        collect() %>% 
        select(-평가금액,-평가손익) %>% 
        spread(key=기준일, value=평가수익률, drop=T) %>% 
        rename_with(~c("전일","당일"), .cols=4:5) %>% 
        transmute(`전일대비(수익률)` = .[[5]] - .[[4]])
      
      self$t_class <- 
        df7 %>% 
        left_join(
          df8 %>% bind_cols(df9),
          by=c('자산군','세부자산군','세부자산군2')
        )
      
      self$t_comm <- bind_rows(df1,df2,df3,df4,df5) %>%
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액,0),
          평가수익률 = round(평가손익 / 장부금액 * 100,2)
        ) %>% 
        select(!c(비중1, 비중2, 비중3, 장부금액)) %>% 
        bind_rows(df6)
      
      self$t_comm2 <- df00 %>%
        select(자산군, 세부자산군, 세부자산군2, 계좌, 상품명, 평가금액) %>% 
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액))
    },
    
    total_trading = function(dates){
      
      if(length(dates)==1){
        start = dates
        end = dates
      } else {
        start = dates[1]
        end = dates[2]
      }
      
      df1 <- self$read('assets') %>% 
        bind_rows(self$read('pension'))
      
      df2 <- self$read('assets_daily') %>% 
        bind_rows(self$read('pension_daily')) %>% 
        filter(between_time(거래일자, start, end))
      
      df3 <- df2 %>% left_join(
        (df1 %>% transmute(계좌, 통화, 종목코드, 자산군, 세부자산군, 세부자산군2, 상품명)), 
        by = c('계좌','종목코드')) %>% 
        filter(자산군!='현금성') %>% 
        filter(매입액!=0|매도액!=0) %>% 
        select(자산군, 세부자산군, 세부자산군2, 통화, 거래일자, 계좌, 상품명, 
               매입수량, 매입액, 매도수량, 매도액) %>% 
        arrange(자산군, 세부자산군, 세부자산군2, 통화, 거래일자, desc(매입액), desc(매도액))
      
      df4 <- df3 %>% summarise(거래일자=NA_Date_, 계좌='', 자산군='', 세부자산군='',
                               세부자산군2='', 상품명='합계', 매도액=sum(매도액), 
                               매입액=sum(매입액),.groups = 'drop')
      
      df3 %>% bind_rows(df4)
    },
    
    plot_total_profit = function(start, end){
      df <- self$read_obj('return') %>% 
        filter(자산군=='<합계>') %>% 
        collect() %>% 
        transmute(기준일=as.Date(기준일),평가금액) %>% 
        filter(기준일>=start, 기준일<=end)
        
      
      df1 <- df %>% full_join(
        self$pension_daily %>%
          bind_rows(self$assets_daily) %>% 
          filter(거래일자 %>% between(first(df$기준일),last(df$기준일))) %>% 
          group_by(거래일자) %>% 
          summarise(입출금=sum(입출금)) %>% 
          rename(기준일=거래일자),
        by='기준일'
      ) %>% 
        arrange(기준일) %>% 
        fill(평가금액, .direction = 'down') %>% 
        mutate(입출금=na.fill(입출금,0)) %>% 
        mutate(
          일간수익률 = na.fill((평가금액-입출금-lag(평가금액))/lag(평가금액)*100,0),
          누적수익률 = (cumprod(일간수익률/100+1)-1)*100,
          일간손익= na.fill(diff_vec(평가금액, silent = T)-입출금,0)/10000,
          손익누계= cumsum(일간손익))
      
      fig1 <- df1 %>% 
        ggplot(aes(x=기준일)) +
        geom_line(aes(y=누적수익률))+
        geom_bar(aes(y=일간수익률), stat='identity')+
        scale_y_continuous(
          breaks = function(x){seq(
            floor(x[1] / 0.25) * 0.25,  # 최소값을 0.25 단위로 내림
            ceiling(x[2] / 0.25) * 0.25,  # 최대값을 0.25 단위로 올림
            by = 0.25  # 0.25 간격
          )}, sec.axis = dup_axis(name=NULL)
        )+
        theme(text=element_text(size=20))
      
      fig2 <- df1 %>% 
        ggplot(aes(x=기준일)) +
        geom_line(aes(y=손익누계))+
        geom_bar(aes(y=일간손익), stat='identity') +
        scale_y_continuous(
          breaks = function(x){seq(
            floor(x[1] / 50) * 50,
            ceiling(x[2] / 50) * 50,
            by = 50  
          )}, sec.axis = dup_axis(name=NULL)
        )+
        theme(text=element_text(size=20))
      
      gridExtra::grid.arrange(fig1,fig2,nrow=2)
    },
    
    compute_t_profit = function(){
      tibble(거래일자=as.Date('2023-12-31'),
             투자평잔 = 63019405,
             투자실현손익 = 2376343,
             연금평잔 = 0,
             연금실현손익 = 0,
             총실현손익 = 2376343,
             평가손익 = 1643492,
             총손익 = 4019835) %>% 
        bind_rows(
          self$bs_pl_book_a %>%
            filter(통화=='원화') %>% 
            group_by(거래일자) %>% 
            summarise(투자평잔=sum(평잔), 투자실현손익=sum(실현손익)) %>% 
            left_join(
              self$bs_pl_book_p %>% 
                group_by(거래일자) %>% 
                summarise(연금평잔=sum(평잔), 연금실현손익=sum(실현손익)),
              by='거래일자'
            ) %>% 
            left_join(
              self$read('return') %>% 
                filter(자산군=='<합계>') %>% 
                mutate(거래일자=as.Date(기준일)) %>% 
                select(거래일자, 평가손익),
              by='거래일자'
            ) %>% 
            transmute(거래일자, 투자평잔, 투자실현손익, 연금평잔, 연금실현손익,
                      총실현손익=투자실현손익+연금실현손익,
                      평가손익,
                      총손익=총실현손익+평가손익)
        ) %>% 
        filter(거래일자<=self$today)
    },
    
    ## 10.(메서드) 평가금액 포함 자료 산출====
    run_valuation = function(){
      self$bs_pl_mkt_a <- self$evaluate_bs_pl_assets()
      self$bs_pl_mkt_p <- self$evaluate_bs_pl_pension()
      self$ret_a <- self$get_class_returns('assets')
      self$ret_a2 <- self$get_class_returns('assets', depth = 1)
      self$ret_p <- self$get_class_returns('pension')
      self$ret_p2 <- self$get_class_returns('pension', depth = 1)
      self$compute_allocation_a()
      self$compute_allocation_p()
      self$compute_total()
    },
    
    ## 11.(메서드) 장부금액 및 현금성자산 추이 산출====
    get_funds = function(){
      
      df1 <- self$bs_pl_mkt_a %>% 
        mutate(계정='투자만기') %>% 
        bind_rows(
          self$bs_pl_mkt_p %>% mutate(계정='연금만기')
        )
      
      df2 <- df1 %>% 
        filter(자산군=='채권', 세부자산군 %in% c('만기무위험','만기회사채'), 
               통화=='원화', 평가금액>0) %>% 
        select(계정, 종목코드, 평가금액) %>% 
        left_join(
          self$assets %>% 
            bind_rows(self$pension) %>% 
            select(종목코드, 만기일),
          by='종목코드'
        ) %>% 
        filter(만기일>self$today)
      
      
      last_y <- df2 %>% arrange(만기일) %>% 
        pull() %>% last() %>% year()
      
      
      df <- tibble(
        거래일자 = seq(self$today, make_date(last_y,12,31), by=1)) %>% 
        left_join(
          df1 %>% 
            filter(자산군=='현금성', 통화=='원화', 평가금액>0) %>% 
            select(거래일자, 계정, 평가금액) %>% 
            group_by(거래일자, 계정) %>% summarise(만기상환=sum(평가금액)) %>% 
            bind_rows(
              df2 %>% select(거래일자=만기일, 계정, 만기상환=평가금액)
            )%>% 
            group_by(거래일자,계정) %>% 
            summarise(만기상환=sum(만기상환)) %>% 
            ungroup() %>% 
            spread(계정,만기상환,fill = 0),
          by='거래일자'
        ) %>% 
        left_join(
          self$read('inflow') %>% select(-행번호),
          by='거래일자'
        ) %>% 
        mutate(across(-거래일자, ~replace_na(.,0))) %>% 
        group_by(거래연월=tsibble::yearmonth(거래일자)) %>% 
        summarise(across(-거래일자,sum)) %>% 
        transmute(거래월= tsibble::yearmonth(거래연월) %>% 
                    format(format = '%Y-%m') %>% 
                    as.character(), 
                  투자가용자금=투자만기+투자유출입, 
                  연금가용자금=연금만기+연금유출입,
                  총가용자금=투자가용자금+연금가용자금)
      
      df3 <- df1 %>% filter(통화=='원화', 자산군!='현금성') %>% 
        group_by(계정) %>% 
        summarise(평가금액=sum(평가금액)) %>% 
        spread(계정, 평가금액)
      
      df4 <- df2 %>% 
        group_by(계정) %>% 
        summarise(만기상환=sum(평가금액)) %>% 
        ungroup() %>% 
        spread(계정,만기상환,fill = 0)
      
      s1 <- sum(df$투자가용자금)
      s2 <- sum(df$연금가용자금)
      s3 <- df3$투자만기 - sum(df4$투자만기)
      s4 <- df3$연금만기 - sum(df4$연금만기)
      
      
      df %>% 
        add_row(거래월 = c('자금누계', '보유자산', '총자산누계'),
                투자가용자금 = c(s1, s3, s1+s3),
                연금가용자금 = c(s2, s4, s2+s4),
                총가용자금= c(s1+s2, s3+s4, s1+s2+s3+s4))
    },
    
    # get_inflow = function(){
    #   
    #   df1 <- self$bs_pl_book_a %>% 
    #     group_by(거래일자) %>% 
    #     summarise(장부금액 = sum(장부금액), .groups='drop') %>% 
    #     left_join(
    #       self$bs_pl_book_a %>% 
    #         filter(자산군=='현금성', 통화=='원화') %>% 
    #         group_by(거래일자) %>% 
    #         summarise(현금성자산 = sum(장부금액), .groups='drop'),
    #       by = '거래일자'
    #     ) 
    #   
    #   y <- df1 %>% filter(거래일자 == today()-1)
    #   
    #   df3 <- 
    #     self$read('inflow') %>% 
    #     filter(거래일자 > today())
    #     
    #   df2 <- df1 %>% 
    #     filter(거래일자 >= today()) %>%
    #     select(거래일자) %>% 
    #     left_join(
    #       df3, 
    #       by='거래일자') %>% 
    #     replace_na(list(순자금유입=0, 만기상환=0)) %>% 
    #     mutate(across(-거래일자, cumsum)) %>% 
    #     transmute(
    #       거래일자,
    #       장부금액 = y$장부금액+순자금유입,
    #       현금성자산 = y$현금성자산+순자금유입+만기상환)
    #   
    #   self$inflow_bal <- df1 %>% 
    #     filter(거래일자 < today()) %>% 
    #     bind_rows(df2)
    #   
    #   self$inflow_table <- 
    #     df3 %>% 
    #     left_join(self$inflow_bal, by='거래일자') %>% 
    #     arrange(거래일자)
    #   
    #   
    #   self$inflow_plot <- self$inflow_bal %>% 
    #     pivot_longer(cols=-거래일자, names_to = '구분', values_to = '금액') %>% 
    #     mutate(금액 = 금액/10000) %>% 
    #     ggplot(aes(x=거래일자,y=금액, color=구분))+
    #     geom_line(linewidth=2)+
    #     facet_grid(rows='구분', scales = 'free_y')
    # },
    
    ## 12.(메서드) 장부잔액 자료 산출====
    run_book = function(){
      self$assets <- self$read('assets')
      self$pension <- self$read('pension')
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      self$assets_daily <- self$get_daily_trading(
        self$assets, self$read('assets_daily')
      )
      self$pension_daily <- self$get_daily_trading(
        self$pension, self$read('pension_daily')
      )
      self$bs_pl_book_a <- self$get_bs_pl('assets')
      self$bs_pl_book_p <- self$get_bs_pl('pension')
    },
    
    ## 13.(메서드) 주가 업데이트====
    update_new_price = function(){
      self$ks <- KrxStocks$new()
    }
    
  )
)


#[함수] 공휴일 얻기====
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
        x <- as_tibble(x$response$body$items$item)%>% 
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

#[클래스] Scrap_econ ====
Scrap_econ <- R6Class(
  
  classname = 'Scrap_econ',
  inherit = MyData,
  public=list(

    ##1. 속성 초기화 ====
    initialize = function(){
      self$con <- dbConnect(SQLite(), 'mydata.sqlite', bigint = 'numeric',
                            extended_types=T)
      get_config()$ecos %>% ecos.setKey()
    },
    
    ##2. 일별 금융데이터 수집====
    scrap_daily = function(start=NULL, end=NULL, item=NULL){
      
      if(is.null(start)){
        start <- self$read_obj('econ_daily') %>% 
          distinct(date) %>% 
          dbplyr::window_order(desc(date)) %>% 
          filter(row_number()==1) %>% pull() %>% -10
      }
      
      if(is.null(end)){end <- today()}
      
      info <- self$read('idx_info')
      
      if(!is.null(item)){info <- info %>% filter(item_name == item)}
      
      df <- info %>% 
        filter(site=='ecos') %>%
        select(stat_code, item_code, cycle, index) %>%
        pmap_dfr(
          ~tryCatch({
            statSearch(stat_code = ..1,
                       item_code1 = ..2,
                       cycle = ..3,
                       start_time = strftime(start,"%Y%m%d"),
                       end_time = strftime(end,"%Y%m%d")
            ) %>% 
              as_tibble() %>% 
              transmute(date = ymd(time), index = ..4, value = data_value)
          }, error=function(e){
            tibble()
          }
          )
        )
      
      get_code <- list('fred' = c('economic.data', 'price'),
                       'yahoo' = c("stock.prices", 'close'))
      
      
      df2 <- info %>% 
        filter(site!='ecos') %>%
        select(item_code, site, index) %>% 
        pmap_dfr(
          ~tryCatch({
            tq_get(..1, 
                   get = get_code[[..2]][1], 
                   from = start,
                   to = end,
            ) %>% 
              as_tibble() %>% 
              transmute(date = ymd(date), index = ..3, value= .data[[get_code[[..2]][2]]])
          }, error=function(e){
            tibble()
          }
          )
        )
      
      bind_rows(df,df2) %>% 
        filter(!is.na(value))
    },
    
    ##3. 수집데이터 DB적재 ====
    upsert_econ = function(record, table){
      dbxUpsert(self$con, table, record, c("date", "index"))
    },
    
    ##4. DB 일별데이터 조회 ====
    query_daily = function(stats, start=NULL, end=NULL){
      
      if(is.null(start)){start <- '2000-01-01'}
      if(is.null(end)){end <- today()}
      
      self$read_obj('econ_daily') %>% 
        filter(between(date, start, end)) %>% 
        right_join(
          self$read_obj('idx_info') %>% 
            filter(item_name %in% stats) %>% 
            select(index, item_name),
          by='index'
        ) %>% 
        select(date, item_name, value) %>% 
        collect()
    },
    
    ##5. 시계열 그래프 그리기 ====
    plot_time_series = function(items, start=NULL, end=NULL){
      
      if(is.null(start)){start <- '2000-01-01'}
      if(is.null(end)){end <- today()}
      
      self$query_daily(items) %>% 
        filter(between_time(date, start, end)) %>% 
        group_by(item_name) %>% 
        plot_ly(x = ~date, y = ~value, color=~item_name, colors = RColorBrewer::brewer.pal(3, "Set2")) %>% 
        add_lines() %>%
        layout(showlegend = T, 
               xaxis = list(
                 tickformat = "%Y-%m",
                 rangeselector=list(
                 buttons=list(
                   list(count=1, label="YTD", step="year", stepmode="todate"),
                   list(count=3, label="3m", step="month", stepmode="backward"),
                   list(count=1, label="1y", step="year", stepmode="backward"),
                   list(count=5, label="5y", step="year", stepmode="backward"),
                   list(count=10, label="10y", step="year", stepmode="backward")
                 ))))
    }
  )
)

