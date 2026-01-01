library(tidyverse)
library(timetk)
library(R6)
library(glue)
library(httr)
library(jsonlite)
library(rvest)
library(RPostgres)
library(dbx)
library(tidyquant)
library(pool)
library(data.table)

get_config <- function(){
  yaml::read_yaml(file = 'secrets/config.yaml', 
                  readLines.warn = F)
}

get_exchange_rate <- function(cur='달러'){
  
  num <- c('달러'= 1, '엔'= 2, '유로'=3, '위안'=4)
  suppressWarnings({
    (read_html("http://finance.naver.com/marketindex/") %>% 
       html_nodes("div.head_info > span.value")
    )[num[cur]] %>%
      html_text() %>% 
      readr::parse_number()
  })
}




#[클래스] MyData ====
MyData <- R6Class(
  
  classname = 'MyData',
  public=list(
    
    con=NULL, config=NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(pw){
      
      # self$con <- dbConnect(SQLite(), file, bigint = 'numeric',
      #                       extended_types=T)
      cfg <- yaml::read_yaml(file = 'ccc.yaml',
                             readLines.warn = F)
      
      
      self$con <- dbPool(
        drv = RPostgres::Postgres(),
        host = cfg$c,
        port = 5432,
        dbname = "postgres",
        user = cfg$a,
        password = pw
      )
      
      self$config <- self$read('config')
      
    },
    
    ##2.(메서드) 테이블 추가 ====
    add_table = function(table, name){
      dbWriteTable(self$con, name, table, overwrite=T)
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
  ),
  
  private= list(
    ## 7. (메서드) 소멸자: 객체가 삭제될 때 DB 연결 종료 ====
    finalize = function() {
      if (!is.null(self$con)) {
        # poolClose로 안전하게 연결 해제
        pool::poolClose(self$con)
      }
    }
  )
)


#[클래스] KrxStocks ====
KrxStocks <- R6Class(
  
  classname = 'KrxStocks',
  inherit = MyData,
  
  public=list(
    
    user.agent = NULL, referer = NULL,
    stock_list = NULL, last_wd = NULL,
    this_wd = NULL, pw=NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(pw, date=NULL){
      
      self$pw <- pw
      super$initialize(self$pw)

      self$user.agent <- 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36'
      self$referer <- 'http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201'
            
      # (1) 기준 날짜 설정
      
      if(is.null(date)){
        if(now(tzone = "Asia/Seoul") >= update(now(tzone = "Asia/Seoul"), 
                                               hour=9, minute=21, second=0)){
          target_date <- today()
        }
        else {
          target_date <- today() - 1
        }
      } else {
        target_date <- as.Date(date)
      }

      target_year <- year(target_date)
      last_year <- target_year - 1
      
      # (2) DB에서 휴일 정보 읽기 (workdays 테이블)
      
      tryCatch({
        db_dates <- self$read('workdays') %>% arrange(기준일)
      }, error = function(e){
        # 테이블이 없거나 읽기 실패 시 빈 tibble 처리
        db_dates <- tibble(기준일 = as.Date(character()))
      })
      
      # (3) 시나리오별 갱신 로직
      need_refetch_db <- FALSE
      
      if(nrow(db_dates) == 0){
        # Case A: DB가 비어있음 -> 작년, 올해 데이터 모두 크롤링 및 저장
        message("DB 비어있음: 2년치(작년+올해) 영업일 정보 초기화 중...")
        days_prev <- self$scrape_workdays(last_year)
        days_curr <- self$scrape_workdays(target_year)
        
        # DB 저장 (bind_rows 후 저장)
        self$update_workdays_db(bind_rows(days_prev, days_curr), clear_all = TRUE)
        need_refetch_db <- TRUE
        
      } else {
        max_db_date <- max(db_dates$기준일)
        max_db_year <- year(max_db_date)
        
        if(target_year > max_db_year){
          # Case B: 해가 바뀌어 DB에 올해 정보가 없음 -> 올해 정보 크롤링 후 추가
          message(glue("새해({target_year}) 영업일 정보 갱신 중..."))
          days_curr <- self$scrape_workdays(target_year)
          self$update_workdays_db(days_curr, clear_year = target_year) # 해당 연도 덮어쓰기
          need_refetch_db <- TRUE
        }
      }
      
      # DB가 갱신되었다면 다시 읽기
      if(need_refetch_db) {
        db_dates <- self$read('workdays') %>% arrange(기준일)
      }
      
      self$this_wd <- db_dates
      
      # (4) 직전 영업일 계산
      # target_date보다 작거나 같은 날짜 중 가장 최신 날짜
      # 만약 target_date가 1월 1일이라 올해 데이터에 없다면, 작년 데이터에서 찾아짐
      
      self$last_wd <- self$this_wd %>% 
        filter(기준일 <= target_date) %>% 
        pull(기준일) %>% last()
      
      # (5) 주가 정보 요청
      self$get_stock_list2()
      
    },
    ## 2. DB 업데이트 헬퍼 메서드 ====
    update_workdays_db = function(new_data, clear_all=FALSE, clear_year=NULL){
      # new_data: tibble(기준일 = Date)
      
      if(clear_all){
        # 전체 삭제 후 삽입
        dbExecute(self$con, "TRUNCATE TABLE workdays")
      } else if(!is.null(clear_year)){
        # 해당 연도만 삭제 후 삽입 (중복 방지 및 갱신)
        dbExecute(self$con, glue("DELETE FROM workdays WHERE EXTRACT(YEAR FROM 기준일) = {clear_year}"))
      }
      
      # 데이터 삽입
      dbWriteTable(self$con, "workdays", new_data, append = TRUE, row.names = FALSE)
    },
    
    ##3.(메서드) KRX POST ====
    post_krx = function(site, params){
      
      url <- list(
        data='http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd',
        open='http://open.krx.co.kr/contents/OPN/99/OPN99000001.jspx')
      
      
      tryCatch({
        res <- POST(url=url[site],
                    query=params, 
                    user_agent(self$user.agent), 
                    add_headers(referer=self$referer)) %>% 
          content('t') %>% 
          jsonlite::fromJSON()
        
        if(length(res) > 0 && !is.null(names(res))){
          return(res[[ names(res)[1] ]] %>% as_tibble())
        } else { 
          return(tibble()) 
        }
      }, error = function(e){ return(tibble()) })
    },
    
    ##4.(메서드) 영업일 얻기 ====
    scrape_workdays = function(year){
      
      unix_time <- 
        (as.numeric(Sys.time()) * 1000) %>% round() %>% as.character()
      
      otp_params <- list(
        bld = 'MKD/01/0110/01100305/mkd01100305_01',
        name = 'form',
        '_' = unix_time)
      
      tryCatch({
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
        
        return(tibble(기준일=bizdays::bizseq(start, end, cal)))
      }, error = function(e){
        # 스크래핑 실패 시 비상용 (주말만 제외)
        start <- as.Date(glue('{year}-01-01')); end <- as.Date(glue('{year}-12-31'))
        days <- seq(start, end, by="day")
        return(tibble(기준일 = days[!wday(days) %in% c(1, 7)]))
      })
    },
    
    ##5.(메서드) 주식 종목(ETF포함) 리스트 얻기====
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
      
      params3 <- list(bld = "dbms/MDC/STAT/standard/MDCSTAT14901",
                      mktId = "ALL",
                      trdDd = yyyymmdd)
      
      suppressWarnings({
        
        # [1단계] 주식 데이터 요청 및 검증
        stock_df <- self$post_krx('data', params1)
        if(nrow(stock_df) == 0 || !("ISU_SRT_CD" %in% names(stock_df))){
          return(tibble()) 
        } 
        
        # [2단계] ETF/ETN 요청
        etf_df <- self$post_krx('data', params2)
        
        if(nrow(etf_df) > 0 && !("MKT_ID" %in% names(etf_df))){
          etf_df <- etf_df %>% mutate(MKT_ID='STK')
        } else {
          etf_df <- tibble()
        }
        
        gold_df <- self$post_krx('data', params3)
        
        if(nrow(gold_df) == 0) {
          gold_df <- tibble()
        }
        
        # [3단계] 병합
        bind_rows(stock_df, etf_df, gold_df) %>% 
          select(종목코드=ISU_SRT_CD, 종목명=ISU_ABBRV, 종가=TDD_CLSPRC, 시장구분=MKT_ID) %>% 
          mutate(시장구분=case_match(시장구분, 
                                 'STK'~'코스피',
                                 'KSQ'~'코스닥',
                                 'KNX'~'코넥스',
                                 'CMD'~'금현물'),
                 종가 = readr::parse_number(종가)
          ) %>% filter(str_ends(종목코드,'0'))
      })
    },
    
    get_stock_list2 = function(){
      
      # 앱 구동 후 시간이 흘러 9시 21분을 넘긴 경우를 대비함
      if(now(tzone = "Asia/Seoul") >= update(now(tzone = "Asia/Seoul"), 
                                             hour=9, minute=21, second=0)){
        target_date <- today()
      } else {
        target_date <- today() - 1
      }
      
      # initialize에서 생성된 this_wd(영업일 달력)를 이용해 다시 계산
      if(!is.null(self$this_wd) && nrow(self$this_wd) > 0){
        self$last_wd <- self$this_wd %>% 
          filter(기준일 <= target_date) %>% 
          pull(기준일) %>% 
          last()
      }
      
      # 주가 정보 요청
      stock_list <- self$get_stock_list(self$last_wd)
      
      # 주가 정보 오류(0건) 시 -> 임시공휴일 가능성 -> 강제 갱신
      if(nrow(stock_list) == 0){
        message("주가 데이터 없음(임시공휴일 등 감지) -> 영업일 정보 강제 갱신 및 재시도")
        
        target_year <- year(target_date)
        
        # 올해 영업일 다시 긁어오기
        days_curr_retry <- self$scrape_workdays(target_year)
        self$update_workdays_db(days_curr_retry, clear_year = target_year)
        
        # DB 다시 읽기
        self$this_wd <- self$read('workdays') %>% arrange(기준일)
        
        # 영업일 재계산
        self$last_wd <- self$this_wd %>% 
          filter(기준일 <= target_date) %>% 
          pull(기준일) %>% last()
        
        # 주가 재요청
        stock_list <- self$get_stock_list(self$last_wd)
      }
      self$stock_list <- stock_list
    },
    
    
    ##7.(메서드) 종목코드 찾기 ====
    find_code = function(name=NULL, mkt=NULL){
      
      if(is.null(self$stock_list) || nrow(self$stock_list) == 0) return(tibble())
      
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
  inherit = MyData,
  public=list(
    
    ## 속성 선언 ====
    token_tmp=NULL, APP_KEY=NULL, APP_SECRET=NULL, ACCT=NULL, 
    URL_BASE=NULL, MY_AGENT=NULL, base_headers=NULL,
    token_headers=NULL,
    
    ## 속성 초기화 ====
    initialize = function(pw, account="my"){
      super$initialize(pw)
      cfg <- split(self$config$value, self$config$token)
      
      # self$token_tmp <- paste0("secrets/KIS", account)
      self$token_tmp <- paste0("KIS", account)
      # 
      # if (!file.exists(self$token_tmp)) {
      #   file.create(self$token_tmp)
      # }
      
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
        as.POSIXct(my_expired, format='%Y-%m-%d %H:%M:%S', tz='Asia/Seoul')
      
      
      dbWriteTable(self$con, self$token_tmp, 
                     tibble(token = my_token,
                            valid_date = format(valid_date, '%Y-%m-%d %H:%M:%S')),
                   overwrite = TRUE)
      
      # writeLines(c(paste('token:', my_token),
      #              paste('valid-date:', 
      #                    format(valid_date, '%Y-%m-%d %H:%M:%S'))),
      #            self$token_tmp)
    },
    
    ##메서드(2) - 토큰 불러오기 ====
    read_token = function() {
      tryCatch({
        # 토큰이 저장된 파일 읽기
        # tkg_tmp <- yaml::read_yaml(self$token_tmp, fileEncoding = 'UTF-8')
        
        tkg_tmp <- self$read(self$token_tmp)
        
        # 토큰 만료 일,시간
        exp_dt <- as.POSIXct(tkg_tmp$valid_date, format='%Y-%m-%d %H:%M:%S', tz='Asia/Seoul')
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
    today = NULL, year = NULL, days = NULL,
    assets = NULL, pension = NULL, ex_usd = NULL, ex_jpy = NULL, 
    bs_pl_mkt_a = NULL, bs_pl_mkt_p = NULL,
    bl = NULL, my = NULL, ks = NULL,
    t_class=NULL, t_comm=NULL, t_comm2=NULL,
    t_comm3=NULL, t_comm4=NULL, t_comm5=NULL,pw=NULL, comm_profit=NULL,
    inflow=NULL, book_info=NULL,bs_pl_a=NULL, bs_pl_p=NULL,
    assets_last_num=NULL, assets_daily_last_num=NULL,
    pension_last_num=NULL, pension_daily_last_num=NULL, 
    inflow_last_num=NULL,cash_in_out=NULL, acct_order=NULL,
    cur_order=NULL, class_order=NULL, class2_order=NULL,
    class3_order=NULL, t_allocation=NULL, account_allocation=NULL,
    y_num=NULL, grid=NULL, future_eval=NULL,
    
    
    ## 1. 속성 초기화====
    initialize = function(pw) {
      
      self$pw <- pw
      # self$con <- dbConnect(SQLite(), 'mydata.sqlite', bigint = 'numeric',
      #                       extended_types=T)
      super$initialize(self$pw)
      self$today <- today()
      self$year <- year(self$today)
      
      tryCatch({
        if(dbExistsTable(self$con, 'inflow') & dbExistsTable(self$con, 'eval_profit')){
          dbExecute(self$con, glue("DELETE FROM inflow WHERE \"거래일자\" < '{self$today}'"))
          dbExecute(self$con, glue("DELETE FROM eval_profit WHERE \"연도\" = '{self$year}'"))
          dbExecute(self$con, glue("DELETE FROM return WHERE \"기준일\" = '{self$today}'"))
        }
      }, error = function(e) {
        # 테이블이 없거나 권한 문제 등 에러 발생 시 무시하고 진행
      })

      self$days <- seq(make_date(2024,1,1), 
                       self$today,
                       by='day')
      suppressWarnings({
        self$ks <- KrxStocks$new(self$pw)
        self$ks$get_stock_list2()
      })
      
      self$acct_order <- c("한투","불리오","엔투하영","금현물", 
                           "한투ISA","엔투ISA","엔투저축연금",
                           "한투연금저축", "미래DC", "농협IRP","엔투IRP")
      self$cur_order <- c("원화", "달러", "엔화")
      self$class_order <- c("<합계>", '', "주식", "대체자산", 
                            "채권", "현금성", "외화자산")
      self$class2_order <- c("","선진국","신흥국","상품","부동산인프라",
                             "국채","투자등급","하이일드","만기무위험",
                             "만기회사채","금융상품","현금","달러자산",
                             "엔화자산")
      self$class3_order <- c("","인덱스","종목","테마","원자재","에너지",
                             "부동산","인프라","선진국","신흥국","단기ETF",
                             "원화상품","외환","원화")
      
    },
    
    ## 2.(공통) 주가 업데이트====
    update_new_price = function(){
      self$ks$get_stock_list2()
    },
    
    ## 3.(거래내역) 거래내역 기록 테이블====
    #   - '거래내역' 화면에 들어갈 테이블 산출(회계/계좌/통화별)
    get_trading_record = function(table, acct, cur, limit_n){
      
      if(table=="투자자산"){
        table_name <- 'assets'
      } else {
        table_name <- 'pension'
      }
      
      if(table=="투자자산"){table <- 'assets'}
      else {table <- 'pension'}
      
      suppressWarnings({
        df1 <- self$read_obj(table)
        df2 <- self$read_obj(paste0(table,'_daily'))
      })
      
      df2 %>% left_join(
        (df1 %>% transmute(계좌, 통화, 종목코드, 종목명)), 
        by = c('계좌','종목코드')) %>% 
        filter(계좌==acct, 통화==cur) %>% 
        dbplyr::window_order(거래일자, 행번호) %>% 
        transmute(
          행번호, 계좌, 통화, 거래일자, 종목명, 
          매입수량, 매입액, 현금지출,
          매입비용 = 현금지출-매입액,
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
    
    ## 4.(장부금액) 계좌거래 내역 전처리 ====
    #   - 회계/계좌/통화별로 각각 기록된 거래내역들을 통합하여
    #   - 모든 일자에 대한 통합된 거래기록을 산출
    #   - 잔액/손익 테이블(bs_pl)을 만들기 위한 전단계
    
    get_daily_trading = function(ast_info, trade){
      
      dt_info <- as.data.table(ast_info)
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
    
    
    ## 5.(장부금액)운용자산 잔액-손익 테이블 생성====
    #   - 거래내역에 기초해 모든 시점의 장부금액/평잔/손익을 산출
    get_bs_pl = function(mode = 'assets', trade_tbl) {
      
      
      trade <- copy(trade_tbl)
        
      if(mode == 'assets') {
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
                         수익=수익_누적, 
                         비용=비용_누적, 
                         실현손익=실현손익_누적, 
                         보유수량, 장부금액, 평잔)]
      bs_pl <- merge(bs_pl, 
                     codes[, .(계좌, 종목코드, 자산군, 세부자산군, 
                               세부자산군2)], 
                     by = c('계좌','종목코드'), 
                     all.x = TRUE)
      
      setkey(bs_pl, 계좌, 종목코드, 거래일자)
      
      # 예수금 & 평잔 처리 (dcast 사용)
      if(mode=='assets'){
        calc_cash_flow <- function(target_cur) {
          cash_dt <- trade[통화 == target_cur, 
                           .(현금 = sum(현금수입 + 입출금 - 현금지출)), 
                           by = .(거래일자, 계좌)]
          
          if(nrow(cash_dt) > 0) {
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
        
        if(!is.null(cw)) {
          map_w <- list('엔투ISA예수금'='엔투ISA',
                        '한투예수금'='한투',
                        '한투ISA예수금'='한투ISA',
                        '금현물계좌현금'='금현물')
          for(nm in names(map_w)) {
            if(map_w[[nm]] %in% names(cw$b)) {
              bs_pl[종목명 == nm, 
                    장부금액 := cw$b[.SD, get(map_w[[nm]]), on="거래일자"]]
              bs_pl[종목명 == nm, 평잔 := cw$e[.SD, get(map_w[[nm]]), on="거래일자"]]
            }
          }
        }
        
        if(!is.null(cd)) {
          map_d <- list('불리오달러'='불리오',
                        '직접운용달러'='한투')
          for(nm in names(map_d)) {
            if(map_d[[nm]] %in% names(cd$b)) {
              bs_pl[종목명 == nm, 장부금액 := cd$b[.SD, get(map_d[[nm]]), on="거래일자"]]
              bs_pl[종목명 == nm, 평잔 := cd$e[.SD, get(map_d[[nm]]), on="거래일자"]]
            }
          }
        }
        
        if(!is.null(cy) && '한투' %in% names(cy$b)) {
          bs_pl[종목명 == '직접운용엔', 장부금액 := cy$b[.SD, 한투, on="거래일자"]]
          bs_pl[종목명 == '직접운용엔', 평잔 := cy$e[.SD, 한투, on="거래일자"]]
        }
      }
      
      return(bs_pl)
      
    },
    
    ## 6.(장부금액) 장부금액 자료 산출====
    run_book = function(){
      
      #DB 원자료 캐싱
      self$assets <- self$read('assets')
      self$pension <- self$read('pension')
      self$inflow <- self$read('inflow')
      
      #DB 행번호
      self$assets_last_num <- self$read_obj('assets') %>% 
        arrange(desc(행번호)) %>% first() %>% pull(행번호)
      self$assets_daily_last_num <- self$read_obj('assets_daily') %>% 
        arrange(desc(행번호)) %>% first() %>% pull(행번호)
      
      self$pension_last_num <- self$read_obj('pension') %>% 
        arrange(desc(행번호)) %>% first() %>% pull(행번호)
      self$pension_daily_last_num <- self$read_obj('pension_daily') %>% 
        arrange(desc(행번호)) %>% first() %>% pull(행번호)
      
      self$inflow_last_num <- self$read_obj('inflow') %>% 
        arrange(desc(행번호)) %>% first() %>% pull(행번호)
      
      
      
      
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
        group_by(거래일자) %>% 
        summarise(입출금=sum(입출금)) %>% 
        rename(기준일=거래일자)
      
      
      
      self$bs_pl_a <- bs_pl_book_a[거래일자 == self$today]
      self$bs_pl_p <- bs_pl_book_p[거래일자 == self$today]
      
      
      self$book_info <- bs_pl_book_a %>% as_tibble() %>% 
        filter(통화=='원화') %>% 
        filter((month(거래일자)==12 & day(거래일자)==31) | 
                 거래일자==self$today) %>% 
        bind_rows(
          bs_pl_book_p %>% as_tibble() %>% 
            filter((month(거래일자)==12 & day(거래일자)==31) | 
                     거래일자==self$today)
        ) %>% 
        group_by(연도=year(거래일자)) %>% 
        summarise(장부금액=sum(장부금액), 평잔=sum(평잔), 
                  실현손익=sum(실현손익))
      
      
    },
    
    
    ## 7.(평가금액)투자자산 평가반영 잔액-손익 테이블 생성====
    evaluate_bs_pl_assets = function() {
      
      # if (is.null(self$bl) && is.null(self$my)) {
      if (is.null(self$bl)) {
        self$bl <- AutoInvest$new(self$pw, 'boolio')
        # self$my <- AutoInvest$new('my')
      }
      
      
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
        bind_rows(
          # mutate(self$my$inquire_balance(), 계좌 = '한투'),
          # mutate(self$my$inquire_balance_ovs(), 계좌 = '한투'),
          # mutate(self$my$inquire_balance_ovs('JPY'), 계좌 = '한투'),
          mutate(self$bl$inquire_balance_ovs(), 계좌 = '불리오')
        ) %>%
        select(계좌, 종목코드,평가금액) %>% as.data.table()
      
      
      bs_pl <- self$bs_pl_a
      
      closing_prices <- as.data.table(self$ks$stock_list)[, .(종목코드, 종가)]
      last_eval <- self$assets %>% 
        select(계좌, 종목코드, 기초평가손익) %>% 
        as.data.table()
      
      bs_pl <- merge(bs_pl, price, by=c("계좌","종목코드"), all.x=TRUE)
      bs_pl <- merge(bs_pl, closing_prices, by='종목코드', all.x=TRUE)
      bs_pl <- merge(bs_pl, last_eval, by=c("계좌","종목코드"), all.x=TRUE)
      
      bs_pl <- bs_pl[평잔 > 0.02]
      bs_pl[, 장부금액 := fifelse(장부금액 < 1, 0, 장부금액)]
      bs_pl[, 기초평가손익 := fifelse(is.na(기초평가손익), 0, 기초평가손익)]
      bs_pl[, 평가금액 := fcase(!is.na(평가금액), 평가금액, !is.na(종가), 종가 * 보유수량, rep(TRUE, .N), 장부금액)]
      
      dollar <- round(sum(bs_pl[통화 == '달러']$평가금액, na.rm=TRUE) * self$ex_usd)
      yen <- round(sum(bs_pl[통화 == '엔화']$평가금액, na.rm=TRUE) * self$ex_jpy)
      
      bs_pl[종목명 == '달러자산', 평가금액 := dollar]
      bs_pl[종목명 == '엔화자산', 평가금액 := yen]
      
      bs_pl[, `:=`(평가손익 = 평가금액 - 장부금액)]
      
      bs_pl[, `:=`(평가손익증감 = 평가손익- 기초평가손익)]
      bs_pl[, `:=`(총손익 = 실현손익 + 평가손익증감)]
      bs_pl <- bs_pl[order(-통화, -평가금액)]
      
      return(as_tibble(bs_pl))
    },
    
    ## 8.(장부금액)연금 평가반영 잔액-손익 테이블 생성========
    evaluate_bs_pl_pension = function(){
      
      price <- self$pension %>%
        select(계좌, 종목코드, 평가금액) %>% 
        filter(평가금액!=0) %>% as.data.table()
      
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
      
      bs_pl <- self$bs_pl_p
      closing_prices <- as.data.table(self$ks$stock_list)[, .(종목코드, 종가)]
      last_eval <- self$pension %>% 
        select(계좌, 종목코드, 기초평가손익) %>% 
        as.data.table()
      
      bs_pl <- merge(bs_pl, price, by=c("계좌","종목코드"), all.x=TRUE)
      bs_pl <- merge(bs_pl, closing_prices, by='종목코드', all.x=TRUE)
      bs_pl <- merge(bs_pl, last_eval, by=c("계좌","종목코드"), all.x=TRUE)
      
      bs_pl <- bs_pl[평잔 > 0.02]
      bs_pl[, 장부금액 := fifelse(장부금액 < 1, 0, 장부금액)]
      
      fund_codes <- bs_pl[(str_sub(종목코드, 1, 2) == 'K5')&(장부금액>0), 종목코드]
      if(length(fund_codes) > 0) {
        fund_prices <- data.table(종목코드 = fund_codes, 기준가 = get_fund_price(fund_codes))
        bs_pl <- merge(bs_pl, fund_prices, by='종목코드', all.x=TRUE)
      } else { bs_pl[, 기준가 := NA_real_] }
      
      bs_pl[, 기초평가손익 := fifelse(is.na(기초평가손익), 0, 기초평가손익)]
      bs_pl[, 평가금액 := fcase(!is.na(평가금액), 평가금액, !is.na(종가), 종가 * 보유수량, !is.na(기준가), 기준가/1000 * 보유수량, rep(TRUE, .N), 장부금액)]
      
      bs_pl[, `:=`(평가손익 = 평가금액 - 장부금액)]
      bs_pl[, `:=`(평가손익증감 = 평가손익-기초평가손익)]
      bs_pl[, `:=`(총손익 = 실현손익+평가손익증감)]
      
      
      bs_pl[order(-통화, -평가금액)]
      return(as_tibble(bs_pl))
    },
    
    ##9.(자산운용내역) 기간별 거래내역 생성====
    total_trading = function(dates){
      
      if(length(dates)==1){
        start = dates
        end = dates
      } else {
        start = dates[1]
        end = dates[2]
      }
      
      
      df1 <- self$assets %>% 
        bind_rows(self$pension)
      
      
      df2 <- self$read_obj('assets_daily') %>% 
        filter(between(거래일자, start, end)) %>% 
        collect() %>% 
        bind_rows(
          self$read_obj('pension_daily') %>% 
            filter(between(거래일자, start, end)) %>% 
            collect()
        )
      
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
    
    ##10.(배분현황) 자산군별/계좌별 손익현황 생성2
    # compute_total2 = function(){
    #   
    #   df <- self$bs_pl_mkt_a
    #   
    #   usd_bs <- round(filter(df, 통화=='달러')$장부금액 * self$ex_usd,0)
    #   jpy_bs <- round(filter(df, 통화=='엔화')$장부금액 * self$ex_jpy,0)
    #   usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)
    #   jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)
    #   usd_eqbal <- round(filter(df, 통화=='달러')$평잔 * self$ex_usd,0)
    #   jpy_eqbal <- round(filter(df, 통화=='엔화')$평잔 * self$ex_jpy,0)
    #   usd_rev <- round(filter(df, 통화=='달러')$수익 * self$ex_usd,0)
    #   jpy_rev <- round(filter(df, 통화=='엔화')$수익 * self$ex_jpy,0)
    #   usd_prof <- round(filter(df, 통화=='달러')$실현손익 * self$ex_usd,0)
    #   jpy_prof <- round(filter(df, 통화=='엔화')$실현손익 * self$ex_jpy,0)
    #   
    #   df2 <- df %>%
    #     mutate(
    #       장부금액 = replace(장부금액, 통화 == '달러', usd_bs),
    #       장부금액 = replace(장부금액, 통화 == '엔화', jpy_bs),
    #       평가금액 = replace(평가금액, 통화 == '달러', usd_eval),
    #       평가금액 = replace(평가금액, 통화 == '엔화', jpy_eval),
    #       평잔 = replace(평잔, 통화 == '달러', usd_eqbal),
    #       평잔 = replace(평잔, 통화 == '엔화', jpy_eqbal),
    #       수익 = replace(수익, 통화 == '달러', usd_rev),
    #       수익 = replace(수익, 통화 == '엔화', jpy_rev),
    #       실현손익 = replace(실현손익, 통화 == '달러', usd_prof),
    #       실현손익 = replace(실현손익, 통화 == '엔화', jpy_prof)) %>%  
    #     bind_rows(self$bs_pl_mkt_p) %>% 
    #     group_by(계좌, 자산군, 통화) %>% 
    #     summarise(across(c(장부금액, 평가금액, 평잔, 
    #                        수익, 실현손익), sum ), .groups = 'drop') %>% 
    #     mutate(평가손익 = 평가금액-장부금액,
    #            세전수익률 = 수익/평잔*100,
    #            세후수익률 = 실현손익/평잔*100,
    #            평가수익률 = 평가손익/장부금액*100) %>%
    #     mutate(계좌 = factor(계좌, 
    #                        levels = c("한투","불리오","엔투하영","금현물",
    #                                   "한투ISA","엔투ISA",
    #                                   "엔투저축연금","한투연금저축", 
    #                                   "미래DC", "농협IRP","엔투IRP","합계")),
    #            자산군 = factor(자산군, 
    #                         levels = c("","주식","대체자산",
    #                                    "채권","현금성", 
    #                                    "외화자산"))) %>% 
    #     ungroup()
    #   
    #   df3 <- df2 %>% 
    #     mutate(자산군="") %>% 
    #     group_by(계좌, 자산군, 통화) %>% 
    #     summarise(across(c(장부금액, 평가금액, 평잔, 
    #                        수익, 실현손익), sum ), .groups = 'drop') %>% 
    #     ungroup() %>% 
    #     mutate(평가손익 = 평가금액-장부금액,
    #            세전수익률 = 수익/평잔*100,
    #            세후수익률 = 실현손익/평잔*100,
    #            평가수익률 = 평가손익/장부금액*100)
    #   
    #   df4 <- df3 %>% filter(통화=='원화') %>% 
    #     summarise(계좌='합계',자산군='',통화='',
    #               across(장부금액:실현손익,sum)) %>% 
    #     mutate(평가손익 = 평가금액-장부금액,
    #            세전수익률 = 수익/평잔*100,
    #            세후수익률 = 실현손익/평잔*100,
    #            평가수익률 = 평가손익/장부금액*100)
    #   
    #   df5 <- df2 %>% 
    #     filter(통화=="원화") %>% 
    #     mutate(계좌="전체") %>% 
    #     group_by(계좌, 자산군, 통화) %>% 
    #     summarise(across(c(장부금액, 평가금액, 평잔, 
    #                        수익, 실현손익), sum ),
    #               .groups = 'drop') %>% 
    #     ungroup() %>% 
    #     bind_rows(
    #       df2 %>% 
    #         filter(통화!='원화') %>% mutate(계좌='전체') %>% 
    #         group_by(계좌, 자산군, 통화) %>% 
    #         summarise(across(c(장부금액, 평가금액, 평잔, 
    #                            수익, 실현손익), sum ), .groups = 'drop') %>% 
    #         ungroup()
    #     ) %>% 
    #     mutate(계좌='전체',
    #            평가손익 = 평가금액-장부금액,
    #            세전수익률 = 수익/평잔*100,
    #            세후수익률 = 실현손익/평잔*100,
    #            평가수익률 = 평가손익/장부금액*100) %>% 
    #     mutate(자산군 = factor(자산군, 
    #                         levels = c("","주식","대체자산",
    #                                    "채권","현금성", 
    #                                    "외화자산"))) %>% 
    #     arrange(자산군, desc(계좌))
    #   
    #   
    #   df6 <- bind_rows(df5, df4,
    #                    
    #                    ( bind_rows(df2, df3) %>%
    #                        mutate(계좌 = factor(계좌, 
    #                                           levels = c("한투","불리오","엔투하영","금현물",
    #                                                      "한투ISA","엔투ISA",
    #                                                      "엔투저축연금","한투연금저축", 
    #                                                      "미래DC", "농협IRP","엔투IRP","합계")),
    #                               자산군 = factor(자산군, 
    #                                            levels = c("","주식","대체자산",
    #                                                       "채권","현금성", 
    #                                                       "외화자산"))) %>% 
    #                        arrange(계좌, 자산군))) %>% 
    #     select(-장부금액,-수익)
    #   
    #   
    #   df7 <- df3 %>% group_by(계좌) %>% summarise(총평가=sum(평가금액)) %>% 
    #     mutate(총평가 = if_else(계좌=='한투', 
    #                          총평가-filter(df2,자산군=='외화자산')$평가금액, 총평가))
    #   
    #   df8 <- df6 %>% 
    #     left_join(df7, by='계좌') %>% 
    #     mutate(총평가=if_else(계좌=='전체'|계좌=='합계',
    #                        df4$평가금액,총평가),
    #            총평가=if_else(자산군=='외화자산',0,총평가),
    #            비중 = 평가금액/총평가*100, .after=3) %>% 
    #     select(-총평가) %>% 
    #     mutate(비중=if_else(자산군=='외화자산',0,비중),
    #            비중=if_else((계좌=='한투'&자산군==''),0,비중))
    #   
      
      # df9 <- self$read_obj('return') %>% 
      #   filter(year(기준일)==year(self$today)-1) %>% 
      #   filter(기준일==max(기준일, na.rm=T), 
      #          세부자산군=='', 세부자산군2=='') %>% 
      #   collect() %>% transmute(자산군=c('','대체자산','주식',
      #                                 '채권','현금성','외화자산'), 
      #                           통화=c('','원화','원화','원화','원화','원화'), 
      #                           전년평가손익=평가손익)
      # 
      # self$t_comm3 <- df8 %>% filter(계좌 %in% c("전체","합계")) %>% 
      #   left_join(df9, by=c('자산군','통화')) %>% 
      #   mutate(평가손익증감=평가손익-전년평가손익, 
      #          총손익 = 실현손익+평가손익증감, .after='평가손익') %>% 
      #   select(-평가수익률, -평가손익, -전년평가손익) %>% 
      #   mutate(총수익률=총손익/평잔*100)
      
    #   self$t_comm4 <- df8 %>% filter(!(계좌 %in% c("전체","합계")))
    #   
    # },
    
    ##10.(보유현황) 자산군별/상품별 보유현황 생성====
    compute_total = function(){
      df <- self$bs_pl_mkt_a
      usd_bs <- round(filter(df, 통화=='달러')$장부금액 * self$ex_usd,0)
      jpy_bs <- round(filter(df, 통화=='엔화')$장부금액 * self$ex_jpy,0)
      usd_eval <- round(filter(df, 통화=='달러')$평가금액 * self$ex_usd,0)
      jpy_eval <- round(filter(df, 통화=='엔화')$평가금액 * self$ex_jpy,0)

      # 1)계좌/자산군/상품까지 원재료
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
            filter(장부금액!=0) %>%
            group_by(계좌, 종목코드) %>%
            summarise(
              보유수량 = sum(보유수량),
              장부금액 = sum(장부금액),
              평가금액=sum(평가금액),.groups = 'drop'),
          by=c('계좌','종목코드'))

      df00 %>% 
        filter(통화=='원화') %>% 
        transmute(
          연도=self$year,계좌,종목코드,
          평가손익=평가금액-장부금액
        ) %>% 
        filter(평가손익!=0) %>% 
        self$upsert('eval_profit',c('연도','계좌','종목코드'))
      
      # 1)계좌없는 자산군~상품까지
      
      df0 <- df00 %>%
        group_by(종목코드) %>%
        summarise(통화=last(통화),
                  자산군=last(자산군),
                  세부자산군=last(세부자산군),
                  세부자산군2=last(세부자산군2),
                  상품명 = last(상품명),
                  보유수량=sum(보유수량),
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액),
                  .groups = 'drop') %>%
        select(-종목코드)

      # 3) 합계
      df2 <- df0 %>%
        filter(통화=='원화') %>%
        select(-통화) %>%
        summarise(자산군="<합계>", 세부자산군 = '',
                  세부자산군2 = '',상품명 = '', 보유수량=0,
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액), .groups = 'drop')

      # 4) 상품까지(외화빼고)
      df1 <- df0 %>%
        select(-통화) %>%
        filter(자산군!="외화자산")

      # 5) 자산군 소계
      df3 <- df1 %>%
        group_by(자산군) %>%
        summarise(세부자산군='', 세부자산군2 = '', 상품명 = '', 보유수량=0,
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액))

      # 6) 세부자산군 소계
      df4 <- df1 %>%
        group_by(자산군, 세부자산군) %>%
        summarise(세부자산군2 = '', 상품명 = '', 보유수량=0,
                  장부금액=sum(장부금액),
                  평가금액=sum(평가금액), .groups = 'drop')

      # 7) 세부자산군2 소계
      df5 <- df1 %>%
        group_by(자산군, 세부자산군, 세부자산군2) %>%
        summarise(상품명 = "", 보유수량=0, 장부금액=sum(장부금액), 평가금액=sum(평가금액),
                  .groups = 'drop')

      # 8) 환차손익 계산
      df6 <- tibble_row(
        자산군='환차손익', 세부자산군='', 세부자산군2 = '',
        보유수량=0,
        평가금액=0,
        평가손익= (sum(df3$장부금액) - df2$장부금액),
        평가수익률 = round(평가손익/df2$장부금액*100,2)
      )

      
      #합치기기(df2,3,4,5,6)
      df7 <- bind_rows(df2,df3,df4,df5) %>%
        select(-보유수량) %>%
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액,0),
          평가수익률 = round(평가손익 / 장부금액 * 100,2)
        ) %>%
        select(!c(상품명, 장부금액)) %>%
        bind_rows(df6 %>%
                    select(-보유수량))

      #DB 업로드
      # df7 %>%
      #   mutate(기준일=self$today, .before=1) %>%
      #   self$upsert('return', c('기준일','자산군','세부자산군','세부자산군2'))

      # 
      # dates <- self$read_obj('return') %>%
      #   distinct(기준일) %>%
      #   arrange(desc(기준일)) %>%
      #   pull()
      # 
      # start <- dates[1]
      # end <- dates[2]
      # 
      # #전일대비손익
      # df8 <- self$read_obj('return') %>%
      #   filter(기준일 %in% c(end, start)) %>%
      #   collect() %>%
      #   select(-평가금액,-평가수익률) %>%
      #   spread(key=기준일, value=평가손익, drop=T) %>%
      #   rename_with(~c("전일","당일"), .cols=4:5) %>%
      #   mutate(`전일대비(손익)` = .[[5]] - .[[4]]) %>%
      #   select(!전일:당일)
      # 
      # #전일대비수익률
      # df9 <- self$read_obj('return') %>%
      #   filter(기준일 %in% c(end, start)) %>%
      #   collect() %>%
      #   select(-평가금액,-평가손익) %>%
      #   spread(key=기준일, value=평가수익률, drop=T) %>%
      #   rename_with(~c("전일","당일"), .cols=4:5) %>%
      #   transmute(`전일대비(수익률)` = .[[5]] - .[[4]])
      # 
      # #전일대비테이블 최종
      # self$t_class <-
      #   df7 %>%
      #   left_join(
      #     df8 %>% bind_cols(df9),
      #     by=c('자산군','세부자산군','세부자산군2')
      #   )

      #상품별 보유현황테이블1 최종
      self$t_comm <- bind_rows(df1,df2,df3,df4,df5) %>%
        mutate(자산군 = factor(자산군, levels = self$class_order),
               세부자산군 = factor(세부자산군, levels = self$class2_order),
               세부자산군2 = factor(세부자산군2, levels = self$class3_order)
        ) %>% 
        arrange(자산군, 세부자산군, 세부자산군2, desc(평가금액), 상품명) %>%
        mutate(
          평가손익 = round(평가금액 - 장부금액,0),
          평가수익률 = round(평가손익 / 장부금액 * 100,2)
        ) %>%
        select(-장부금액) %>% 
        bind_rows(df6)

      #상품별/계좌별 보유현황테이블2 최종
      
      self$t_comm2 <- df00 %>%
        select(계좌, 자산군, 세부자산군, 세부자산군2, 통화, 상품명, 보유수량,
               장부금액, 평가금액) %>%
        bind_rows(
          df00  %>% 
            filter(자산군!='외화자산') %>%  
            group_by(계좌) %>% 
            summarise(
              자산군='', 세부자산군='', 세부자산군2='',
              상품명='', 보유수량=0, 장부금액=sum(장부금액),
              평가금액=sum(평가금액))
        ) %>% 
        mutate(
          평가손익 = round(평가금액 - 장부금액,0),
          평가수익률 = round(평가손익 / 장부금액 * 100,2),
          계좌 = factor(계좌, levels = self$acct_order),
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order)) %>%
        select(-장부금액) %>% 
        arrange(계좌, 자산군, 세부자산군, 세부자산군2, 
                desc(평가수익률), 상품명) %>% 
        filter(자산군!='외화자산')
    },
    
    
    ##11.(배분현황) 자산배분 생성====
    compute_total_allocation = function(){
      df1 <- self$t_comm %>% 
        filter(상품명=='') %>% 
        select(자산군, 세부자산군, 세부자산군2, 평가금액)
      
      df2 <- df1 %>% 
        mutate(비중 = 평가금액/df1$평가금액[1]*100) %>% 
        full_join(
          self$read('allocation') %>% 
            add_row(자산군='현금성', 세부자산군="", 
                    목표1 = 100-sum(.$목표1, na.rm = T)) %>% 
            mutate(세부자산군2 = '', .after=2),
          by=c('자산군','세부자산군', '세부자산군2')
        ) %>% 
        mutate(
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
          평가금액 = if_else(is.na(평가금액),0, 평가금액)
        ) %>% 
        arrange(자산군, 세부자산군, 세부자산군2)
      
      
      self$t_allocation <- df2 %>% mutate(
        목표금액 = na_if(df1$평가금액[[1]]*(coalesce(목표1,0)+coalesce(목표2,0))/100,0), 
        .before=목표1) %>% 
        mutate(과부족 = 평가금액-목표금액)
      
      self$account_allocation <- self$t_comm2 %>%
        select(계좌, 자산군, 평가금액) %>%
        mutate(자산군 = if_else(자산군 == "" | is.na(자산군), 
                             "합계", as.character(자산군))) %>% 
        group_by(계좌, 자산군) %>% 
        summarise(평가금액=sum(평가금액),.groups = 'drop') %>%
        pivot_wider(names_from = 자산군, values_from = 평가금액) %>% 
        select(계좌, 주식, 대체자산, 채권, 현금성, 합계)%>%
        mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE))
    },
    
    
    ##12-1.(손익현황) 종합손익 그래프 생성====
    plot_total_profit = function(start, end){
      df <- self$read_obj('return') %>% 
        filter(자산군=='<합계>') %>% 
        collect() %>% 
        transmute(기준일=as.Date(기준일),평가금액) %>% 
        filter(기준일>=start, 기준일<=end) %>% 
        arrange(기준일)
      
      df %>% full_join(
        self$cash_in_out %>% 
          filter(기준일 %>% between(first(df$기준일),last(df$기준일))),
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
    },
    
    ##12-2.(손익현황) 평가금액 추이 그래프 생성====
    plot_total_eval = function(){
      days1 <- self$today %m-% years(1)
      
      df2 <- self$read_obj('return') %>% 
        filter(자산군=='<합계>') %>% 
        transmute(기준일=as.Date(기준일),평가금액=평가금액/10000) %>% 
        filter(기준일>=days1) %>% 
        arrange(기준일) %>% 
        collect() %>% 
        mutate(구분='실선')
      
      
      df3 <- tibble(기준일=seq(self$today+1,self$today %m+% years(1),1)) %>% 
        left_join(
          self$inflow %>% 
            transmute(기준일=거래일자, 자금유출입=자금유출입/10000) %>% 
            group_by(기준일) %>% 
            summarise(평가금액=sum(자금유출입)),
          by='기준일'
        ) %>% 
        mutate(평가금액=if_else(is.na(평가금액),0,평가금액),
               평가금액=if_else(기준일==self$today+1, 
                            평가금액+last(df2$평가금액),
                            평가금액),
               평가금액=cumsum(평가금액)) %>% 
        mutate(구분='점선')
      
      bind_rows(df2,df3) %>% 
        ggplot(aes(x=기준일, y=평가금액)) +
        geom_line(aes(group = 1), color = "grey80", alpha = 0.5) +
        geom_line(aes(linetype = 구분, color = 구분), linewidth = 1) +
        scale_linetype_manual(
          values = c("실선" = "solid", "점선" = "dashed")) +
        scale_color_manual(values = 
                             c("실선" = "steelblue", "점선" = "firebrick")) +
        scale_y_continuous(
          breaks = function(x){seq(
            floor(x[1] / 5000) * 5000,  # 최소값 단위
            ceiling(x[2] / 5000) * 5000,  # 최대값 단위
            by = 5000
          )}, sec.axis = dup_axis(name=NULL)
        )+
        scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m") +
        theme(text=element_text(size=20),
              axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    ##13.(손익현황) 종합손익 테이블 생성====
    compute_t_profit = function(){
      tibble(연도 = 2023,
             장부금액 = 77986913,
             평잔 = 63019405,
             실현손익 = 2376343) %>% 
        bind_rows(self$book_info) %>% 
        left_join(
          self$read_obj('eval_profit') %>% 
            group_by(연도) %>% 
            summarise(평가손익=sum(평가손익, na.rm=T)) %>% 
            collect(),
          by='연도'
        ) %>% 
        left_join(
          self$read_obj('return') %>% 
            filter(자산군=='<합계>') %>% 
            filter((month(기준일)==12 & day(기준일)==31) | 
                     기준일==self$today) %>% 
            transmute(연도=year(기준일), 평가금액) %>%
            collect() %>% 
            add_row(연도=2023, 평가금액=78916405),
          by='연도'
        ) %>% 
        transmute(
          연도=as.character(연도),
          장부금액, 평잔, 평가금액, 평가손익, 실현손익,
          평가손익증감=if_else(연도==2023, first(평가손익),
                         diff_vec(평가손익,silent = T)),
          총손익 = 실현손익+평가손익증감,
          실현수익률 = 실현손익/평잔*100,
          평가증감률 = 평가손익증감/평잔*100,
          총수익률 = 실현수익률+평가증감률)
    },
    
    ##14.(손익현황) 손익변동 생성 ====
    compute_profit_var = function(){
      dates <- self$read_obj('return') %>%
        distinct(기준일) %>%
        pull()
      
      dates_list <- tibble(기준일=seq(first(dates), last(dates), by=1)) %>% 
        left_join(
          tibble(기준일=dates, 기록일=dates),
          by='기준일'
        ) %>% 
        fill(기록일)
      
      d1 <- self$today - days(1)
      d7 <- self$today - weeks(1)
      dm <- self$today %m-% months(3)
      dy <- self$today %m-% years(1)
      ly <- make_date(self$year-1,12,31)
      
      filtered <- dates_list %>% 
        filter(기준일 %in% c(d1,d7,dm,dy,ly))
      
      past_value <- self$read_obj('return') %>% 
        filter(기준일 %in% filtered$기록일) %>% 
        rename(기록일=기준일) %>% 
        collect() %>% 
        left_join(
          filtered, by='기록일'
        ) %>% 
        select(-기록일,-평가금액,-총수익률) %>% 
        mutate(
          전년도 = if_else(year(기준일)<self$year, T,F),
          기준일 = case_match(기준일, d1~'1d', d7~'7d', dm~'1m', 
                           dy~'1y', ly~'ly'))
      
      ldays <- past_value %>% filter(전년도) %>% distinct(기준일) %>% .$기준일
      
      past_value <- past_value %>% select(-전년도) %>% 
        pivot_wider(names_from = 기준일, values_from = 총손익)
      
      if(ly %in% c(d1,d7,dm,dy)){
        past_value <- 
          past_value %>% 
          left_join(
            self$read_obj('return') %>% 
              filter(기준일 == ly) %>% 
              select(-기준일, -평가금액,-총수익률) %>% 
              rename(ly=총손익) %>% 
              collect(),
            by=c('자산군','세부자산군','세부자산군2')
          )
      }
      
      self$t_comm3 %>% 
        select(자산군:세부자산군2, 평가금액, 평잔, 총손익, 총수익률) %>% 
        left_join(
          past_value, by=c('자산군','세부자산군','세부자산군2')
        ) %>% 
        mutate(across(any_of(ldays), ~ ly - .x + 총손익)) %>% 
        mutate(across(any_of(setdiff(c('1d','7d','1m','1y'),ldays)), ~ 총손익 - .x)) %>% 
        select(-ly) %>% 
        select(자산군:총수익률,'1d','7d','1m','1y')
    },
    
    
    ##15.(손익현황) 자산군별/계좌별 손익현황 생성 ====
    compute_asset_profit = function(){
      
      df1 <- self$bs_pl_mkt_a %>% 
        filter(통화=='원화') %>% 
        bind_rows(self$bs_pl_mkt_p) %>% 
        mutate(
          is_target = (자산군 == '외화자산' & 세부자산군 == '달러자산'),
          자산군 = if_else(is_target, '주식', 자산군),
          세부자산군 = if_else(is_target, '선진국', 세부자산군),
          세부자산군2 = if_else(is_target, '종목', 세부자산군2)
        ) %>% 
        select(-is_target)
      
      summ_fun <- function(df){
        df %>% 
          summarise(
            장부금액=sum(장부금액),
            평잔 = sum(평잔),
            비용 = sum(비용),
            평가금액=sum(평가금액),
            평가손익=sum(평가손익),
            실현손익=sum(실현손익),
            평가손익증감=sum(평가손익증감),
            총손익=sum(총손익), 
            .groups = 'drop'
          )
      }
      
      
      df2 <- df1 %>% 
        group_by(자산군,세부자산군,세부자산군2) %>% 
        summ_fun()
      
      df3 <- df1 %>% 
        group_by(자산군, 세부자산군) %>% 
        summ_fun() %>% 
        mutate(세부자산군2="", .after=2)
      
      df4 <- df1 %>% 
        group_by(자산군) %>% 
        summ_fun() %>% 
        mutate(세부자산군="", 세부자산군2="", .after = 1)
      
      df5 <- df1 %>%
        summ_fun() %>% 
        mutate(자산군="<합계>", 세부자산군="", 세부자산군2="",.before=1)
      
      # 자산군별 손익현황
      
      self$t_comm3 <- bind_rows(df2,df3,df4,df5)%>%
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
      
      # 자산군별 손익현황 DB업로드
      
      self$t_comm3 %>% 
        select(자산군:세부자산군2, 평가금액,총손익,총수익률) %>% 
        mutate(기준일=self$today, .before=1) %>% 
        self$upsert('return',c('기준일','자산군','세부자산군','세부자산군2'))
      
      
      
      df7 <- df1 %>% 
        group_by(계좌, 자산군) %>% 
        summ_fun()
      
      df8 <- df1 %>% group_by(계좌) %>% 
        summ_fun() %>% 
        mutate(자산군='', .after=1)
      
      # 계좌별 손익현황
      
      self$t_comm4 <- bind_rows(df7,df8)%>%
        mutate(
          계좌 = factor(계좌, levels=self$acct_order),
          자산군 = factor(자산군, levels = self$class_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>% 
        select(-비용) %>% 
        arrange(계좌, 자산군)
      
    },
    
    ##16.(손익현황) 상품별 손익현황 계산====
    compute_comm_profit = function(){
      
      df_a <- self$bs_pl_mkt_a
      df_p <- self$bs_pl_mkt_p
      
      self$comm_profit <- bind_rows(df_a, df_p) %>%
        mutate(
          계좌 = factor(계좌, levels = self$acct_order),
          통화 = factor(통화, levels = self$cur_order),
          자산군 = factor(자산군, levels = self$class_order),
          비용률 = if_else(평잔 != 0, 비용 / 평잔 * 100, 0),
          실현수익률 = if_else(평잔 != 0, 실현손익 / 평잔 * 100, 0),
          평가증감률 = if_else(평잔 != 0, 평가손익증감 / 평잔 * 100, 0),
          총수익률 = 실현수익률 + 평가증감률
        ) %>%
        arrange(계좌, 통화, 자산군, 세부자산군, 세부자산군2, desc(평가금액)) %>%
        select(계좌, 통화, 자산군, 세부자산군, 세부자산군2, 종목명, 보유수량, 
               장부금액, 평잔, 평가금액, 평가손익, 실현손익, 평가손익증감,
               총손익,비용률, 실현수익률, 평가증감률, 총수익률)
    },
    
    ## 17. (유동성관리) 가용자금 분석 테이블 생성 ====
    get_liquidity_analysis = function() {
      
      # [Step 1] inflow_table2: 현재 시점 계좌별 총자산/현금성자산 현황
      # ma_v()가 실행된 상태여야 t_comm4 사용 가능
      
      # 1-1. 계좌별 총자산 (자산군='', 통화='원화')
      df_total <- self$t_comm2 %>% 
        filter(자산군 == '' | is.na(자산군)) %>% 
        select(계좌, 평가금액) %>% 
        rename(총자산 = 평가금액)
      
      # 1-2. 계좌별 현금성자산 (자산군='현금성', 통화='원화')
      df_cash <- self$t_comm2 %>% 
        filter(자산군 == '현금성') %>% 
        group_by(계좌, 자산군) %>% 
        summarise(평가금액=sum(평가금액), .groups='drop') %>% 
        select(계좌, 평가금액) %>% 
        rename(현금성자산 = 평가금액)
      
      # 1-3. 모든 계좌 리스트 확보 (전체 기준)
      # 전체 계좌 목록을 확보하여 추후 테이블 생성 시 누락 방지
      all_accts <- factor(
        unique(c(df_total$계좌, df_cash$계좌)),
        levels = self$acct_order)
      
      current_status <- tibble(계좌 = all_accts) %>% 
        left_join(df_total, by='계좌') %>% 
        left_join(df_cash, by='계좌') %>% 
        replace(is.na(.), 0) %>% 
        pivot_longer(cols = -계좌, names_to = "구분", values_to = "금액") %>% 
        pivot_wider(names_from = 계좌, values_from = 금액) %>% 
        # 합계 열 추가
        mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE)) %>% 
        arrange(구분)
      
      
      # [Step 2] 공통 데이터 준비 (월별 피벗)
      
      # 2-1. 자금유출입 (inflow_table1) 월별 집계
      inflow_monthly <- self$inflow %>% 
        mutate(거래월 = format(as.Date(거래일자), "%Y-%m")) %>% 
        filter(as.Date(거래일자) >= floor_date(self$today, "month")) %>% 
        group_by(거래월, 계좌) %>% 
        summarise(금액 = sum(자금유출입, na.rm = TRUE), .groups = 'drop')
      
      # 2-2. 만기 자산 (maturity_table) 월별 집계
      maturity_data <- self$bs_pl_mkt_a %>% 
        bind_rows(self$bs_pl_mkt_p) %>% 
        filter(자산군=='채권', 세부자산군 %in% c('만기무위험','만기회사채'), 
               통화=='원화', 평가금액>0) %>% 
        left_join(
          self$assets %>% bind_rows(self$pension) %>% select(종목코드, 만기일),
          by = "종목코드"
        ) %>% 
        filter(as.Date(만기일) > self$today) %>% 
        mutate(거래월 = format(as.Date(만기일), "%Y-%m")) %>% 
        group_by(거래월, 계좌) %>% 
        summarise(금액 = sum(평가금액, na.rm = TRUE), .groups = 'drop')
      
      # 2-3. 미래 월 리스트 생성
      future_months <- sort(unique(c(inflow_monthly$거래월, maturity_data$거래월)))
      current_month <- format(self$today, "%Y-%m")
      
      # 현재월이 없으면 추가, 데이터가 아예 없으면 현재월만
      if(length(future_months) == 0) {
        future_months <- current_month
      } else if(!(current_month %in% future_months)){
        future_months <- sort(c(current_month, future_months))
      }
      
      base_proj <- tibble(거래월 = future_months)
      
      
      # [Step 3] inflow_table3: 향후 총자산 추이 (누적 O)
      # 로직: 현재 총자산 + 누적 자금유출입
      
      # 초기값 (현재 총자산)
      init_total <- df_total %>% 
        pivot_wider(names_from = 계좌, values_from = 총자산) %>% 
        mutate(거래월 = current_month)
      
      # 유출입 피벗
      flow_pivot <- inflow_monthly %>% 
        pivot_wider(names_from = 계좌, values_from = 금액, values_fill = 0)
      
      # 결합 및 누적
      total_projection <- bind_rows(init_total, flow_pivot) %>% 
        right_join(base_proj, by = "거래월") %>% # 모든 월 표시
        group_by(거래월) %>% 
        # all_accts에 있는 모든 계좌에 대해 합산 (유출입 없는 계좌도 0으로 처리되어 포함됨)
        summarise(across(any_of(all_accts), \(x) sum(x, na.rm=T))) %>% 
        arrange(거래월) %>% 
        # 결측치 0으로 채우고 누적합 계산
        mutate(across(any_of(all_accts), ~cumsum(tidyr::replace_na(., 0)))) %>% 
        # 합계 열 추가
        mutate(합계 = rowSums(select(., -거래월), na.rm = TRUE))
      
      
      # [Step 4] inflow_table4: 향후 가용자금 추이 (누적 X, 매달 Flow)
      # 로직: 첫달 = 현재 현금성자산 + 이달의 Flow, 이후 = 해당 월의 Flow
      
      # 초기값 (현재 현금성자산)
      init_cash <- df_cash %>% 
        pivot_wider(names_from = 계좌, values_from = 현금성자산) %>% 
        mutate(거래월 = current_month)
      
      # 유출입 + 만기 합산
      total_inflow <- bind_rows(inflow_monthly, maturity_data) %>% 
        group_by(거래월, 계좌) %>% 
        summarise(금액 = sum(금액, na.rm = TRUE), .groups = 'drop') %>% 
        pivot_wider(names_from = 계좌, values_from = 금액, values_fill = 0)
      
      # 현금 흐름과 관련된 모든 계좌 식별
      cash_related_accts <- unique(c(names(init_cash), names(total_inflow)))
      cash_related_accts <- setdiff(cash_related_accts, "거래월")
      
      # 결합 (누적하지 않음)
      cash_projection <- bind_rows(init_cash, total_inflow) %>% 
        right_join(base_proj, by = "거래월") %>% 
        group_by(거래월) %>% 
        summarise(across(any_of(cash_related_accts), \(x) sum(x, na.rm=T))) %>% 
        arrange(거래월) %>% 
        mutate(across(any_of(cash_related_accts), ~tidyr::replace_na(., 0))) %>% 
        # 합계 열 추가
        mutate(합계 = rowSums(select(., -거래월), na.rm = TRUE))
      
      
      return(list(
        current_status = current_status,
        total_projection = total_projection,
        cash_projection = cash_projection
      ))
    },
    
    ##18.(메서드) 평가금액 계산====
    run_valuation = function(){
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      self$bs_pl_mkt_a <- self$evaluate_bs_pl_assets()
      self$bs_pl_mkt_p <- self$evaluate_bs_pl_pension()
      self$compute_total()
      self$compute_total_allocation()
      self$compute_asset_profit()
      self$compute_comm_profit()
      # self$plot_total_profit()
    },
    
    ##19.(메서드) 기초평가손익 갱신====
    renew_last_eval_profit = function(){
      self$assets %>% 
        select(-기초평가손익) %>% 
        left_join(
          self$read('eval_profit') %>% 
            filter(연도==self$year-1) %>% 
            select(-연도) %>% 
            rename(기초평가손익=평가손익),
          by=c('계좌','종목코드')
        ) %>% 
        arrange(행번호) %>% 
        self$add_table('assets')
      
      self$pension %>% 
        select(-기초평가손익) %>% 
        left_join(
          self$read('eval_profit') %>% 
            filter(연도==self$year-1) %>% 
            select(-연도) %>% 
            rename(기초평가손익=평가손익),
          by=c('계좌','종목코드')
        ) %>% 
        arrange(행번호) %>% 
        self$add_table('pension')
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

