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


#[클래스] MyData ====
MyData <- R6Class(
  
  classname = 'MyData',
  public=list(
    
    con=NULL, config=NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(pw){
      
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
      
      self$token_tmp <- paste0("KIS", account)

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
      
    },
    
    ##메서드(2) - 토큰 불러오기 ====
    read_token = function() {
      tryCatch({

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
      headers <- c("tr_id" = "FHKST01010100", "custtype" = "P")
      
      fetch_single <- function(code){
        data <- list(
          FID_COND_MRKT_DIV_CODE = "J",
          FID_INPUT_ISCD = code
        )
        
        # API 호출
        res <- self$GET_tbl(path, data, headers)
        
        # 정상적으로 응답이 오면 현재가(stck_prpr)를 숫자로 변환하여 반환
        # 에러 발생 시 NA 반환
        tryCatch({
          if(!is.null(res$output$stck_prpr)){
            as.numeric(res$output$stck_prpr) 
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)
      }
      
      result_prices <- purrr::map_dbl(sym_cd, function(code) {
        
        # (중요) 연속 호출 시 API 제한(초당 건수 등)을 피하기 위해 미세한 딜레이 추가
        # 필요에 따라 시간(0.05초)을 조절하세요.
        Sys.sleep(0.05) 
        
        fetch_single(code)
      })
      
      return(result_prices)
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
    y_num=NULL, grid=NULL, future_eval=NULL, closing_prices=NULL,
    account_allocation2=NULL,
    
    ## 1. 속성 초기화====
    initialize = function(pw) {
      
      self$pw <- pw

      super$initialize(self$pw)
      self$today <- today()
      self$year <- year(self$today)
      
      tryCatch({
        if(dbExistsTable(self$con, 'inflow')){
          dbExecute(self$con, glue("DELETE FROM inflow WHERE \"거래일자\" < '{self$today}'"))
        }
      }, error = function(e) {
        # 테이블이 없거나 권한 문제 등 에러 발생 시 무시하고 진행
      })

      self$days <- seq(make_date(2024,1,1), 
                       self$today,
                       by='day')
      
      if (is.null(self$bl)) {
        self$bl <- AutoInvest$new(self$pw, 'boolio')
      }
      
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
                             "원화상품","외화상품","외환","원화")
      
    },
    
    ## 2.(공통) 가격 업데이트====
    update_new_price = function(){
      
      #1) 환율
      
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
      
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      
      
      #2) 국내주식 종목/ETF 종가
      
      all_codes <- tibble(self$bs_pl_a) %>% 
        bind_rows(
          tibble(self$bs_pl_p)
        ) %>% 
        filter(보유수량!=0) %>% 
        .$종목코드
      
      target_codes <- unique(all_codes[str_detect(all_codes, 
                                                  "^\\d[a-zA-Z0-9]{4}\\d$")])
      
      closing_prices <-  
        tibble(종목코드 = target_codes,
               종가 = self$bl$get_current_price(target_codes))
      
      #3) 금가격 종가
      
      url <- "https://api.stock.naver.com/marketindex/metals/M04020000"
      
      tryCatch({
        # 1. API 요청 (httr 패키지 사용)
        resp <- GET(url = url)
        
        # 2. JSON 파싱 (jsonlite 패키지 사용)
        # functions.R 상단에 library(httr), library(jsonlite)가 선언되어 있어 바로 사용 가능합니다.
        json_data <- content(resp, as = 'text', encoding = 'UTF-8') %>% 
          jsonlite::fromJSON()
        
        # 3. 현재가(closePrice) 추출 및 숫자 변환
        # get_exchange_rate 함수처럼 쉼표가 포함된 문자열을 숫자로 변환하기 위해 readr::parse_number 사용
        price <- json_data$closePrice %>% 
          readr::parse_number()
        
        gold <- tibble(종목코드='04020000', 종가=price)
        
      }, error = function(e){
        # 에러 발생 시 NA 반환 (기존 get_exchange_rate 스타일은 경고 억제만 되어 있으나, 안전을 위해 예외처리)
        message("금 시세 조회 실패")
        gold <- tibble()
      })
      
      #4) 펀드 기준가가
      
      
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
      
      fund_codes <- all_codes[(str_sub(all_codes, 1, 2) == 'K5')]
      if(length(fund_codes) > 0) {
        fund_prices <- tibble(종목코드 = fund_codes, 종가 = get_fund_price(fund_codes)/1000)
      } else {
        fund_prices <- tibble()
      }
      
      
      #5) 결합
      
      self$closing_prices <- 
        bind_rows(closing_prices, gold, fund_prices)
      
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
      
      price <- self$assets %>%
        select(계좌, 종목코드, 상품명, 평가금액) %>% 
        filter(평가금액!=0) %>% 
        bind_rows(
          mutate(self$bl$inquire_balance_ovs(), 계좌 = '불리오')
        ) %>%
        select(계좌, 종목코드,평가금액) %>% as.data.table()
      
      
      bs_pl <- self$bs_pl_a
      
      last_eval <- self$assets %>% 
        select(계좌, 종목코드, 기초평가손익) %>% 
        as.data.table()
      
      bs_pl <- merge(bs_pl, price, by=c("계좌","종목코드"), all.x=TRUE)
      bs_pl <- merge(bs_pl, self$closing_prices, by='종목코드', all.x=TRUE)
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
     
      bs_pl <- self$bs_pl_p

      last_eval <- self$pension %>% 
        select(계좌, 종목코드, 기초평가손익) %>% 
        as.data.table()
      
      bs_pl <- merge(bs_pl, price, by=c("계좌","종목코드"), all.x=TRUE)
      bs_pl <- merge(bs_pl, self$closing_prices, by='종목코드', all.x=TRUE)
      bs_pl <- merge(bs_pl, last_eval, by=c("계좌","종목코드"), all.x=TRUE)
      
      bs_pl <- bs_pl[평잔 > 0.02]
      bs_pl[, 장부금액 := fifelse(장부금액 < 1, 0, 장부금액)]
      
      bs_pl[, 기초평가손익 := fifelse(is.na(기초평가손익), 0, 기초평가손익)]
      bs_pl[, 평가금액 := fcase(!is.na(평가금액), 평가금액, 
                            !is.na(종가), 종가 * 보유수량, rep(TRUE, .N), 장부금액)]
      
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

      tryCatch({
        if(dbExistsTable(self$con, 'eval_profit')){
          dbExecute(self$con, glue("DELETE FROM eval_profit WHERE \"연도\" = '{self$year}'"))
        }
      }, error = function(e) {
        # 테이블이 없거나 권한 문제 등 에러 발생 시 무시하고 진행
      })
      
      
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
        select(계좌, 자산군, 세부자산군, 세부자산군2, 평가금액) %>%
        mutate(자산군 = if_else(자산군 == "" | is.na(자산군), 
                             "합계", as.character(자산군))) %>% 
        group_by(계좌, 자산군, 세부자산군, 세부자산군2) %>% 
        summarise(평가금액=sum(평가금액),.groups = 'drop') %>%
        pivot_wider(names_from = 계좌, values_from = 평가금액) %>% 
        mutate(
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
        ) %>% 
        arrange(자산군, 세부자산군, 세부자산군2) %>% 
        select(자산군, 세부자산군, 세부자산군2,
               한투연금저축,엔투저축연금,미래DC,엔투IRP,농협IRP,
               엔투ISA,한투ISA,엔투하영,불리오,금현물,한투) %>% 
        mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE))
      
      self$account_allocation2 <- self$read('acct_allo') %>% 
        mutate(
          자산군 = factor(자산군, levels = self$class_order),
          세부자산군 = factor(세부자산군, levels = self$class2_order),
          세부자산군2 = factor(세부자산군2, levels = self$class3_order),
        ) %>% 
        arrange(자산군, 세부자산군, 세부자산군2) %>% 
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
      dm <- self$today %m-% months(1)
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
      
      tryCatch({
        if(dbExistsTable(self$con, 'return')){
          dbExecute(self$con, glue("DELETE FROM return WHERE \"기준일\" = '{self$today}'"))
        }
      }, error = function(e) {
        # 테이블이 없거나 권한 문제 등 에러 발생 시 무시하고 진행
      })
      
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
     
      self$update_new_price()
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
