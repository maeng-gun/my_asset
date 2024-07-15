library(reticulate)
library(R6)
library(dplyr)
library(httr)
py_run_file('xw.py')

#[클래스] XlWings ====
XlWings <- R6Class(
  
  classname = 'XlWings',
  public=list(
    
    path=NULL,
    ap=NULL,
    wb=NULL,
    ws=NULL,
    
    ##1. 속성 초기화 ====
    initialize = function(file){
      self$open(file)
    },
    
    ##2.(메서드) 파일 열기 ====
    open = function(file){
      
      self$path <- paste0(getwd(), "/xlfiles/", file)
      
      if(length(py$xw$apps)==0){
        self$wb <- py$xw$Book(self$path)
        self$ap <- self$wb$app
        self$ws <- self$wb$sheets(1)
      } else {
        self$ap <- py$xw$apps(py$xw$apps$keys()[1])
        self$wb <- self$ap$books$open(self$path)
        self$ws <- self$wb$sheets(1)
      }
    },
    
    ##3.(메서드) 파일 닫기 ====
    close = function(){
      self$wb$close()
    },
    
    ##4.(메서드) 앱 닫기 ====
    kill = function(){
      self$ap$kill()
    },
    
    ##5.(메서드) 가져오기 ====
    read_table = function(cell){
      py$to_df(self$ws$range(cell)) %>% as_tibble()
    },
    
    ##6.(메서드) 새로고침 ====
    refresh = function(){
      self$wb$macro('DoAllSheetRefresh')()
    },
    
    ##7.(메서드) 붙여넣기 ====
    paste = function(value, cell, down=F){
      py$paste(self$ws$range(cell), value, down)
    },
    
    ##8.(메서드) 테이블지우기 ====
    clear_table = function(cell){
      self$ws$range(cell)$expand('table')$clear()
    }
  )
)


self <- XlWings$new('stocks.xlsm')

self$clear_table('A7')
rep('20240711',4) %>% 
  self$paste('C4')

df$종목코드 %>% 
  self$paste('A7',T)
self$refresh()


df <- self$read_table('A6') %>% 
  setNames(c('종목코드','종목명','대분류','중분류',
             '소분류', '시장구분','시가총액')) %>% 
  arrange(desc(시가총액))


df %>% 
  arrange(desc(시가총액)) %>% 
  group_by(중분류) %>% 
  reframe(종목명 = head(종목명, n=3),
          시가총액 = head(시가총액, n=3)/1000000000000)
  
df %>% filter(중분류=='미디어')

self$kill()



py$paste(self$ws$range('A7'), df$종목코드, T)




#[클래스] KrxStocks ====
KrxStocks <- R6Class(
  
  classname = 'KrxStocks',
  public=list(
    
    url = NULL, user.agent = NULL, referer = NULL,
    stock_list = NULL,
      
    ##1. 속성 초기화 ====
    initialize = function(){
      self$url <- 'http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd'
      self$user.agent <- 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36 '
      self$referer <- 'http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201'
      self$get_stock_list()
    },
    
    ##2.(메서드) KRX POST ====
    post_krx = function(params){
      res <- POST(url=self$url,
                  query=params, 
                  user_agent(self$user.agent), 
                  add_headers(referer=self$referer)) %>% 
        content('t') %>% 
        jsonlite::fromJSON()
      
      res[[ names(res)[1] ]] %>% 
        as_tibble()
    },
    
    ##3.(메서드) KRX POST ====
    get_stock_list = function(yyyymmdd=NULL){
      
      if(is.null(yyyymmdd)){ yyyymmdd <- strftime(lubridate::today(),'%Y%m%d')}
      
      params1 <- list(bld = "dbms/MDC/STAT/standard/MDCSTAT01501",
                      mktId = "ALL",
                      trdDd = yyyymmdd)
      
      self$stock_list <- self$post_krx(params1) %>% 
        select(종목코드=ISU_SRT_CD, 종목명=ISU_ABBRV, 시장구분=MKT_ID) %>% 
        mutate(종목코드=paste0('A',종목코드),
               시장구분=case_match(시장구분, 'STK'~'코스피', 'KSQ'~'코스닥', 
                                   'KNX'~'코넥스'))
    },
    
    ##4.(메서드) 종목코드 찾기 ====
    find_code = function(name=NULL, mkt=NULL){
      
      df <- self$stock_list
      
      if(!is.null(mkt)){df <- df %>% filter(시장구분 %in% mkt)}
      if(!is.null(name)){
        df <- df %>% 
          filter(stringr::str_detect(종목명, name))
      }
      
      df
    }
  )
)

self <- KrxStocks$new()






