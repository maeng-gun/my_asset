library(reticulate)
library(R6)
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


