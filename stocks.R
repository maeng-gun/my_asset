library(reticulate)
library(R6)
library(dplyr)
library(stringr)
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