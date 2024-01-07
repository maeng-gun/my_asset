library(dplyr)
library(R6)
library(ecos)
import::from(stringr, str_detect, str_extract)

Ecos <- R6Class(
  "Ecos",
  
  public = list(
    table_list = NULL,
    
    #속성 초기화
    initialize = function() {
      yaml::read_yaml(file = 'config.yaml', 
                      readLines.warn = F)$ecos |> 
        ecos.setKey()
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
          select(stat_name, item_name, stat_code, item_code, cycle:new_name) |> 
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
