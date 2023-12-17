import::from(stringr, str_detect)
import::from(readxl, read_excel)
library(dplyr)
library(shiny)
library(ecos)
library(miniUI)

yaml::read_yaml('config.yaml', 
                readLines.warn = F)$ecos |> 
  ecos.setKey()

table_list <- statTableList() |> tibble() 

find_stat <- function(name=''){
  
  if(name == '') table_list
  else{
    table_list|> 
    filter(str_detect(stat_name, name), 
           srch_yn=='Y')
  }
}

find_items <- function(code, item='', cyl=''){
  df <- statItemList(code) |> tibble()
  if(item!='') df <- df |> filter(item_name==item)
  if(cyl!='') df <- df |> filter(cycle==cyl)
  df |> select(item_name, stat_code, item_code, cycle)
}

save_items <- function(df){
  readRDS('ecos_item.rds') |> 
    bind_rows(df) |> 
    distinct() |> 
    saveRDS('ecos_item.rds')
}


saveRDS(df, 'df.rds')

find_stat('시장금리')
find_items('817Y002')

readRDS('ecos_item.rds')

name <- '국고채(10년)'
item_table <- read_excel('ecos.xlsx')
item_info <- item_table |> 
  filter(통계항목명 = name)

code = '721Y001'
item1 = '5050000'
cyl = "M"

  


start <-  NULL
end <-  NULL
if(is.null(start)) start <- df_item$start_time
if(is.null(end)) end <- df_item$end_time
cnt <- df_item$data_cnt
cyl <- df_item$cycle

statSearch(stat_code=code, item_code1=item1, cycle=cyl, 
           start_time=start, end_time=end, count=100) |> tibble() |> View()



myHelloGadget = function(v){
  ui = miniPage(
    gadgetTitleBar("ECOS items"),
    miniContentPanel(
      fillCol(
        uiOutput('in_ui'),
        tableOutput('tbl'),
        textOutput('num')
      )
    )
  )
  
  server = function(input, output, session){
    
    values <- reactiveValues(code='', item='', cyl='')
    
    output$in_ui <- renderUI({
      fillRow(
        textInput('name','검색어'),
        actionButton('btn1','입력'),
        selectizeInput('code_in','통계표코드', values$code),
        selectizeInput('item_in','아이템이름', values$item),
        selectizeInput('cyl_in','아이템이름', values$cyl),
        actionButton('btn2','입력'),
        actionButton('btn3','초기화')
      )
    })
    
    observeEvent(input$btn1,{
      df <- find_stat(input$name)
      output$tbl <- renderTable(df)
      values$code <- df$stat_code
    })

    observeEvent(input$btn2,{
      df <- find_items(input$code_in, input$item_in, input$cyl_in)
      output$tbl <- renderTable(df)
      values$code <- c('', unique(df$stat_code))
      values$item <- c('', unique(df$item_name))
      values$cyl <- c('', unique(df$cycle))
      
    })
    
    observeEvent(input$btn3,{
      df <- find_items(input$code_in, input$item_in, input$cyl_in)
      output$tbl <- renderTable(df)
      values$code <- 
      values$item <- c('', unique(df$item_name))
      values$cyl <- c('', unique(df$cycle))
      
    })
    
    observeEvent(input$done, {
      returnValue = 1 # return 값 설정 가능
      stopApp(returnValue)
    })
  }
  runGadget(ui,server, 
            viewer=dialogViewer('ggg', width = 1200, height = 1000))
}

myHelloGadget()
