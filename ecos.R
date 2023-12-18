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
  
  if(is.null(name)||name == '') table_list
  else{
    table_list|> 
    filter(str_detect(stat_name, name), 
           srch_yn=='Y')
  }
}

find_items <- function(code='', item='전체', cyl='전체'){
  
  if(is.null(code)||code == '') {
    tibble(item_name='',stat_code='',item_code='',cycle='')
  }
  else {
    df <- statItemList(code) |> tibble()
  
    if(is.null(item)||item=='전체'){}
    else df <- df |> filter(item_name==item)
    
    if(is.null(cyl)||cyl=='전체'){} 
    else df <- df |> filter(cycle==cyl)
  
    df |> select(item_name, stat_code, item_code, cycle)
  }
}

save_items <- function(df){
  readRDS('ecos_item.rds') |> 
    bind_rows(df) |> 
    distinct() |> 
    saveRDS('ecos_item.rds')
}

myHelloGadget = function(v){
  ui = miniPage(
    gadgetTitleBar("ECOS items"),
    miniTabstripPanel(
      miniTabPanel(
        "통계표목록",
        icon=icon('table'),
        miniContentPanel(
          fillCol(
            uiOutput('in_ui'),
            tableOutput('tbl')
          )
        )           
      ),
      miniTabPanel(
        "아이템목록",
        icon=icon('table'),
        miniContentPanel(
          fillCol(
            uiOutput('in_ui2'),
            tableOutput('tbl2')
          )
        )
      )
    )
  )
  
  server = function(input, output, session){
    
    values <- reactiveValues(name='', 
                             code_list='',
                             code='', 
                             item_list='전체',
                             item='전체', 
                             cyl_list='전체',
                             cyl='전체')
    
    output$in_ui <- renderUI({
      textInput('name','검색어', values$name)
    })
    
    observe({
      values$name <- input$name
      df <- find_stat(input$name)
      output$tbl <- renderTable(df)
      values$code_list <- c('', df$stat_code)
      values$code <- input$code_in
    })
    
    output$in_ui2 <- renderUI({
      fillRow(
        selectizeInput('code_in','통계표코드', 
                       values$code_list, values$code),
        selectizeInput('item_in','아이템이름', 
                       values$item_list, values$item),
        selectizeInput('cyl_in','데이터주기', 
                       values$cyl_list, values$cyl)
      )
    })
    

    observe({
      df2 <- find_items(input$code_in, input$item_in, input$cyl_in)
      output$tbl2 <- renderTable(df2)
      values$code <- input$code_in
      values$item_list <- c('전체', unique(df2$item_name))
      values$item <- input$item_in
      values$cyl_list <- c('전체', unique(df2$cycle))
      values$cyl <- input$cyl_in
    })
    
    observeEvent(input$done, {
      returnValue = 1 # return 값 설정 가능
      stopApp(returnValue)
    })
  }
  runGadget(ui,server, 
            viewer=browserViewer())
}

myHelloGadget()
