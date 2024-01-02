import::from(stringr, str_detect, str_extract)
import::from(readxl, read_excel)
import::from(shinyjs, useShinyjs, extendShinyjs, js)


library(dplyr)
library(shiny)
library(ecos)
library(miniUI)
library(bs4Dash)

yaml::read_yaml('config.yaml', 
                readLines.warn = F)$ecos |> 
  ecos.setKey()

table_list <- statTableList() |> tibble()

find_stat <- function(name=''){
  
  if(is.null(name)||name == '') table_list
  else{
    table_list |> 
      filter(str_detect(stat_name, name), 
           srch_yn=='Y') |> 
      mutate(stat_name = stringr::str_extract(stat_name,"(?<=\\.\\s).*"))
  }
}

find_items <- function(code='', item='전체', cyl='전체'){
  
  if(is.null(code)||code == '') {
    tibble(item_name='',stat_code='',item_code='',cycle='')
  }
  else {
    df <- statItemList(code) |> 
      tibble() |> 
      select(stat_name, item_name, stat_code, item_code, cycle:data_cnt)
  
    # if(is.null(item)||item=='전체'){}
    # else df <- df |> filter(item_name==item)
    # 
    # if(is.null(cyl)||cyl=='전체'){} 
    # else df <- df |> filter(cycle==cyl)
  
    # df |> select(stat_name, item_name, stat_code, item_code, cycle:data_cnt)
    df
  }
}

save_items <- function(df){
  readRDS('ecos_item.rds') |> 
    bind_rows(df) |> 
    distinct() |> 
    saveRDS('ecos_item.rds')
}

myHelloGadget = function(){
  ui = miniPage(
    useShinyjs(),
    extendShinyjs(
      text =  "shinyjs.closeWindow = function() { window.close(); }",
      functions = c("closeWindow")
    ),
    gadgetTitleBar("ECOS items", left=NULL),
    miniTabstripPanel(
      miniTabPanel(
        "통계표목록",
        icon=icon('table'),
        miniContentPanel(
          column(4,textInput('name','검색어')),
          column(8,tableOutput('tbl'))
        )           
      ),
      miniTabPanel(
        "아이템목록",
        icon=icon('table'),
        miniContentPanel(
          column(
            width=4,
            selectizeInput('code_in','통계표코드',''),
            selectizeInput('item_in','아이템이름','전체'),
            selectizeInput('cyl_in','데이터주기','전체')
            ),
          column(8, tableOutput('tbl2'))
        )
      )
    )
  )
  
  server = function(input, output, session){
    
    observe({
      df <- find_stat(input$name)
      output$tbl <- renderTable(df)
      
      updateSelectizeInput(session, 'code_in', 
                           choices = c('', df$stat_code),
                           selected = input$code_in)
      
      df2 <- find_items(input$code_in)
      
      if(is.null(input$item_in)||input$item_in=='전체'){}
      else df2 <- df2 |> filter(item_name==input$item_in)
      
      if(is.null(input$cyl_in)||input$cyl_in=='전체'){}
      else df2 <- df2 |> filter(cycle==input$cyl_in)
      
      output$tbl2 <- renderTable(df2)
      
      updateSelectizeInput(session, 'item_in', 
                           choices = c('전체', unique(df2$item_name)),
                           selected = input$item_in)
      
      updateSelectizeInput(session, 'cyl_in', 
                           choices = c('전체', unique(df2$cycle)),
                           selected = input$cyl_in)
      
      session$onSessionEnded(
        function(){
          stopApp(df2)
        }
        
      )
      
      observeEvent(input$done, {
        js$closeWindow()
        stopApp(df2)
      })
      
    })
  }
  runGadget(ui,server, 
            viewer=browserViewer())
}

myHelloGadget()

