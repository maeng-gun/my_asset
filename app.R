library(shiny)
library(bs4Dash)
import::from(shinyWidgets, sendSweetAlert, useSweetAlert)
# import::from(shinyjs, useShinyjs, extendShinyjs, js)

# <User Interface> ####

# 1. 대쉬보드 헤더####
header <- dashboardHeader(
    title = dashboardBrand(
        title = div("포트폴리오 관리", align = 'center'),
        color = "info"
    )
)

# 2. 대쉬보드 사이드바(메뉴)####
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = 'menu_tabs',
        sidebarHeader("경제지표"),
        menuItem(
            text = "한국은행 지표선정",
            icon = icon(name = "hand-pointer"),
            tabName = "ecos_stat"
        ),
        menuItem(
          text = "포트폴리오 운용현황",
          icon = icon(name = "sack-dollar"),
          tabName = "pf_bs_pl"
        )
    )
)

# 3. 대쉬보드 본문####
body <- dashboardBody(
    useSweetAlert(),
    tabItems(
        tabItem(
            tabName = "ecos_stat",
            tabBox(
                width=12,
                status='primary',
                type='tabs',
                # 탭 - 선정 아이템====
                tabPanel(
                    title = "선정 아이템",
                    icon=icon('square-check'),
                    tableOutput('selected_item')
                ),
                # 탭 - 통계표 조회====
                tabPanel(
                    title = "통계표 조회",
                    icon=icon('table-list'),
                    fluidRow(
                        column(2,textInput('name','검색어')),
                        column(10,tableOutput('ecos_stat_tables'))
                    )
                ),
                # 탭 - 아이템 추가====
                tabPanel(
                    title = "아이템 추가",
                    icon=icon('square-plus'),
                    fluidRow(
                        column(
                            width=3,
                            selectizeInput('name_in','통계표이름','전체'),
                            selectizeInput('code_in','통계표코드','전체'),
                            selectizeInput('item_in','아이템이름','전체'),
                            selectizeInput('cyl_in','데이터주기','전체'),
                            br(),
                            textInput('ecos_name',"아이템명 설정", 
                                      width='100%'),
                            actionButton('add_item','아이템 추가',
                                         icon=icon('square-plus'),
                                         width='100%',
                                         status='primary')
                        ),
                        column(9, tableOutput('ecos_item_tables'))
                    )
                )
            )
        ),
        tabItem(
          tabName = 'pf_bs_pl',
          box(
            status='primary',
            width=12,
            title="포트폴리오"
          )
        )
    )
)

#바닥글
footer <- dashboardFooter(
    right = "developed by H.M. Choi"
)


ui <- dashboardPage(
    header, sidebar, body, footer=footer
)


# <Server> ====
    
server <- function(input, output, session) {
    
    source("functions.R")
    
    ec = Ecos$new()
    rv <-  reactiveValues(
        name_in='전체',
        code='전체', df=NULL, df2=NULL, 
        df3=NULL, df4=NULL, df5=NULL,
        df_s=ec$read_items(),
        df_d=NULL
        )
    
    #통계표 조회====
    observeEvent(input$name,{
        rv$df <- ec$find_stat(input$name)
        updateSelectizeInput(session, 'name_in', 
                             choices = c('전체',rv$df$stat_name),
                             selected = '전체')
        
    })
    
    #아이템 추가==== 
    
    # * 통계표 이름==== 
    observeEvent(input$name_in,{

        rv$name_in <- input$name_in

        updateSelectizeInput(session, 'name_in',
                             choices = c('전체', rv$df$stat_name),
                             selected = rv$name_in)
        
        if(rv$name_in=='전체'){
            code <- '전체'
            rv$code <- '전체'
        }
        else {
            rv$df_d <- rv$df |> filter(stat_name==rv$name_in)
            code <- rv$df_d$stat_code
            
            if(length(code)==1){rv$code <- code}
            else {rv$code <- code[1]}
            
        }
        updateSelectizeInput(session, 'code_in', choices = code,
                             selected = rv$code)
    })
    
    # * 통계표 코드==== 
    observeEvent(input$code_in,{
        tryCatch({
            rv$df2 <- ec$find_items(input$code_in)
        }, error = function(e) {
            rv$df2 <- ec$find_items('전체')
        })
      
        rv$df5 <- rv$df2

        updateSelectizeInput(session, 'item_in',
                             choices = c('전체', unique(rv$df2$item_name)),
                             selected = '전체')
        updateSelectizeInput(session, 'cyl_in',
                             choices = c('전체', unique(rv$df2$cycle)),
                             selected = '전체')
    })

    # * 아이템이름==== 
    observeEvent(input$item_in,{
        if(is.null(input$item_in)||input$item_in=='전체'){
            rv$df3 <- rv$df2
        }
        else rv$df3 <- rv$df2 |> filter(item_name==input$item_in)

        rv$df5 <- rv$df3
        
        updateSelectizeInput(session, 'item_in',
                             choices = c('전체', unique(rv$df2$item_name)),
                             selected = input$item_in)
        
        updateSelectizeInput(session, 'cyl_in',
                             choices = c('전체', unique(rv$df2$cycle)),
                             selected = '전체')        
    })
    
    # * 데이터주기====    
    observeEvent(input$cyl_in,{
        if(is.null(input$cyl_in)||input$cyl_in=='전체'){
            rv$df4 <- rv$df3
        }
        else rv$df4 <- rv$df3 |> filter(cycle==input$cyl_in)

        rv$df5 <- rv$df4
                
        updateSelectizeInput(session, 'cyl_in',
                             choices = c('전체', unique(rv$df3$cycle)),
                             selected = input$cyl_in)
    })
    
    # * 아이템 추가====    
    observeEvent(input$add_item,{
        ec$save_items(rv$df5, input$ecos_name)
        sendSweetAlert(title="추가하였습니다!", type='success')
        rv$df_s <- ec$read_items()
    })
    
    observe({
        output$selected_item <- renderTable(rv$df_s)
        output$ecos_stat_tables <- renderTable(rv$df)
        output$ecos_item_tables <- renderTable(rv$df5)
    })
}

shinyApp(ui = ui, server = server,
         options = list(port=5148))
