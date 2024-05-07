library(shiny)
library(bs4Dash)
library(waiter)
library(flextable)
library(shinyWidgets)
import::from(shinyjs, useShinyjs, extendShinyjs, js)

# <User Interface> ====

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
    sidebarHeader("포트폴리오 관리"),
    menuItem(
      text = "자산운용 내역 기록",
      icon = icon('receipt'),
      tabName = 'trading_record'
    ),
    menuItem(
      text = "자산운용 현황",
      icon = icon("sack-dollar"),
      tabName = "pf_bs_pl"
    ),
    menuItem(
      text = "자산/유동성 추이",
      icon = icon("chart-line"),
      tabName = "pf_liquid"
    ),
    sidebarHeader("경제지표"),
    menuItem(
      text = "한국은행 지표선정",
      icon = icon("hand-pointer"),
      tabName = "ecos_stat"
    )
  ),
  br(),
  actionButton('close_win',
               label = '프로그램 종료',
               width='90%',
               status='primary')
)

# 3. 대쉬보드 본문####
body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", 
                functions = c("closeWindow")),
  useSweetAlert(),
  useWaiter(),
  # autoWaiter(
  #   id=c('pf_box1','trading_box'),
  #   html = tagList(spin_loader(), "로딩중..."),
  #   color = transparent(.5)
  # ),
  tabItems(
    ##1) 자산운용 내역 기록====
    tabItem(
      tabName = 'trading_record',
      tabBox(
        width=12,
        status='primary',
        type='tabs',
        
        ###a. 거래내역====
        tabPanel(
          title="거래내역",
          fluidRow(
            box(
              id='trading_box',
              width = 12,
              status = 'info',
              solidHeader = T,
              title = "입력사항",
              collapsible = F,
              uiOutput('manage_ass_trade'),
              br(),
              div(
                actionButton(
                  inputId = "ass_trade_new",
                  label = "추가",
                  width = '30%', 
                  status = "info"
                ),
                actionButton(
                  inputId = "ass_trade_mod",
                  label = "수정",
                  width = '30%', 
                  status = "success"
                ),
                actionButton(
                  inputId = "ass_trade_del",
                  label = "삭제",
                  width = '30%', 
                  status = "primary"
                ), style='text-align: center')
              
            )
          ),
          fluidRow(
            box(
              width = 12,
              solidHeader = F,
              collapsible = F,
              headerBorder = F,
              uiOutput("trade_table")
            )
          )
        ),
        
        ###b. 투자종목 관리====
        tabPanel(
          title="투자종목 관리",
          fluidRow(
            box(
              id='ticker_box',
              width = 12,
              status = 'info',
              solidHeader = T,
              title = "입력사항",
              collapsible = F,
              uiOutput('manage_ticker'),
              br(),
              div(
                actionButton(
                  inputId = "ticker_new",
                  label = "추가",
                  width = '30%', 
                  status = "info"
                ),
                actionButton(
                  inputId = "ticker_mod",
                  label = "수정",
                  width = '30%', 
                  status = "success"
                ),
                actionButton(
                  inputId = "ticker_del",
                  label = "삭제",
                  width = '30%', 
                  status = "primary"
                ), style='text-align: center')
              
            )
          ),
          fluidRow(
            box(
              width = 12,
              solidHeader = F,
              collapsible = F,
              headerBorder = F,
              uiOutput("ticker_table")
            )
          )
        ),
        
        ###c. 구분항목 관리====
        tabPanel(
          title="구분항목 관리",
          box(
            id='list_box',
            width = 12,
            status = 'info',
            solidHeader = T,
            title = "입력사항",
            collapsible = F,
            fluidRow(
              column(
                width = 2,
                uiOutput('ass_account_list')
              ),
              column(
                width = 2,
                uiOutput('pen_account_list')
              ),
              column(
                width = 2,
                uiOutput('ass_cur_list')
              ),
              column(
                width = 2,
                uiOutput('ass_class_list')
              ),
              column(
                width = 2,
                uiOutput('ass_class1_list')
              ),
              column(
                width = 2,
                uiOutput('ass_class2_list')
              )
            )
          )
        )
      )
    ),
    ##2) 자산운용 현황====
    tabItem(
      tabName = 'pf_bs_pl',
      actionButton('kis','한투접속'),
      br(),
      br(),
      tabBox(
        id='pf_box1',
        width=12,
        status='primary',
        type='tabs',
        
        ###a. 투자자산현황====
        tabPanel(
          title="투자현황",
          h5("1. 자산군별 배분현황"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("allo0")
            ),
            column(
              width = 8,
              uiOutput("allo1")
            )
          ),
          h5("2. 통화별 배분현황"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("allo2")
            ),
            column(
              width = 8,
              uiOutput("allo3")
            )
          ),
          h5("3. 불리오 배분현황"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("allo5")
            ),
            column(
              width = 8,
              uiOutput("allo4")
            )
          )
        ),
        ###b. 투자손익현황====
        tabPanel(
          title="투자손익현황",
          h5("1. 자산군별 손익현황"),
          br(),
          fluidRow(
            column(
              width = 6,
              uiOutput("class_ret_a")
            ),
            column(
              width = 6,
              uiOutput("class_ret_a2")
            )
          ),
          br(),
          h5("2. 개별자산 손익현황"),
          br(),
          fluidRow(
            uiOutput("bs_pl_mkt_a")
          )
        ),
        ###c. 연금현황====
        tabPanel(
          title="연금현황",
          h5("1. 자산군별 배분현황"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("allo6")
            ),
            column(
              width = 8,
              uiOutput("allo7")
            )
          ),
          h5("2. 계좌별 배분현황"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("allo8")
            ),
            column(
              width = 8,
              uiOutput("allo9")
            )
          )
        ),
        ###d. 연금손익현황====
        tabPanel(
          title="연금손익현황",
          h5("1. 자산군별 손익현황"),
          br(),
          fluidRow(
            column(
              width = 6,
              uiOutput("class_ret_p")
            ),
            column(
              width = 6,
              uiOutput("class_ret_p2")
            )
          ),
          br(),
          h5("2. 개별자산 손익현황"),
          br(),
          fluidRow(
            uiOutput("bs_pl_mkt_p")
          )
        )
      )
    ),
    ##3) 자산/유동성 추이====
    tabItem(
      tabName = "pf_liquid",
      box(
        id='list_box',
        width = 12,
        status = 'info',
        solidHeader = T,
        title = "자산/유동성 추이",
        collapsible = F,
        fluidRow(
          column(
            width = 2,
            uiOutput('manage_inflow'),
          ),
          column(
            width = 3,
            uiOutput('inflow_table')
          ),
          column(
            width = 7,
            plotOutput('inflow_plot')
          )
        )
      )
    ),
    ##4) 한국은행 지표선정====
    tabItem(
      tabName = "ecos_stat",
      tabBox(
        width=12,
        status='primary',
        type='tabs',
        ###a. 선정 아이템====
        tabPanel(
          title = "선정 아이템",
          icon=icon('square-check'),
          tableOutput('selected_item')
        ),
        ###b. 통계표 조회====
        tabPanel(
          title = "통계표 조회",
          icon=icon('table-list'),
          fluidRow(
            column(3,textInput('name','검색어')),
            column(9,tableOutput('ecos_stat_tables'))
          )
        ),
        ###c. 아이템 추가====
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

  w1 <- Waiter$new(
    id=c('pf_box1','trading_box', 'ticker_box', 'list_box'),
    html = tagList(spin_loader(), "로딩중..."),
    color = transparent(.5))
  
  w1$show()
  source("functions.R", echo=F)
  ec <- Ecos$new()
  md <- MyData$new('mydata.sqlite')
  ma <- MyAssets$new()


    
  # 0) 반응성 값 초기화====
  rv <-  reactiveValues(
    name_in='전체',
    code='전체', df=NULL, df2=NULL, 
    df3=NULL, df4=NULL, df5=NULL,
    df_s=ec$read_items(), df_d=NULL, 
    tickers=NULL, ticker_new=NULL,
    trade=NULL, trade_new_=NULL,
    type1=NULL, type2=NULL,
    inflow=NULL, inflow_new=NULL,
    ctg=readRDS("categories.rds"),
    
  )
  
  w1$hide()
  
  # 1) 자산운용 내역 기록====
  ## a. 투자자산 거래내역====
  ### * 메뉴 설정====

  
  output$manage_ass_trade <- renderUI({
    fluidRow(
      column(
        width = 1,
        selectInput(
          inputId = 'type2',
          label = "운용구분",
          choices = c("투자자산", "연금자산")
        ),
        selectInput(
          inputId = 'ass_account2',
          label = "계좌",
          choices = unique(md$read('assets')$계좌)
        )
      ),
      column(
        width = 1,
        selectInput(
          inputId = 'ass_cur2',
          label = "통화",
          choices = unique(md$read('assets')$통화)
        ),
        selectInput(
          inputId = 'new2',
          label = "신규/수정",
          choices = "신규"
        )
      ),
      column(
        width = 1,
        airDatepickerInput(
          inputId = 'trading_date',
          label = "거래일자",
          addon = "none",
          value = Sys.Date()
        ),
        selectInput(
          inputId = 'ass_name2',
          label = "종목명",
          choices = NULL
        )
      ),
      column(
        width = 1,
        numericInput(
          inputId = 'buy_q',
          label = "매입수량",
          value = 0
        ),
        numericInput(
          inputId = 'sell_q',
          label = "매도수량",
          value = 0
        )
      ),
      column(
        width = 2,
        autonumericInput(
          inputId = 'buy_p',
          label = "매입액",
          value = 0
        ),
        autonumericInput(
          inputId = 'sell_b',
          label = "매도원금",
          value = 0
        )
      ),
      column(
        width = 2,
        autonumericInput(
          inputId = 'buy_c',
          label = "현금지출",
          value = 0
        ),
        autonumericInput(
          inputId = 'sell_p',
          label = "매도액",
          value = 0
        )
      ),
      column(
        width = 2,
        autonumericInput(
          inputId = 'int_dev',
          label = "이자배당액",
          value = 0
        ),
        autonumericInput(
          inputId = 'sell_c',
          label = "현금수입",
          value = 0
        )
      ),
      column(
        width = 2,
        autonumericInput(
          inputId = 'in_out_c',
          label = "입출금",
          value = 0
        )
      )
    )
  })

  
  ### * 테이블 설정====
  output$trade_table <- renderUI({
    if(!is.null(rv$trade)){
        rv$trade |> 
          arrange(desc(행번호)) |> 
          flextable()|>
          theme_vanilla() |>
          set_table_properties(layout='autofit') |>
          htmltools_value(ft.align = 'center')
    } else {
    }
  })

  reset_trade <- reactive({
    input$ass_trade_new
    input$ass_trade_mod
    input$ass_trade_del
    ma$get_trading_record(input$type2, 
                          input$ass_account2,
                          input$ass_cur2)
  })
  
  update_new_trade <- reactive({
    updateSelectInput(session, 'new2',
                      choices = c('신규', rev(rv$trade$행번호)),
                      selected = '신규')
    updateSelectInput(
      session, 'ass_name2', 
      choices = (md$read(rv$type2) |> 
                   filter(계좌==input$ass_account2,
                          통화==input$ass_cur2))$종목명)
  })
  
  ### * 운용구분 설정====
  
  observeEvent(input$type2,{
    if(input$type2 == "투자자산"){
      rv$type2 <- 'assets'
    } else {
      rv$type2 <- 'pension'
    }
    updateSelectInput(session, 'ass_account2',
                      choices = unique(md$read(rv$type2)$계좌))
    rv$trade <- reset_trade()
    update_new_trade()
  })
  
  observeEvent(input$ass_account2,{

    updateSelectInput(
      session, 'ass_cur2',
      choices = unique((md$read(rv$type2) |> 
                          filter(계좌==input$ass_account2))$통화)
    )
    rv$trade <- reset_trade()
    update_new_trade()
  })
  
  observeEvent(input$ass_cur2,{
    rv$trade <- reset_trade()
    update_new_trade()
  })
  
  ### * 신규/구분 설정====
  observeEvent(input$new2,{
    if(input$new2 != "신규"){
      t_rows2 <- filter(rv$trade, 행번호 == input$new2)
      updateAirDateInput(session, 'trading_date', value = t_rows2$거래일자)
      updateSelectInput(session, 'ass_name2', selected = t_rows2$종목명)
      updateNumericInput(session, 'buy_q', value = t_rows2$매입수량)
      updateNumericInput(session, 'sell_q', value = t_rows2$매도수량)
      updateAutonumericInput(session, 'buy_p', value = t_rows2$매입액)
      updateAutonumericInput(session, 'sell_b', value = t_rows2$매도원금)
      updateAutonumericInput(session, 'buy_c', value = t_rows2$현금지출)
      updateAutonumericInput(session, 'sell_p', value = t_rows2$매도액)
      updateAutonumericInput(session, 'int_dev', value = t_rows2$이자배당액)
      updateAutonumericInput(session, 'sell_c', value = t_rows2$현금수입)
      updateAutonumericInput(session, 'in_out_c', value = t_rows2$입출금)
    } else{
      updateAirDateInput(session, 'trading_date', value=Sys.Date())
      updateNumericInput(session, 'buy_q', value = 0)
      updateNumericInput(session, 'sell_q', value = 0)
      updateAutonumericInput(session, 'buy_p', value = 0)
      updateAutonumericInput(session, 'sell_b', value = 0)
      updateAutonumericInput(session, 'buy_c', value = 0)
      updateAutonumericInput(session, 'sell_p', value = 0)
      updateAutonumericInput(session, 'int_dev', value = 0)
      updateAutonumericInput(session, 'sell_c', value = 0)
      updateAutonumericInput(session, 'in_out_c', value = 0)
    }
  })
  
  ### * 추가/수정/삭제 선택시====
  observe({
    if(!is.null(input$ass_name2)){
      trade_ticker <- 
        bind_rows(md$read('assets'), md$read('pension')) |> 
        filter(계좌 == input$ass_account2,
               통화 == input$ass_cur2,
               종목명 == input$ass_name2) |> 
        pull(종목코드)
      
      if(length(trade_ticker)>0){
        rv$trade_new <- tibble::tibble_row(
          행번호=0, 거래일자=input$trading_date, 
          계좌=input$ass_account2,
          종목코드=trade_ticker,
          매입수량=input$buy_q, 매입액=input$buy_p, 
          현금지출=input$buy_c, 매도수량=input$sell_q, 
          매도원금=input$sell_b, 매도액=input$sell_p,
          이자배당액=input$int_dev, 현금수입=input$sell_c,
          입출금=input$in_out_c
        )
      }
    }
    
    
  })

  observeEvent(input$ass_trade_new,{
    if(input$type2 == "투자자산"){
      rv$trade_new$행번호 <- tail(md$read('assets_daily')$행번호, 1)+1
      dbxInsert(md$con, 'assets_daily', rv$trade_new)
    } else {
      rv$trade_new$행번호 <- tail(md$read('pension_daily')$행번호, 1)+1
      dbxInsert(md$con, 'pension_daily', rv$trade_new)
    }
    rv$trade <- reset_trade()
    update_new_trade()
  })


  observeEvent(input$ass_trade_mod,{
    rv$trade_new$행번호 <- input$new2
    if(input$type2 == "투자자산"){
      dbxUpdate(md$con, 'assets_daily', rv$trade_new, 
                where_cols = c("행번호"))

    } else {
      dbxUpdate(md$con, 'pension_daily', rv$trade_new, 
                where_cols = c("행번호"))
    }
    rv$trade <- reset_trade()
    update_new_trade()
  })

  observeEvent(input$ass_trade_del,{
    rv$trade_new$행번호 <- input$new2
    if(input$type2 == "투자자산"){
      dbxDelete(md$con, 'assets_daily', rv$trade_new)
    } else {
      dbxDelete(md$con, 'pension_daily', rv$trade_new)
    }
    rv$trade <- reset_trade()
    update_new_trade()
  })

  ## b. 투자종목 관리====
  
  ### * 메뉴 설정====
  output$manage_ticker <- renderUI({
    fluidRow(
      column(
        width = 2,
        selectInput(
          inputId = 'type1',
          label = "운용구분",
          choices = c("투자자산", "연금자산")
        ),
        selectInput(
          inputId = 'ass_account',
          label = "계좌",
          choices = rv$ctg$ass_account
        )
      ),
      column(
        width = 2,
        selectInput(
          inputId = 'new1',
          label = "신규/수정",
          choices = "신규"
        )
        ,
        textInput(
          inputId = 'ticker',
          label = "종목코드",
          value = ""
        )
      ),
      column(
        width = 2,
        textInput(
          inputId = 'ass_name',
          label = "종목명",
          value = ""
        ),
        textInput(
          inputId = 'comm_name',
          label = "상품명",
          value = ""
        )
      ),
      column(
        width = 2,
        selectInput(
          inputId = 'ass_class',
          label = "자산군",
          choices = rv$ctg$ass_class
        ),
        selectInput(
          inputId = 'ass_class1',
          label = "세부자산군",
          choices = rv$ctg$ass_class1
        )
      ),
      column(
        width = 2,
        selectInput(
          inputId = 'ass_class2',
          label = "세부자산군2",
          choices = rv$ctg$ass_class2
        ),
        selectInput(
          inputId = 'ass_cur',
          label = "통화",
          choices = rv$ctg$ass_cur
        )
      ),
      column(
        width = 2,
        autonumericInput(
          inputId = 'eval_price',
          label = "평가금액",
          value = 0
        ),
        autonumericInput(
          inputId = 'init_e_pl',
          label = "기초평가손익",
          value = 0
        )
      )
    )
  })

  ### * 테이블 설정====
  output$ticker_table <- renderUI({
    if(!is.null(rv$tickers)){
      rv$tickers |> 
        arrange(desc(행번호)) |> 
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        htmltools_value(ft.align = 'center')
    } else {
    }
  })

  reset_ticker <- reactive({
    input$ticker_new
    input$ticker_mod
    input$ticker_del
    if(input$type1 == "투자자산"){
      md$read('assets') |> 
        filter(계좌==input$ass_account)
    } else {
      md$read('pension') |> 
        filter(계좌==input$ass_account)}
  })
  
  update_manage_ticker <-  reactive({
    updateSelectInput(session, 'new1',
                      choices = c('신규', rev(rv$tickers$행번호)),
                      selected = '신규')
  })
    
  ### * 운용구분 설정====

  
  observeEvent(input$type1,{
    if(input$type1 == "투자자산"){
      mode <- 'ass_account'
    } else {
      mode <- 'pen_account'
    }
    updateSelectInput(session, 'ass_account',
                      choices = rv$ctg[[mode]])
    rv$tickers <- reset_ticker()
    update_manage_ticker()
  })
  
  observeEvent(input$ass_account,{
    rv$tickers <- reset_ticker()
    update_manage_ticker()
  })
  
  
  ### * 신규/구분 설정====
  observeEvent(input$new1,{
    if(input$new1 != "신규"){
      t_rows <- filter(rv$tickers, 행번호 == input$new1)
      
      # updateSelectInput(session, 'new1',
      #                   choices = c('신규', rev(rv$tickers$행번호)),
      #                   selected = input$new1)
      updateSelectInput(session, 'ass_account', selected = t_rows$계좌)
      
      updateTextInput(session, 'ticker', value = t_rows$종목코드)
      updateTextInput(session, 'ass_name', value = t_rows$종목명)
      updateTextInput(session, 'comm_name', value = t_rows$상품명)
      
      updateSelectInput(session, 'ass_class', selected = t_rows$자산군)
      updateSelectInput(session, 'ass_class1',
                        selected = t_rows$세부자산군)
      updateSelectInput(session, 'ass_class2',
                        selected = t_rows$세부자산군2)
      
      updateSelectInput(session, 'ass_cur', selected = t_rows$통화)
      updateAutonumericInput(session, 'eval_price',
                             value = t_rows$평가금액)
      updateAutonumericInput(session, 'init_e_pl',
                             value = t_rows$기초평가손익)
    }
    else{
      
      updateSelectInput(session, 'ass_account', selected = NULL)
      
      updateTextInput(session, 'ticker', value = '')
      updateTextInput(session, 'ass_name', value = '')
      updateTextInput(session, 'comm_name', value = '')
      
      updateSelectInput(session, 'ass_class', selected = NULL)
      updateSelectInput(session, 'ass_class1',
                        selected = '')
      updateSelectInput(session, 'ass_class2',
                        selected = '')
      
      updateSelectInput(session, 'ass_cur', selected = NULL)
      updateAutonumericInput(session, 'eval_price',
                             value = 0)
      updateAutonumericInput(session, 'init_e_pl',
                             value = 0)
      
    }
  })
  
  ### * 추가/수정/삭제 선택시====
  observe({
    rv$ticker_new <- tibble::tibble_row(
      행번호=0, 계좌=input$ass_account, 
      종목코드=input$ticker,
      종목명=input$ass_name, 평가금액=input$eval_price,
      상품명=input$comm_name, 통화=input$ass_cur, 
      자산군=input$ass_class, 
      세부자산군=input$ass_class1, 세부자산군2=input$ass_class2,
      기초평가손익=input$init_e_pl
    )
  })
  
  observeEvent(input$ticker_new,{
    
    if(input$type1 == "투자자산"){
      rv$ticker_new$행번호 <- tail(md$read('assets')$행번호, 1)+1
      dbxInsert(md$con, 'assets', rv$ticker_new)
    } else {
      rv$ticker_new$행번호 <- tail(md$read('pension')$행번호, 1)+1
      dbxInsert(md$con, 'pension', rv$ticker_new)
    }

    rv$tickers <- reset_ticker()
    update_manage_ticker()
  })
  
  
  observeEvent(input$ticker_mod,{
    rv$ticker_new$행번호 <- input$new1
    if(input$type1 == "투자자산"){
      dbxUpdate(md$con, 'assets', rv$ticker_new, where_cols = c("행번호"))
    } else {
      dbxUpdate(md$con, 'pension', rv$ticker_new, where_cols = c("행번호"))
    }
    rv$tickers <- reset_ticker()
    update_manage_ticker()
  })
  
  observeEvent(input$ticker_del,{
    rv$ticker_new$행번호 <- input$new1
    if(input$type1 == "투자자산"){
      dbxDelete(md$con, 'assets', rv$ticker_new)
    } else {
      dbxDelete(md$con, 'pension', rv$ticker_new)
    }
    rv$tickers <- reset_ticker()
    update_manage_ticker()
  })
  
  
  ## c. 구분항목 관리====
  
  ass_ctg <- list('ass_account', 'pen_account', 'ass_cur','ass_class',
                  'ass_class1','ass_class2')
  
  ctg_kor <- list('투자계좌', '연금계좌', '통화','자산군',
                  '세부자산군','세부자산군2')
  
  map2(ass_ctg, ctg_kor, function(i, j){
    output[[paste0(i,'_list')]] <<- renderUI({
      tagList(
        textInput(
          inputId = paste0('add_',i),
          label = j,
          value = ""
        ),
        selectInput(paste0('select_',i), NULL, rv$ctg[[i]]),
        div(
          actionButton(
            inputId = glue("add_{i}_btn"),
            label = "추가",
            width = '45%', 
            status = "info"
          ),
          actionButton(
            inputId = glue("del_{i}_btn"),
            label = "삭제",
            width = '45%', 
            status = "primary"
          ),
          align = 'center'
        )
      )
    })

    observeEvent(input[[glue("add_{i}_btn")]],{
      rv$ctg[[i]] <- c(rv$ctg[[i]], 
                       input[[glue("add_{i}")]])
      saveRDS(rv$ctg, 'categories.rds')
    })
    
    observeEvent(input[[glue("del_{i}_btn")]],{
      
      rv$ctg[[i]] <- rv$ctg[[i]][
        rv$ctg[[i]]!=input[[glue("select_{i}")]]
      ]
      
      saveRDS(rv$ctg, 'categories.rds')
    })
  })
  
  render_allo <- function(df){
    renderUI({
      df |> flextable() |>
        theme_vanilla() |>
        merge_v(j=1:2) |>
        set_table_properties(layout='autofit',width=0.9) |>
        htmltools_value()
    })
  }  
  
  # 2) 자산운용 현황====
  
  observeEvent(input$kis,{
    
    w1$show()
    
    ma$run_valuation()
    
    w1$hide()

    
   ## a. 투자자산현황====

    ###* 자산군별 배분현황====
    output$allo0 <- render_allo(ma$allo0)
    output$allo1 <- render_allo(ma$allo1)
    
    ###* 통화별 배분현황====
    output$allo2 <- render_allo(ma$allo2)
    output$allo3 <- render_allo(ma$allo3)
    
    ###* 불리오 배분현황====
    output$allo5 <- render_allo(ma$allo5)
    output$allo4 <- render_allo(ma$allo4)
   

    ## b. 투자손익현황====

    
    output$class_ret_a <- renderUI({
      ma$ret_a |>
        select(1:3,평가금액,실현손익, 평가손익,
               실현수익률:평가수익률) |>
        flextable() |>
        theme_vanilla() |>
        merge_v(j=1:2) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=4:6, digits = 0) |>
        colformat_double(j=7:8, digits = 2) |>
        htmltools_value()
    })
    
    output$class_ret_a2 <- renderUI({
      ma$ret_a2 |>
        select(1:2,평가금액,실현손익, 평가손익,
               실현수익률:평가수익률) |>
        flextable() |>
        theme_vanilla() |>
        merge_v(j=1) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=3:5, digits = 0) |>
        colformat_double(j=6:7, digits = 2) |>
        htmltools_value()
    })

    output$bs_pl_mkt_a <-renderUI({
      ma$bs_pl_mkt_a |>
        select(통화, 자산군, 세부자산군, 종목명,
               보유수량,장부금액, 평가금액, 실현손익,평가손익,
               실현수익률, 평가수익률) |>
        arrange(통화,자산군,세부자산군) |>
        flextable() |>
        theme_vanilla() |>
        merge_v(j=1:3) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=5:9, digits = 0) |>
        colformat_double(j=10:11, digits = 2) |>
        htmltools_value()
    })

  })
  
  ## c. 연금자산현황====
  output$allo6 <- render_allo(ma$allo6)
  output$allo7 <- render_allo(ma$allo7)
  output$allo8 <- render_allo(ma$allo8)
  output$allo9 <- render_allo(ma$allo9)
  
  ## d. 연금손익현황====
  
  output$class_ret_p <- renderUI({
    ma$ret_p |>
      select(1:3,평가금액,실현손익, 평가손익,
             실현수익률:평가수익률) |>
      flextable() |>
      theme_vanilla() |>
      merge_v(j=1:2) |>
      set_table_properties(layout='autofit') |>
      colformat_double(j=4:6, digits = 0) |>
      colformat_double(j=7:8, digits = 2) |>
      htmltools_value()
  })
  
  output$class_ret_p2 <- renderUI({
    ma$ret_p2 |>
      select(1:2,평가금액,실현손익, 평가손익,
             실현수익률:평가수익률) |>
      flextable() |>
      theme_vanilla() |>
      merge_v(j=1) |>
      set_table_properties(layout='autofit') |>
      colformat_double(j=3:5, digits = 0) |>
      colformat_double(j=6:7, digits = 2) |>
      htmltools_value()
  })
  
  output$bs_pl_mkt_p <-renderUI({
    ma$bs_pl_mkt_p |>
      select(계좌, 자산군, 세부자산군, 종목명,
             보유수량, 장부금액, 평가금액, 실현손익, 평가손익,
             실현수익률,평가수익률) |>
      arrange(계좌,자산군,세부자산군) |>
      flextable() |>
      theme_vanilla() |>
      merge_v(j=1:3) |>
      set_table_properties(layout='autofit') |>
      colformat_double(j=5:9, digits = 0) |>
      colformat_double(j=10:11, digits = 2) |>
      htmltools_value()
  })
  
  # 3) 자산/유동성 추이====
  
  ### * 메뉴 설정====
  output$manage_inflow <- renderUI({
    fluidRow(
      selectInput(
        inputId = 'new3',
        label = "신규/수정",
        choices = "신규",
        width='100%'
      ),
      airDatepickerInput(
          inputId = 'trading_date2',
          label = "거래일자",
          addon = "none",
          value = Sys.Date(),
          width='100%'
      ),
      autonumericInput(
        inputId = 'net_inflow',
        label = "순자금유입",
        value = 0,
        width='100%'
      ),
      autonumericInput(
        inputId = 'payment',
        label = "만기상환",
        value = 0,
        width='100%'
      ),
      br(),
      actionButton(
        inputId = "inflow_new",
        label = "추가",
        status = "info",
        width='100%'
      ),
      br(),
      actionButton(
        inputId = "inflow_mod",
        label = "수정",
        status = "success",
        width='100%'
      ),
      br(),
      actionButton(
        inputId = "inflow_del",
        label = "삭제",
        status = "primary",
        width='100%'
      )
    ) 
  })
  
  ### * 테이블 설정====
  
  reset_inflow <- reactive({
    input$inflow_new
    input$inflow_mod
    input$inflow_del
    md$read('inflow')
  })
  
  update_manage_inflow <-  reactive({
    updateSelectInput(session, 'new3',
                      choices = c('신규', rev(rv$inflow$행번호)),
                      selected = '신규')
  })
  
  output$inflow_table <- renderUI({
    if(!is.null(rv$inflow)){
      rv$inflow |>
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        htmltools_value(ft.align = 'center')
    } else {
    }
  })
  
  ### * 신규/구분 설정====
  observeEvent(input$new3,{
    if(input$new3 != "신규"){
      rv$inflow <- reset_inflow()
      t_rows <- filter(rv$inflow, 행번호 == input$new3)
      updateAirDateInput(session, 'trading_date2', value = t_rows$거래일자)
      updateAutonumericInput(session, 'net_inflow', value = t_rows$순자금유입)
      updateAutonumericInput(session, 'payment', value = t_rows$만기상환)
      
    } else {
      rv$inflow <- reset_inflow()
      update_manage_inflow()
      updateAirDateInput(session, 'trading_date2', value = Sys.Date())
      updateAutonumericInput(session, 'net_inflow', value = 0)
      updateAutonumericInput(session, 'payment', value = 0)
    }
  })
  
  ### * 추가/수정/삭제 선택시====
  observe({
    rv$inflow_new <- tibble::tibble_row(
      행번호=0, 거래일자 = input$trading_date2,
      순자금유입=input$net_inflow,
      만기상환=input$payment
    )
  })
  
  observeEvent(input$inflow_new,{
    
    rv$inflow_new$행번호 <- tail(md$read('inflow')$행번호, 1)+1
    dbxInsert(md$con, 'inflow', rv$inflow_new)
    rv$inflow <- reset_inflow()
    update_manage_inflow()
  })
  
  observeEvent(input$inflow_mod,{
    rv$inflow_new$행번호 <- input$new3
    dbxUpdate(md$con, 'inflow', rv$inflow_new, where_cols = c("행번호"))
    rv$inflow <- reset_inflow()
    update_manage_inflow()
  })
  
  observeEvent(input$inflow_del,{
    dbxDelete(md$con, 'inflow', tibble::tibble_row(행번호=input$new3))
    rv$inflow <- reset_inflow()
    update_manage_inflow()
  })
  
  ### * 그래프 설정====
  output$inflow_plot <- renderPlot({
    ma$inflow_plot +
      theme(
        text = element_text(size = 20),
        legend.position = "none",
        axis.title = element_blank()
      )
  })
  
  
  
  # 4) 한국은행 지표선정====
  ## b. 통계표 조회====
  observeEvent(input$name,{
    rv$df <- ec$find_stat(input$name)
    updateSelectizeInput(session, 'name_in', 
                         choices = c('전체',rv$df$stat_name),
                         selected = '전체')
    
  })
  
  ## c. 아이템 추가==== 
  
  ### * 통계표 이름==== 
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
  
  ### * 통계표 코드==== 
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
  
  ### * 아이템이름==== 
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
  
  ### * 데이터주기====    
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
  
  ## * 아이템 추가====
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
  
  
  observeEvent(input$close_win,{
    js$closeWindow()
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
