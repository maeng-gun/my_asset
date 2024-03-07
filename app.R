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
    sidebarHeader("경제지표"),
    menuItem(
      text = "한국은행 지표선정",
      icon = icon("hand-pointer"),
      tabName = "ecos_stat"
    ),
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
  tabItems(
    ##1) 한국은행 지표선정====
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
    ),
    ##2) 자산운용 내역 기록====
    tabItem(
      tabName = 'trading_record',
      tabBox(
        width=12,
        status='primary',
        type='tabs',

        ###a. 투자종목 관리====
        tabPanel(
          title="투자종목 관리",
          fluidRow(
            box(
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
            uiOutput("ticker_table")
          )
        ),
        
        ###b. 투자자산 거래내역====
        tabPanel(
          title="투자자산 거래내역",
          fluidRow(
            box(
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
        
        ###c. 연금자산 거래내역====
        tabPanel(
          title="연금자산 거래내역"
        ),
        
        ###d. 구분항목 관리====
        tabPanel(
          title="구분항목 관리",
          fluidRow(
            column(
              width = 2,
              uiOutput('ass_account_list')
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
            ),
            column(
              width = 2
            )
          )
        )
      )
    ),
    ##3) 자산운용 현황====
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
            uiOutput("class_ret_a")
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
            uiOutput("class_ret_p")
          ),
          br(),
          h5("2. 개별자산 손익현황"),
          br(),
          fluidRow(
            uiOutput("bs_pl_mkt_p")
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
    ctg=readRDS("categories.rds"),
    
  )
  
  # 1) 한국은행 지표선정====
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

  # 2) 자산운용 내역 기록====
  
  ## a. 투자종목 관리====
  
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
          inputId = 'new1',
          label = "신규/수정",
          choices = "신규"
        )
      ),
      column(
        width = 2,
        selectInput(
          inputId = 'ass_account',
          label = "계좌",
          choices = rv$ctg$ass_account
        ),
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
      rv$tickers |> flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        htmltools_value()
    } else {
    }
  })
  
  ### * 운용구분 설정====
  
  update_manage_ticker <-  reactive({
    
    if(input$type1 == "투자자산"){
      rv$tickers <- md$read('assets')}
    else {
      rv$tickers <- md$read('pension')}
    
    updateSelectInput(session, 'new1',
                      choices = c('신규', rev(rv$tickers$행번호)),
                      selected = '신규')
    })

  
  observeEvent(input$type1,{
    update_manage_ticker()
  })


  ### * 신규/구분 설정====
  observeEvent(input$new1,{
    if(input$new1 != "신규"){
      t_rows <- filter(rv$tickers, 행번호 == input$new1)

      updateSelectInput(session, 'new1',
                        choices = c('신규', rev(rv$tickers$행번호)),
                        selected = input$new1)
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
      행번호=0, 계좌=input$ass_account, 종목코드=input$ticker,
      종목명=input$ass_name, 평가금액=input$eval_price,
      상품명=input$comm_name, 통화=input$ass_cur, 
      자산군=input$ass_class, 
      세부자산군=input$ass_class1, 세부자산군2=input$ass_class2,
      기초평가손익=input$init_e_pl
    )
  })
    
  observeEvent(input$ticker_new,{
    rv$ticker_new$행번호 <- tail(rv$tickers$행번호, 1)+1
    if(input$type1 == "투자자산"){
      dbxInsert(md$con, 'assets', rv$ticker_new)
      rv$tickers <- md$read('assets')
    } else {
      dbxInsert(md$con, 'pension', rv$ticker_new)
      rv$tickers <- md$read('pension')
    }
    updateSelectInput(session, 'new1',
                      choices = c('신규', rev(rv$tickers$행번호)),
                      selected = '신규')
  })

    
  observeEvent(input$ticker_mod,{
    rv$ticker_new$행번호 <- input$new1
    if(input$type1 == "투자자산"){
      dbxUpdate(md$con, 'assets', rv$ticker_new, where_cols = c("행번호"))
      rv$tickers <- md$read('assets')
    } else {
      dbxUpdate(md$con, 'pension', rv$ticker_new, where_cols = c("행번호"))
      rv$tickers <- md$read('pension')
    }
    updateSelectInput(session, 'new1',
                      choices = c('신규', rev(rv$tickers$행번호)),
                      selected = '신규')
  })
    
  observeEvent(input$ticker_del,{
    rv$ticker_new$행번호 <- input$new1
    if(input$type1 == "투자자산"){
      dbxDelete(md$con, 'assets', rv$ticker_new)
      rv$tickers <- md$read('assets')
    } else {
      dbxDelete(md$con, 'pension', rv$ticker_new)
      rv$tickers <- md$read('pension')
    }
    updateSelectInput(session, 'new1',
                      choices = c('신규', rev(rv$tickers$행번호)),
                      selected = '신규')
  })

  ## b. 투자자산 거래내역====
  
  # 운용구분  신규/수정 
  # 계좌 통화
  # [1] "거래일자"   "종목명"    
  # [4] "매입수량"  "매입액"      "현금지출" "이자배당액"  "입출금" 
      #     
  # [7] "매도수량"  "매도원금"    "매도액"  "현금수입"   
  # [10]  
      #  
  
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
          inputId = 'new2',
          label = "신규/수정",
          choices = "신규"
        )
      ),
      column(
        width = 1,
        selectInput(
          inputId = 'ass_account2',
          label = "계좌",
          choices = rv$ctg$ass_account
        ),
        selectInput(
          inputId = 'ass_cur2',
          label = "통화",
          choices = rv$ctg$ass_cur
        )
      ),
      column(
        width = 1,
        airDatepickerInput(
          inputId = 'trading_date',
          label = "거래일자",
          addon = "none"
        ),
        selectInput(
          inputId = 'ass_name2',
          label = "종목명",
          choices = ""
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
        rv$trade |> flextable()|>
          theme_vanilla() |>
          set_table_properties(layout='autofit') |>
          htmltools_value(ft.align = 'center')
    } else {
    }
  })

  ### * 운용구분 설정====
  # update_manage_trade <-  reactive({
  #   
  #   if(input$type2 == "투자자산"){
  #     rv$trade <- md$read('assets_daily')}
  #   else {
  #     rv$trade <- md$read('pension')}
  # 
  #   # updateSelectInput(session, 'new2',
  #   #                   choices = c('신규', rev(rv$trade$행번호)),
  #   #                   selected = '신규')
  # })
  
  observeEvent(input$type2,{
    if(input$type2 == "투자자산"){
      rv$trade <- ma$read('assets_daily')}
    else {
      rv$trade <- md$read('pension_daily')}
  })
  
  ## d. 구분항목 관리====
  
  ass_ctg <- list('ass_account','ass_cur','ass_class',
                  'ass_class1','ass_class2')
  
  ctg_kor <- list('계좌','통화','자산군',
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
  
  
  # 3) 자산운용 현황====
  
  observeEvent(input$kis,{
    w1 <- Waiter$new(
      id='pf_box1',
      html = tagList(spin_loader(), "로딩중..."),
      color = transparent(.5))
    
    w1$show()
    
    ma$run_valuaion()
    
    w1$hide()
  
    render_allo <- function(df){
      renderUI({
        df |> flextable() |> 
          theme_vanilla() |> 
          merge_v(j=1:2) |>
          set_table_properties(layout='autofit',width=0.9) |> 
          htmltools_value()
      })
    }
    
    
    ## a. 투자자산현황====
    
    output$allo0 <- render_allo(ma$allo0)
    output$allo1 <- render_allo(ma$allo1)
    output$allo2 <- render_allo(ma$allo2)
    output$allo3 <- render_allo(ma$allo3)
    output$allo4 <- render_allo(ma$allo4)
    output$allo5 <- render_allo(ma$allo5)
    
    # b. 투자손익현황====
    
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
    
    output$bs_pl_mkt_a <-renderUI({
      ma$bs_pl_mkt_a |> 
        select(통화, 자산군, 세부자산군, 종목명,
               보유수량,평가금액, 실현손익,평가손익,
               실현수익률, 평가수익률) |>
        arrange(통화,자산군,세부자산군) |> 
        flextable() |> 
        theme_vanilla() |> 
        merge_v(j=1:3) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=5:8, digits = 0) |>
        colformat_double(j=9:10, digits = 2) |> 
        htmltools_value()
    })
    
    # c. 투자자산현황====
    output$allo6 <- render_allo(ma$allo6)
    output$allo7 <- render_allo(ma$allo7)
    output$allo8 <- render_allo(ma$allo8)
    output$allo9 <- render_allo(ma$allo9)
    
    # b. 투자손익현황====
    
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
    
    output$bs_pl_mkt_p <-renderUI({
      ma$bs_pl_mkt_p |> 
        select(계좌, 자산군, 세부자산군, 종목명,
               보유수량,평가금액, 실현손익, 평가손익,
               실현수익률,평가수익률) |>
        arrange(계좌,자산군,세부자산군) |> 
        flextable() |> 
        theme_vanilla() |> 
        merge_v(j=1:3) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=5:8, digits = 0) |>
        colformat_double(j=9:10, digits = 2) |> 
        htmltools_value()
    })
    
  })
  
  
  observeEvent(input$close_win,{
    js$closeWindow()
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
