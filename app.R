library(shiny)
library(bs4Dash)
library(waiter)
library(flextable)
library(shinyWidgets)
library(lubridate)
library(shiny.pwa)
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
    # airDatepickerInput(
    #   inputId = 'base_date',
    #   label = "기준일",
    #   addon = "none",
    #   value = Sys.Date()
    # ),
    actionButton('reval', '평가금액 재계산'),
    actionButton('kis','주가 업데이트'),
    br(),
    sidebarHeader("포트폴리오 관리"),
    menuItem(
      text = "자산운용 내역 기록",
      icon = icon('receipt'),
      tabName = 'trading_record'
    ),
    menuItem(
      text = "통합 자산운용 현황",
      icon = icon("sack-dollar"),
      tabName = "pf_total",
      selected = T
    ),
    menuItem(
      text = "계좌별 자산운용 현황",
      icon = icon("sack-dollar"),
      tabName = "pf_bs_pl"
    ),
    menuItem(
      text = "유동성 관리",
      icon = icon("chart-line"),
      tabName = "pf_liquid"
    ),
    # sidebarHeader("경제지표"),
    # menuItem(
    #   text = "한국은행 지표선정",
    #   icon = icon("hand-pointer"),
    #   tabName = "ecos_stat"
    # ),
    menuItem(
      text = "자산배분 현황",
      icon = icon("chart-pie"),
      tabName = "asset_allo"
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
  pwa(
    domain = "https://hailey-family.shinyapps.io/my_asset/",
    title = "가족자산관리",
    output = "www",
    icon = "www/3890929_chart_growth_invest_market_stock_icon.png"
  ),
  useShinyjs(),
  extendShinyjs(
    text = "
      shinyjs.closeWindow = function() { window.close(); }
      shinyjs.enterToClick = function(params) {
        var inputId = params.inputId;
        var buttonId = params.buttonId;
        $(document).on('keydown', '#' + inputId, function (e) {
          if (e.keyCode == 13) {
            e.preventDefault();
            $('#' + buttonId).click();
          }
        });
      }
    ",
    functions = c("closeWindow", "enterToClick")
  ),
  useSweetAlert(),
  useWaiter(),
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
                    choices = NULL
                  )
                ),
                column(
                  width = 1,
                  selectInput(
                    inputId = 'ass_cur2',
                    label = "통화",
                    choices = NULL
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
              ),
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
        ),
        
        ###d. 종합거래내역====
        tabPanel(
          title="종합거래내역",
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
                airDatepickerInput(
                  inputId = 'total_trade_date',
                  label = "거래일자",
                  range = T,
                  addon = "none",
                  value = c(Sys.Date(), Sys.Date())
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = 'total_ass1',
                  label = "자산군",
                  choices = c("전체","대체자산","외화자산",
                              "주식","채권")
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = 'total_ass2',
                  label = "세부자산군",
                  choices = c("전체","부동산인프라","상품",
                              "달러자산","엔화자산","선진국",
                              "신흥국","간접","직접")
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = 'total_ass3',
                  label = "세부자산군2",
                  choices = c("전체","부동산","인프라",
                              "에너지","원자재","",
                              "섹터","인덱스","종목",
                              "신흥국","선진국")
                )
              ),
              column(
                width = 1,
                selectInput(
                  inputId = 'total_curr',
                  label = "통화",
                  choices = c("전체","원화","달러","엔화")
                )
              )
            )
          ),
          fluidRow(
            uiOutput("total_trade_table")
          )
        )
      )
    ),
    ##2) 통합 자산운용 현황====
    tabItem(
      tabName = 'pf_total',
      br(),
      tabBox(
        id='pf_box2',
        width=12,
        status='primary',
        type='tabs',
        
        ###a. 종합손익(그래프)====
        tabPanel(
          title="종합손익(그래프)",
          fluidRow(
            column(
              width = 2,
              airDatepickerInput(
                inputId = 'total_s_date',
                label = "시작일",
                addon = "none",
                value = make_date(year(Sys.Date()),1,1)-1
              ),
              airDatepickerInput(
                inputId = 'total_e_date',
                label = "종료일",
                addon = "none",
                value = Sys.Date()
              )
            ),
            column(
              width = 10,
              plotOutput("total_profit", height = "800px")
            )
            
          )
        ),
        ###b. 종합손익(테이블)====
        tabPanel(
          title="종합손익(테이블)",
          fluidRow(
            uiOutput("t_profit1")
          ),
          fluidRow(
            airDatepickerInput(
              inputId = 't_profit_date',
              label = "조회연월",
              addon = "none",
              view = 'months',
              value = Sys.Date()
            )
          ),
          fluidRow(
            uiOutput("t_profit2")
          )
        ),
        
        ###c. 자산군별 보유현황====
        tabPanel(
          title="자산군별 보유현황",
          fluidRow(
            uiOutput("t_asset_class")
          )
        ),
        ###d. 상품별 보유현황1====
        tabPanel(
          title="상품별 보유현황1",
          fluidRow(
            uiOutput("t_commodity")
          )
        ),
        ###e. 상품별 보유현황2====
        tabPanel(
          title="상품별 보유현황2",
          fluidRow(
            uiOutput("t_commodity2")
          )
        )
      )
    ),
    ##3) 계좌별 자산운용 현황====
    tabItem(
      tabName = 'pf_bs_pl',
      tabBox(
        id='pf_box1',
        width=12,
        status='primary',
        type='tabs',
        
        ###a. 투자자산현황====
        tabPanel(
          title="투자현황",
          fluidRow(
            uiOutput("total_accounts1")
          ),
          br(),
          fluidRow(
            uiOutput("total_accounts2")
          )
          # h5("1. 자산군별 배분현황"),
          # br(),
          # fluidRow(
          #   column(
          #     width = 4,
          #     uiOutput("allo0")
          #   ),
          #   column(
          #     width = 8,
          #     uiOutput("allo1")
          #   )
          # ),
          # h5("2. 통화별 배분현황"),
          # br(),
          # fluidRow(
          #   column(
          #     width = 4,
          #     uiOutput("allo2")
          #   ),
          #   column(
          #     width = 8,
          #     uiOutput("allo3")
          #   )
          # ),
          # h5("3. 불리오 배분현황"),
          # br(),
          # fluidRow(
          #   column(
          #     width = 4,
          #     uiOutput("allo5")
          #   ),
          #   column(
          #     width = 8,
          #     uiOutput("allo4")
          #   )
          # )
        ),
        ###b. 투자손익현황====
        tabPanel(
          title="투자손익현황",
          # h5("1. 자산군별 손익현황"),
          # br(),
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput("class_ret_a")
          #   ),
          #   column(
          #     width = 6,
          #     uiOutput("class_ret_a2")
          #   )
          # ),
          # br(),
          # h5("2. 개별자산 손익현황"),
          # br(),
          fluidRow(
            uiOutput("bs_pl_mkt_a")
          )
        )
        ###c. 연금현황====
        # tabPanel(
        #   title="연금현황",
        #   h5("1. 자산군별 배분현황"),
        #   br(),
        #   fluidRow(
        #     column(
        #       width = 4,
        #       uiOutput("allo6")
        #     ),
        #     column(
        #       width = 8,
        #       uiOutput("allo7")
        #     )
        #   ),
        #   h5("2. 계좌별 배분현황"),
        #   br(),
        #   fluidRow(
        #     column(
        #       width = 4,
        #       uiOutput("allo8")
        #     ),
        #     column(
        #       width = 8,
        #       uiOutput("allo9")
        #     )
        #   )
        # ),
        ###d. 연금손익현황====
        # tabPanel(
        #   title="연금손익현황",
        #   h5("1. 자산군별 손익현황"),
        #   br(),
        #   fluidRow(
        #     column(
        #       width = 6,
        #       uiOutput("class_ret_p")
        #     ),
        #     column(
        #       width = 6,
        #       uiOutput("class_ret_p2")
        #     )
        #   ),
        #   br(),
        #   h5("2. 개별자산 손익현황"),
        #   br(),
        #   fluidRow(
        #     uiOutput("bs_pl_mkt_p")
        #   )
        # )
      )
    ),
    
    ##4) 유동성 관리====
    tabItem(
      tabName = "pf_liquid",
      tabBox(
        id = 'liquid_tabs',
        width = 12,
        status = 'primary',
        type = 'tabs',
        
        ### a. 자금유출입 탭 ====
        tabPanel(
          title = "자금유출입",
          fluidRow(
            column(
              width = 3,
              box(
                width = 12,
                title = "입력사항",
                status = "info",
                solidHeader = TRUE,
                collapsible = F,
                uiOutput('manage_inflow')
              )
            ),
            column(
              width = 5,
              box(
                width = 12,
                title = "유출입 내역",
                status = "info",
                solidHeader = TRUE,
                collapsible = F,
                uiOutput('inflow_table1')
              )
            ),
            column(
              width = 4,
              box(
                width = 12,
                title = "만기도래내역",
                status = "info",
                solidHeader = TRUE,
                collapsible = F,
                uiOutput('maturity_table')
              )
            )
          )
        ),
        
        ### b. 총자산추이 탭 ====
        tabPanel(
          title = "총자산추이",
          fluidRow(
            box(
              width = 12,
              title = "총자산현황",
              status = "info",
              solidHeader = TRUE,
              collapsible = F,
              uiOutput('current_total_asset_table')
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "총자산추이",
              status = "info",
              solidHeader = TRUE,
              collapsible = F,
              uiOutput('inflow_table3')
            )
          )
        ),
        
        ### c. 가용자금추이 탭 ====
        tabPanel(
          title = "가용자금추이",
          fluidRow(
            box(
              width = 12,
              title = "현금성자산현황",
              status = "info",
              solidHeader = TRUE,
              collapsible = F,
              uiOutput('current_cash_asset_table')
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "가용자금추이",
              status = "info",
              solidHeader = TRUE,
              collapsible = F,
              uiOutput('inflow_table4')
            )
          )
        )
      )
    ),
      ##5) 자산배분 현황====
    tabItem(
      tabName = "asset_allo",
      tabBox(
        id = 'allo_tabs',
        width = 12,
        status = 'primary',
        type = 'tabs',
        
        ### 1번탭: 총 자산배분 (기존 화면) ====
        tabPanel(
          title = "총 자산배분",
          fluidRow(
            column(
              width = 2,
              box(
                width = 12,
                title = "목표 비중 설정",
                status = "info",
                solidHeader = TRUE,
                collapsible = FALSE,
                div("자산군", align = 'center'),
                numericInput(inputId = 'ass_bond', label = "채권", value = 0),
                numericInput(inputId = 'ass_stock', label = "주식", value = 0),
                numericInput(inputId = 'ass_alter', label = "대체투자", value = 0),
                br(),
                div("세부자산군", align = 'center'),
                numericInput(inputId = 'ass_bond_sol', label = "채권_국채", value = 0),
                numericInput(inputId = 'ass_bond_nr', label = "채권_만기무위험", value = 0),
                numericInput(inputId = 'ass_bond_cor', label = "채권_만기회사채", value = 0),
                numericInput(inputId = 'ass_bond_ig', label = "채권_투자등급", value = 0),
                numericInput(inputId = 'ass_stock_dev', label = "주식_신흥국", value = 0),
                numericInput(inputId = 'ass_alter_com', label = "대체투자_상품", value = 0),
                br(),
                actionButton(
                  inputId = "allo_renew",
                  label = "수정",
                  status = "info",
                  width = '100%'
                )
              ) # box 닫기 (쉼표 없음)
            ), # column 닫기 (쉼표 있음)
            column(
              width = 10,
              uiOutput('allocation_table')
            ) # column 닫기 (쉼표 없음)
          ) # fluidRow 닫기
        ), # tabPanel 닫기
        
        ### 2번탭: 계좌별 배분현황 (신규) ====
        tabPanel(
          title = "계좌별 배분현황",
          fluidRow(
            box(
              width = 12,
              status = "info",
              solidHeader = FALSE,
              uiOutput('account_allocation_table')
            )
          )
        )
      ) # tabBox 닫기
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
  
  show_delay <- function(text, type){
    show_alert(title=text, type=type)
  }
  
  # w1$show()
  source("functions.R", echo=F)
  # ec <- Ecos$new()
  
  
  auth_rv <- reactiveValues(authenticated = FALSE, pg_pass = NULL)
  
  login_modal <- modalDialog(
    title = "보안 접속",
    fluidRow(
      column(12,
             passwordInput("db_password_input", "데이터베이스 비밀번호를 입력하세요:", width = "100%"),
             br(),
             actionButton("login_button", "접속", status = "info", width = "100%"),
             br(),
             uiOutput("login_error_modal") # 비밀번호 오류 시 메시지 출력
      )
    ),
    footer = NULL,
    easyClose = FALSE # 사용자가 임의로 닫지 못하게 함
  )
  
  showModal(login_modal)
  
  js$enterToClick(inputId = "db_password_input", buttonId = "login_button")
  
  # 로그인 버튼 로직 ---
  observeEvent(input$login_button, {
    req(input$db_password_input)
    
    # Waiter로 연결 시도 중임을 표시
    w_modal <- Waiter$new(
      id = "login_button", # 버튼 위에 로딩 표시
      html = tagList(spin_loader(), "연결 시도 중..."),
      color = transparent(.5)
    )
    w_modal$show()
    
    # 1. 입력된 비밀번호로 Postgres 연결 테스트
    temp_con <- NULL
    cfg <- yaml::read_yaml(file = 'ccc.yaml',
                           readLines.warn = F)
    tryCatch({
      temp_con <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = cfg$c,
        port = 5432,
        dbname = "postgres",
        user = cfg$a,
        password = input$db_password_input # <--- 입력된 비밀번호 사용
      )
      
      # 2. 연결 성공 시
      DBI::dbDisconnect(temp_con) # 테스트 연결 종료
      auth_rv$pg_pass <- input$db_password_input # 성공한 비밀번호 저장
      auth_rv$authenticated <- TRUE # 인증 상태 변경
      
      w_modal$hide()
      removeModal() # 모달 창 닫기
      
    }, error = function(e) {
      # 3. 연결 실패 시
      w_modal$hide()
      output$login_error_modal <- renderUI({
        p("비밀번호가 올바르지 않거나 DB에 연결할 수 없습니다.", style = "color: red; margin-top: 10px;")
      })
    })
  })
  
  # 0) 반응성 값 초기화====
  
  observeEvent(auth_rv$authenticated, {
    req(auth_rv$authenticated == TRUE)
  
    show_delay("앱 구동중...", "info")
    
    sk_b <- reactiveVal(T)
    sk_v <- reactiveVal(T)
    sk_c <- reactiveVal(T)
    
    ma <- MyAssets$new(auth_rv$pg_pass)
    
    ma_b <- reactive({
      sk_b()
      ma$run_book()
      ma
    })
  
    observeEvent(input$reval,{
      w1$show()
      
      sk_v(!sk_v())
      w1$hide()
    })  
    
    observeEvent(input$kis,{
      w1$show()
      
      ma$update_new_price()
      sk_v(!sk_v())
      
      w1$hide()
    })  
    
    ma_v <- reactive({
      sk_v()
      ma$run_valuation()
      ma
    })
    
    ctg <- reactive({
      sk_c()
      df <- ma$read('categories')
      split(df$value, df$key)
    })
    
    
    
    
    rv_app <-  reactiveValues(
      name_in='전체',
      code='전체', df=NULL, df2=NULL, 
      df3=NULL, df4=NULL, df5=NULL, df_d=NULL, 
      tickers=NULL, ticker_new=NULL,
      trade=NULL, trade_new_=NULL,
      type2=NULL,
      initial_load_done = FALSE,
      inflow=NULL, inflow_new=NULL
      # ctg=readRDS("categories.rds")
    )
    
    
    # w1$hide()

    
    # 1) 자산운용 내역 기록====
    ## a. 투자자산 거래내역====
    ### * 메뉴 설정====
  
    ### * 테이블 설정====
    output$trade_table <- renderUI({
      if(!is.null(rv_app$trade)){
          rv_app$trade |> 
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
      ma_b()$get_trading_record(input$type2, 
                            input$ass_account2,
                            input$ass_cur2)
    })
    
    update_new_trade <- reactive({
      
      updateSelectInput(session, 'new2',
                        choices = c('신규', rev(rv_app$trade$행번호)),
                        selected = '신규')
      
      updateSelectInput(
        session, 'ass_name2', 
        choices = (ma_b()$read(rv_app$type2) |> 
                     filter(계좌==input$ass_account2,
                            통화==input$ass_cur2))$종목명)
    })
    
    ### * 운용구분 설정====
    
    observeEvent(input$type2,{
      if(input$type2 == "투자자산"){
        rv_app$type2 <- 'assets'
        mode <- 'ass_account'
      } else {
        rv_app$type2 <- 'pension'
        mode <- 'pen_account'
      }
      updateSelectInput(session, 'ass_account2',
                        choices = unique(ma_b()$read(rv_app$type2)$계좌))
      rv_app$trade <- reset_trade()
      update_new_trade()
    })
    
    observeEvent(input$ass_account2,{
  
      updateSelectInput(
        session, 'ass_cur2',
        choices = unique((ma_b()$read(rv_app$type2) |> 
                            filter(계좌==input$ass_account2))$통화)
      )
      rv_app$trade <- reset_trade()
      update_new_trade()
    })
    
    observeEvent(input$ass_cur2,{
      rv_app$trade <- reset_trade()
      update_new_trade()
    })
    
    ### * 신규/구분 설정====
    observeEvent(input$new2,{
      if(input$new2 != "신규"){
        t_rows2 <- filter(rv_app$trade, 행번호 == input$new2)
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
          bind_rows(ma$read('assets'), ma$read('pension')) |> 
          filter(계좌 == input$ass_account2,
                 통화 == input$ass_cur2,
                 종목명 == input$ass_name2) |> 
          pull(종목코드)
        
        if(length(trade_ticker)>0){
          rv_app$trade_new <- tibble::tibble_row(
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
        rv_app$trade_new$행번호 <- tail(sort(ma$read('assets_daily')$행번호), 1)+1
        dbxInsert(ma$con, 'assets_daily', rv_app$trade_new)
      } else {
        rv_app$trade_new$행번호 <- tail(sort(ma$read('pension_daily')$행번호), 1)+1
        dbxInsert(ma$con, 'pension_daily', rv_app$trade_new)
      }
      sk_b(!sk_b())
      # renew_bs()
      rv_app$trade <- reset_trade()
      update_new_trade()
    })
  
  
    observeEvent(input$ass_trade_mod,{
      rv_app$trade_new$행번호 <- input$new2
      if(input$type2 == "투자자산"){
        dbxUpdate(ma$con, 'assets_daily', rv_app$trade_new, 
                  where_cols = c("행번호"))
  
      } else {
        dbxUpdate(ma$con, 'pension_daily', rv_app$trade_new, 
                  where_cols = c("행번호"))
      }
      sk_b(!sk_b())
      # renew_bs()
      rv_app$trade <- reset_trade()
      update_new_trade()
    })
  
    observeEvent(input$ass_trade_del,{
      rv_app$trade_new$행번호 <- input$new2
      if(input$type2 == "투자자산"){
        dbxDelete(ma$con, 'assets_daily', rv_app$trade_new)
      } else {
        dbxDelete(ma$con, 'pension_daily', rv_app$trade_new)
      }
      sk_b(!sk_b())
      # renew_bs()
      rv_app$trade <- reset_trade()
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
            choices = NULL,
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
            choices = NULL
          ),
          selectInput(
            inputId = 'ass_class1',
            label = "세부자산군",
            choices = NULL
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = 'ass_class2',
            label = "세부자산군2",
            choices = NULL
          ),
          selectInput(
            inputId = 'ass_cur',
            label = "통화",
            choices = NULL
          )
        ),
        column(
          width = 2,
          autonumericInput(
            inputId = 'eval_price',
            label = "평가금액",
            value = 0
          ),
          airDatepickerInput(
            inputId = 'maturity_date',
            label = "만기일",
            addon = "none",
            value = Sys.Date()
          )
        )
      )
    })
  
    ### * 테이블 설정====
    output$ticker_table <- renderUI({
      if(!is.null(rv_app$tickers)){
        rv_app$tickers |> 
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
        ma_b()$read('assets') |> 
          filter(계좌==input$ass_account)
      } else {
        ma_b()$read('pension') |> 
          filter(계좌==input$ass_account)}
    })
    
    update_manage_ticker <-  reactive({
      updateSelectInput(session, 'new1',
                        choices = c('신규', rev(rv_app$tickers$행번호)),
                        selected = '신규')
    })
    
    update_categories <- reactive({
      
      updateSelectInput(session, 'ass_class',
                        choices = ctg()$ass_class)
  
      updateSelectInput(session, 'ass_class1',
                        choices = ctg()$ass_class1)
  
      updateSelectInput(session, 'ass_class2',
                        choices = ctg()$ass_class2)
  
      updateSelectInput(session, 'ass_cur',
                        choices = ctg()$ass_cur)
    })
      
    ### * 운용구분 설정====
  
    
    observeEvent(input$type1,{
      if(input$type1 == "투자자산"){
        mode <- 'ass_account'
      } else {
        mode <- 'pen_account'
      }
      updateSelectInput(session, 'ass_account',
                        choices = ctg()[[mode]])
      rv_app$tickers <- reset_ticker()
      update_manage_ticker()
      update_categories()
    })
    
    observeEvent(input$ass_account,{
      rv_app$tickers <- reset_ticker()
      update_manage_ticker()
      update_categories()
    })
    
    
    ### * 신규/구분 설정====
    observeEvent(input$new1,{
      if(input$new1 != "신규"){
        t_rows <- filter(rv_app$tickers, 행번호 == input$new1)
        
        # updateSelectInput(session, 'new1',
        #                   choices = c('신규', rev(rv_app$tickers$행번호)),
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
        updateAutonumericInput(session, 'maturity_date',
                               value = t_rows$만기일)
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
        updateAutonumericInput(session, 'maturity_date',
                               value = Sys.Date)
        
      }
    })
    
    ### * 추가/수정/삭제 선택시====
    observe({
      rv_app$ticker_new <- tibble::tibble_row(
        행번호=0, 계좌=input$ass_account, 
        종목코드=input$ticker,
        종목명=input$ass_name, 평가금액=input$eval_price,
        상품명=input$comm_name, 통화=input$ass_cur, 
        자산군=input$ass_class, 
        세부자산군=input$ass_class1, 세부자산군2=input$ass_class2,
        만기일=input$maturity_date,
        기초평가손익=NULL
      )
    })
    
    observeEvent(input$ticker_new,{
      
      if(input$type1 == "투자자산"){
        rv_app$ticker_new$행번호 <- tail(sort(ma$read('assets')$행번호), 1)+1
        dbxInsert(ma$con, 'assets', rv_app$ticker_new)
      } else {
        rv_app$ticker_new$행번호 <- tail(sort(ma$read('pension')$행번호), 1)+1
        dbxInsert(ma$con, 'pension', rv_app$ticker_new)
      }
  
      sk_b(!sk_b())
      # renew_bs()
      rv_app$tickers <- reset_ticker()
      update_manage_ticker()
      update_new_trade()
    })
    
    
    observeEvent(input$ticker_mod,{
      rv_app$ticker_new$행번호 <- input$new1
      if(input$type1 == "투자자산"){
        dbxUpdate(ma$con, 'assets', rv_app$ticker_new, where_cols = c("행번호"))
      } else {
        dbxUpdate(ma$con, 'pension', rv_app$ticker_new, where_cols = c("행번호"))
      }
      sk_b(!sk_b())
      # renew_bs()
      rv_app$tickers <- reset_ticker()
      update_manage_ticker()
      update_new_trade()
    })
    
    observeEvent(input$ticker_del,{
      rv_app$ticker_new$행번호 <- input$new1
      if(input$type1 == "투자자산"){
        dbxDelete(ma$con, 'assets', rv_app$ticker_new)
      } else {
        dbxDelete(ma$con, 'pension', rv_app$ticker_new)
      }
      sk_b(!sk_b())
      # renew_bs()
      rv_app$tickers <- reset_ticker()
      update_manage_ticker()
      update_new_trade()
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
          selectInput(paste0('select_',i), NULL, ctg()[[i]]),
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
        
        new_category_item <- tibble(
          key = i,
          value = input[[glue("add_{i}")]]
        )
        
        ma$upsert(df = new_category_item,
                  name = 'categories', 
                  cols = c('key', 'value'))
        
        # x <- ctg()
        # x[[i]] <- c(x[[i]], input[[glue("add_{i}")]])
        # saveRDS(x, 'categories.rds')
        sk_c(!sk_c())
        update_categories()
      })
      
      observeEvent(input[[glue("del_{i}_btn")]],{
        
        item_to_delete <- tibble(
          key = i,
          value = input[[glue("select_{i}")]]
        )
        
        dbxDelete(ma$con, 'categories', item_to_delete)
        
        # x <- ctg()
        # x[[i]] <- x[[i]][ x[[i]]!=input[[glue("select_{i}")]] ]
        # saveRDS(x, 'categories.rds')
        sk_c(!sk_c())
        update_categories()
      })
    })
    
    render_allo <- function(df){
        df |> flextable() |>
          theme_vanilla() |>
          merge_v(j=1:2) |>
          set_table_properties(layout='autofit',width=0.9) |>
          htmltools_value()
    }  
    
    ##d. 종합거래내역====
    
    total_table <- reactive({
      ma_b()$total_trading(input$total_trade_date)
    })
    
    
    output$total_trade_table <- renderUI({
      
      if(!is.null(input$total_trade_date)){
        
        df <- total_table()
        
        if(input$total_ass1!="전체"){
          df <- df %>% filter(자산군==input$total_ass1)
        }
        
        if(input$total_ass2!="전체"){
          df <- df %>% filter(세부자산군==input$total_ass2)
        }
        
        if(input$total_ass3!="전체"){
          df <- df %>% filter(세부자산군2==input$total_ass3)
        }
        
        if(input$total_curr!="전체"){
          df <- df %>% filter(통화==input$total_curr)
        }
        
        df |>
          flextable() |>
          theme_box() |>
          merge_v(j=1:5) |>
          set_table_properties(layout='autofit') |>
          colformat_double(j=7:11, digits = 0) |>
          htmltools_value(ft.align = 'center') 
      } else{
        
      }
    })
    
    # 2) 통합 자산운용 현황====
    
    ## a. 종합손익(그래프)====
    
    output$total_profit <- renderPlot({
      plot_obj <- ma_v()$plot_total_profit(
        input$total_s_date,input$total_e_date)
      
      if (rv_app$initial_load_done == FALSE) {
        
        # 3. "완료!" 메시지를 띄움
        show_delay("완료!", "success")
        
        # 4. 플래그를 TRUE로 변경하여 다시는 메시지가 뜨지 않도록 함
        rv_app$initial_load_done <- TRUE
      }
      
      plot_obj
    })
    
    ## b. 종합손익(테이블)====
    
    output$t_profit1 <- renderUI({
      ma_v()$compute_t_profit() %>% 
        group_by(연도=year(거래일자)) %>% 
        summarise(across(-거래일자, last)) %>% 
        transmute(
          연도=as.character(연도), 
          총평잔 = 투자평잔+연금평잔,
          평가손익,
          평가손익증감 = if_else(연도==2023, first(평가손익),
                           diff_vec(평가손익,silent = T)),
          총실현손익 = 투자실현손익+연금실현손익,
          총기간손익 = 총실현손익+평가손익증감,
          총기간수익률 = 총기간손익 / 총평잔 * 100,
          투자평잔, 
          투자실현손익, 
          투자실현수익률 = 투자실현손익 / 투자평잔 * 100,
          연금평잔, 
          연금실현손익,
          연금실현수익률 = 연금실현손익 / 연금평잔 * 100,
        ) %>% 
        flextable() |>
        theme_box() |>
        set_header_labels(
          values = list(
            연도="연도",
            총평잔="연평균잔액",
            평가손익="평가손익(누계)",
            평가손익증감="평가손익증감(A)",
            총실현손익="실현손익(B)",
            총기간손익 = "연간총손익(A+B)",
            총기간수익률 = "연간총수익률",
            투자평잔="연평균잔액",
            투자실현손익="실현손익",
            투자실현수익률 = "실현수익률",
            연금평잔="연평균잔액", 
            연금실현손익="실현손익",
            연금실현수익률 = "실현수익률"
          )
        ) %>%
        add_header_row(values = c("","총계정","투자계정","연금계정"), 
                       colwidths = c(1,6,3,3)) %>% 
        set_table_properties(layout='autofit') |>
        colformat_double(j=c(2,3,4,5,6,8,9,11,12), digits = 0) |>
        colformat_double(j=c(7,10,13), digits = 2) |>
        htmltools_value()
    })
    
    output$t_profit2 <- renderUI({
      ma_v()$compute_t_profit() %>% 
        filter(year(거래일자)==year(input$t_profit_date),
               month(거래일자)==month(input$t_profit_date)) %>% 
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=2:8, digits = 0) %>% 
        htmltools_value()
    })
    
    ## c. 자산군별 보유현황====
    
    output$t_asset_class <- renderUI({
      ma_v()$t_class |>
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=c(4,8,10), digits = 0) |>
        colformat_double(j=c(9,11), digits = 2) |>
        colformat_double(j=5:7, digits = 1) |>
        htmltools_value()
    })
    
    ## d. 상품별 보유현황1====
    output$t_commodity <- renderUI({
      ma_v()$t_comm |>
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=5:6, digits = 0) |>
        colformat_double(j=7, digits = 2) |>
        htmltools_value()
    })
    
    ## e. 상품별 보유현황2====
    output$t_commodity2 <- renderUI({
      ma_v()$t_comm2 |>
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=6, digits = 0) |>
        htmltools_value()
    })
    
    # 3) 자산별 손익현황====
    
   ## a. 계좌별 손익현황====
  
    output$total_accounts1 <- renderUI({
      ma_v()$t_comm3 %>% 
        flextable() |>
        theme_vanilla() |>
        merge_v(j=1) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=c(5:9), digits = 0) |>
        colformat_double(j=c(4,10:12), digits = 2) |>
        htmltools_value()
      
    })
    
    output$total_accounts2 <- renderUI({
      ma_v()$t_comm4 %>% 
        flextable() |>
        theme_vanilla() |>
        merge_v(j=1) |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=c(5:8), digits = 0) |>
        colformat_double(j=c(4,9:11), digits = 2) |>
        htmltools_value()
      
    })
    # ###* 자산군별 배분현황
    # output$allo0 <- renderUI({render_allo(ma()$allo0)})
    # output$allo1 <- renderUI({render_allo(ma()$allo1)})
    # twhdyd
    # ###* 통화별 배분현황
    # output$allo2 <- renderUI({render_allo(ma()$allo2)})
    # output$allo3 <- renderUI({render_allo(ma()$allo3)})
    # 
    # ###* 불리오 배분현황
    # output$allo5 <- renderUI({render_allo(ma()$allo5)})
    # output$allo4 <- renderUI({render_allo(ma()$allo4)})
   
  
    ## b. 상품별 손익현황====
  
    output$bs_pl_mkt_a <-renderUI({
      
      ma_v()$comm_profit %>%
        flextable() %>%
        theme_vanilla() %>%
        merge_v(j = 1:5) %>% # 상위 분류 병합
        colformat_double(j = 7:14, digits = 0) %>% # 금액형은 소수점 제거
        colformat_double(j = 15:19, digits = 2) %>% # 수익률은 소수점 2자리
        set_table_properties(layout = 'autofit', width = 1) %>%
        htmltools_value(ft.align = 'center')
    })
  
    # output$class_ret_a <- renderUI({
    #   ma_v()$ret_a |>
    #     select(1:3,평가금액,실현손익, 평가손익,
    #            실현수익률:평가수익률) |>
    #     flextable() |>
    #     theme_vanilla() |>
    #     merge_v(j=1:2) |>
    #     set_table_properties(layout='autofit') |>
    #     colformat_double(j=4:6, digits = 0) |>
    #     colformat_double(j=7:8, digits = 2) |>
    #     htmltools_value()
    # })
    # 
    # output$class_ret_a2 <- renderUI({
    #   ma_v()$ret_a2 |>
    #     select(1:2,평가금액,실현손익, 평가손익,
    #            실현수익률:평가수익률) |>
    #     flextable() |>
    #     theme_vanilla() |>
    #     merge_v(j=1) |>
    #     set_table_properties(layout='autofit') |>
    #     colformat_double(j=3:5, digits = 0) |>
    #     colformat_double(j=6:7, digits = 2) |>
    #     htmltools_value()
    # })
    
    ## c. 연금자산현황
    # output$allo6 <- renderUI({render_allo(ma()$allo6)})
    # output$allo7 <- renderUI({render_allo(ma()$allo7)})
    # output$allo8 <- renderUI({render_allo(ma()$allo8)})
    # output$allo9 <- renderUI({render_allo(ma()$allo9)})
    
    ## d. 연금손익현황
    
    # output$class_ret_p <- renderUI({
    #   ma_v()$ret_p |>
    #     select(1:3,평가금액,실현손익, 평가손익,
    #            실현수익률:평가수익률) |>
    #     flextable() |>
    #     theme_vanilla() |>
    #     merge_v(j=1:2) |>
    #     set_table_properties(layout='autofit') |>
    #     colformat_double(j=4:6, digits = 0) |>
    #     colformat_double(j=7:8, digits = 2) |>
    #     htmltools_value()
    # })
    # 
    # output$class_ret_p2 <- renderUI({
    #   ma_v()$ret_p2 |>
    #     select(1:2,평가금액,실현손익, 평가손익,
    #            실현수익률:평가수익률) |>
    #     flextable() |>
    #     theme_vanilla() |>
    #     merge_v(j=1) |>
    #     set_table_properties(layout='autofit') |>
    #     colformat_double(j=3:5, digits = 0) |>
    #     colformat_double(j=6:7, digits = 2) |>
    #     htmltools_value()
    # })
    # 
    # output$bs_pl_mkt_p <-renderUI({
    #   ma_v()$bs_pl_mkt_p |>
    #     select(계좌, 자산군, 세부자산군, 종목명,
    #            보유수량, 장부금액, 평가금액, 실현손익, 평가손익,
    #            실현수익률,평가수익률) |>
    #     arrange(계좌,자산군,세부자산군) |>
    #     flextable() |>
    #     theme_vanilla() |>
    #     merge_v(j=1:3) |>
    #     set_table_properties(layout='autofit') |>
    #     colformat_double(j=5:9, digits = 0) |>
    #     colformat_double(j=10:11, digits = 2) |>
    #     htmltools_value()
    # })
    
    
    # 4) 유동성 관리====
    
    ### * 메뉴 설정====
    output$manage_inflow <- renderUI({
      
      acct_list <- unique(c(ma_b()$assets$계좌, ma_b()$pension$계좌))
      
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
        selectInput(
          inputId = 'inflow_acct', # 계좌 선택 추가
          label = "계좌",
          choices = acct_list,
          width = '100%'
        ),
        autonumericInput(
          inputId = 'payment', # 단일 입력창으로 변경
          label = "자금유출입",
          value = 0,
          width = '100%'
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
      ma_b()$read('inflow') %>%
        filter(거래일자 >= ma$today) %>% 
        select(행번호,거래일자,계좌,자금유출입) %>% 
        arrange(거래일자)
    })
    # 
    
    liq <- reactiveValues(
      c=NULL, d=NULL
    )
    
    output$inflow_table1 <- renderUI({
      
      liq$c <- reset_inflow()
      
      liq$c %>% 
        flextable() |>
        theme_vanilla() %>% 
        set_header_labels(자금유출입 = "금액") %>% 
        colformat_double(j = "자금유출입", digits = 0) %>% 
        set_table_properties(layout = 'autofit') %>% 
        htmltools_value(ft.align = 'center')
    })
    
    update_manage_inflow <-  reactive({
      updateSelectInput(session, 'new3',
                        choices = c('신규', liq$c$행번호),
                        selected = '신규')
    })
    
    
    output$maturity_table <- renderUI({
      ma_v()$bs_pl_mkt_a %>% 
        bind_rows(ma_v()$bs_pl_mkt_p) %>% 
        filter(자산군=='채권', 세부자산군 %in% c('만기무위험','만기회사채'), 
               통화=='원화', 평가금액>0) %>% 
        select(계좌, 종목명, 종목코드, 평가금액) %>% 
        left_join(
          ma_b()$assets %>% 
            bind_rows(ma_b()$pension) %>% 
            select(종목코드, 만기일),
          by='종목코드'
        ) %>% 
        filter(만기일>ma$today) %>% 
        select(계좌, 종목명, 평가금액, 만기일) %>% 
        arrange(만기일) %>% 
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        htmltools_value(ft.align = 'center')
    })
    
    liquidity_data <- reactive({
      ma_v()$get_liquidity_analysis()
    })
    
    # 4-5-1. 현재 계좌별 총자산 (총자산추이 탭 상단)
    output$current_total_asset_table <- renderUI({
      liquidity_data()$current_status %>% 
        filter(구분 == '총자산') %>% 
        flextable() %>% 
        theme_box() %>% 
        colformat_double(digits=0) %>% 
        set_table_properties(layout='autofit', width=1) %>% 
        htmltools_value()
    })
    
    # 4-5-2. 현재 계좌별 현금성자산 (가용자금추이 탭 상단)
    output$current_cash_asset_table <- renderUI({
      liquidity_data()$current_status %>% 
        filter(구분 == '현금성자산') %>% 
        flextable() %>% 
        theme_box() %>% 
        colformat_double(digits=0) %>% 
        set_table_properties(layout='autofit', width=1) %>% 
        htmltools_value()
    })
    
    # 4-5-3. 향후 총자산 추이
    output$inflow_table3 <- renderUI({
      liquidity_data()$total_projection %>% 
        flextable() %>% 
        theme_vanilla() %>% 
        colformat_double(digits=0) %>% 
        set_table_properties(layout='autofit') %>% 
        htmltools_value()
    })
    
    # 4-5-4. 향후 가용자금 추이
    output$inflow_table4 <- renderUI({
      liquidity_data()$cash_projection %>% 
        flextable() %>% 
        theme_vanilla() %>% 
        colformat_double(digits=0) %>% 
        set_table_properties(layout='autofit') %>% 
        htmltools_value()
    })
    
    ### * 신규/구분 설정====
    observeEvent(input$new3, {
      if(input$new3 != "신규"){
        t_rows <- filter(liq$c, 행번호 == input$new3)
        updateAirDateInput(session, 'trading_date2', value = t_rows$거래일자)
        updateSelectInput(session, 'inflow_acct', selected = t_rows$계좌)
        updateAutonumericInput(session, 'payment', value = t_rows$자금유출입)
      } else {
        update_manage_inflow()
        updateAirDateInput(session, 'trading_date2', value = Sys.Date())
        updateSelectInput(session, 'inflow_acct', selected = NULL)
        updateAutonumericInput(session, 'payment', value = 0)
      }
    })
  
    ### * 추가/수정/삭제 선택시====
    observe({
      liq$d <- tibble::tibble_row(
        행번호 = 0, 
        거래일자 = input$trading_date2,
        계좌 = input$inflow_acct,
        자금유출입 = input$payment
      )
    })
  
    observeEvent(input$inflow_new, {
      # DB에 '계좌', '자금유출입' 컬럼이 있어야 함
      liq$d$행번호 <- tail(sort(ma$read('inflow')$행번호), 1) + 1
      dbxInsert(ma$con, 'inflow', liq$d)
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })
  
    observeEvent(input$inflow_mod, {
      liq$d$행번호 <- input$new3
      dbxUpdate(ma$con, 'inflow', liq$d, where_cols = c("행번호"))
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })
    
    observeEvent(input$inflow_del, {
      dbxDelete(ma$con, 'inflow', tibble::tibble_row(행번호 = input$new3))
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })
    
    
    # 5) 자산배분 현황====
    
    update_new_allo <- reactive({
      
      input$allo_renew
      df <- ma_b()$read('allocation')
      
      updateNumericInput(inputId = 'ass_bond', 
                         value = df$목표1[[7]])
      updateNumericInput(inputId = 'ass_stock', 
                         value = df$목표1[[4]])
      updateNumericInput(inputId = 'ass_alter', 
                         value = df$목표1[[1]])
      updateNumericInput(inputId = 'ass_bond_sol', 
                         value = df$목표2[[8]])
      updateNumericInput(inputId = 'ass_bond_ig', 
                         value = df$목표2[[9]])
      updateNumericInput(inputId = 'ass_bond_nr', 
                         value = df$목표2[[10]])
      updateNumericInput(inputId = 'ass_bond_cor', 
                         value = df$목표2[[11]])
      updateNumericInput(inputId = 'ass_stock_dev', 
                         value = df$목표2[[6]])
      updateNumericInput(inputId = 'ass_alter_com', 
                         value = df$목표2[[3]])
    })
    
    observeEvent(T,{
      update_new_allo()
    }, once = T)
    
    observeEvent(input$allo_renew,{
      df <- ma$read('allocation') %>% 
        mutate(목표1 = c(input$ass_alter, NA, NA,
                         input$ass_stock, NA, NA,
                         input$ass_bond, NA, NA, NA, NA, NA),
               목표2 = c(NA, 
                         input$ass_alter - input$ass_alter_com,
                         input$ass_alter_com,
                         NA, 
                         input$ass_stock - input$ass_stock_dev,
                         input$ass_stock_dev,
                         NA, 
                         input$ass_bond_sol,
                         input$ass_bond_ig,
                         input$ass_bond_nr, 
                         input$ass_bond_cor,
                         input$ass_bond - input$ass_bond_sol - input$ass_bond_ig - input$ass_bond_nr - input$ass_bond_cor))
      
      dbWriteTable(ma$con, 'allocation', df, 
                   overwrite = TRUE, row.names = FALSE)
      
      update_new_allo()
    })
    
    
    
    
    output$allocation_table <- renderUI({
      input$allo_renew
      
      df <- ma_v()$t_class %>% select(1:7) %>% 
        left_join(
          ma_b()$read('allocation') %>% 
            add_row(자산군='현금성', 세부자산군="", 
                    목표1 = 100-sum(.$목표1, na.rm = T)) %>% 
            mutate(세부자산군2 = '', .after=2),
          by=c('자산군','세부자산군', '세부자산군2')
        ) %>% 
        mutate(
          자산군=factor(자산군, 
                     levels=c("<합계>","주식","대체자산","채권",
                              "현금성", "환차손익"))
        ) %>% 
        arrange(자산군)
        
      
      df %>% mutate(
        목표금액 = na_if(df$평가금액[[1]]*(coalesce(목표1,0)+coalesce(목표2,0))/100,0), 
        .before=목표1) %>% 
        mutate(과부족 = 평가금액-목표금액) %>% 
        flextable() |>
        theme_vanilla() |>
        set_table_properties(layout='autofit') |>
        colformat_double(j=c(4,8,11), digits = 0) |>
        # colformat_double(j=c(9,11), digits = 2) |>
        colformat_double(j=c(5:7,9:10), digits = 1) |>
        htmltools_value()
    })
    
    output$account_allocation_table <- renderUI({
      req(ma_v())
      
      # 1. 원본 데이터 및 계좌 순서 추출
      # t_comm4에 존재하는 계좌들의 순서(Factor Level 순)를 그대로 가져옵니다.
      df_src <- ma_v()$t_comm4
      target_accts <- levels(df_src$계좌)[levels(df_src$계좌) %in% unique(df_src$계좌)]
      
      # 2. 데이터 필터링 및 전처리
      df <- df_src %>%
        filter(통화 == '원화') %>%
        select(계좌, 자산군, 평가금액) %>%
        # 자산군이 비어있거나 NA인 경우 '합계'로 변경 (계좌별 총자산 행)
        mutate(자산군 = if_else(자산군 == "" | is.na(자산군), "합계", as.character(자산군)))
      
      # 3. 피벗 (행: 자산군, 열: 계좌)
      df_wide <- df %>%
        pivot_wider(names_from = 계좌, values_from = 평가금액, values_fill = 0)
      
      # 4. 열(계좌) 순서 및 행(자산군) 순서 정렬
      # 4-1. 열 순서: target_accts에 있는 계좌만, 그 순서대로 선택 (데이터에 없는 계좌는 0으로 추가)
      # for(acct in target_accts) {
      #   if(!acct %in% names(df_wide)) {
      #     df_wide[[acct]] <- 0
      #   }
      # }
      # 
      # 4-2. 행 순서: 주식 -> 대체자산 -> 채권 -> 현금성 -> 합계 순으로 정렬
      row_levels <- c("주식", "외화자산", "대체자산", "채권", "현금성", "합계")
      
      df_final <- df_wide %>%
        # select(자산군, all_of(target_accts)) %>%  # 열 순서 적용
        mutate(자산군 = factor(자산군, levels = row_levels)) %>%
        mutate(합계 = rowSums(select(., where(is.numeric)), na.rm = TRUE)) %>% # 행 순서 적용을 위한 Factor 변환
        arrange(자산군)
      
      # 5. Flextable 렌더링
      df_final %>%
        flextable() %>%
        theme_vanilla() %>%
        colformat_double(digits = 0) %>%
        set_table_properties(layout = 'autofit') %>%
        align(align = "center", part = "all") %>%
        bg(i = ~ 자산군 == "합계", bg = "#f0f0f0") %>% # 합계 행 강조
        htmltools_value()
    })
    
    
    # 4) 한국은행 지표선정===
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ## b. 통계표 조회===
    # observeEvent(input$name,{
    #   rv_app$df <- ec$find_stat(input$name)
    #   updateSelectizeInput(session, 'name_in', 
    #                        choices = c('전체',rv_app$df$stat_name),
    #                        selected = '전체')
    #   
    # })
    
    ## c. 아이템 추가===
    
    ### * 통계표 이름=== 
    # observeEvent(input$name_in,{
    #   
    #   rv_app$name_in <- input$name_in
    #   
    #   updateSelectizeInput(session, 'name_in',
    #                        choices = c('전체', rv_app$df$stat_name),
    #                        selected = rv_app$name_in)
    #   
    #   if(rv_app$name_in=='전체'){
    #     code <- '전체'
    #     rv_app$code <- '전체'
    #   }
    #   else {
    #     rv_app$df_d <- rv_app$df |> filter(stat_name==rv_app$name_in)
    #     code <- rv_app$df_d$stat_code
    #     
    #     if(length(code)==1){rv_app$code <- code}
    #     else {rv_app$code <- code[1]}
    #     
    #   }
    #   updateSelectizeInput(session, 'code_in', choices = code,
    #                        selected = rv_app$code)
    # })
    
    ### * 통계표 코드===
    # observeEvent(input$code_in,{
    #   tryCatch({
    #     rv_app$df2 <- ec$find_items(input$code_in)
    #   }, error = function(e) {
    #     rv_app$df2 <- ec$find_items('전체')
    #   })
    #   
    #   rv_app$df5 <- rv_app$df2
    #   
    #   updateSelectizeInput(session, 'item_in',
    #                        choices = c('전체', unique(rv_app$df2$item_name)),
    #                        selected = '전체')
    #   updateSelectizeInput(session, 'cyl_in',
    #                        choices = c('전체', unique(rv_app$df2$cycle)),
    #                        selected = '전체')
    # })
    
    ### * 아이템이름===
    # observeEvent(input$item_in,{
    #   if(is.null(input$item_in)||input$item_in=='전체'){
    #     rv_app$df3 <- rv_app$df2
    #   }
    #   else rv_app$df3 <- rv_app$df2 |> filter(item_name==input$item_in)
    #   
    #   rv_app$df5 <- rv_app$df3
    #   
    #   updateSelectizeInput(session, 'item_in',
    #                        choices = c('전체', unique(rv_app$df2$item_name)),
    #                        selected = input$item_in)
    #   
    #   updateSelectizeInput(session, 'cyl_in',
    #                        choices = c('전체', unique(rv_app$df2$cycle)),
    #                        selected = '전체')        
    # })
    
    ### * 데이터주기===    
    # observeEvent(input$cyl_in,{
    #   if(is.null(input$cyl_in)||input$cyl_in=='전체'){
    #     rv_app$df4 <- rv_app$df3
    #   }
    #   else rv_app$df4 <- rv_app$df3 |> filter(cycle==input$cyl_in)
    #   
    #   rv_app$df5 <- rv_app$df4
    #   
    #   updateSelectizeInput(session, 'cyl_in',
    #                        choices = c('전체', unique(rv_app$df3$cycle)),
    #                        selected = input$cyl_in)
    # })
    
    ## * 아이템 추가===
    # observeEvent(input$add_item,{
    #   ec$save_items(rv_app$df5, input$ecos_name)
    #   sendSweetAlert(title="추가하였습니다!", type='success')
    #   rv_app$df_s <- ec$read_items()
    # })
    # 
    # observe({
    #   output$selected_item <- renderTable(rv_app$df_s)
    #   output$ecos_stat_tables <- renderTable(rv_app$df)
    #   output$ecos_item_tables <- renderTable(rv_app$df5)
    # })
    
    
    # 5) 한국은행 지표선정===
    
    
    
    observeEvent(input$close_win,{
      js$closeWindow()
      stopApp()
    })
  }, once = TRUE, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)
