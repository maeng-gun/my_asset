# =============================================================================
# mod_trade_ticker — 투자종목 관리 모듈
# =============================================================================
# (구 mod_ticker.R에서 이름 변경)
# DB CRUD는 pool 객체 직접 주입받아 사용
# =============================================================================

mod_trade_ticker_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "투자종목 관리",
    fluidRow(
      box(
        id = ns('ticker_box'),
        width = 12, status = 'info', solidHeader = TRUE,
        title = "입력사항", collapsible = FALSE,
        uiOutput(ns('manage_ticker')),
        br(),
        div(
          actionButton(ns("ticker_new"), label = "추가", width = '30%', status = "info"),
          actionButton(ns("ticker_mod"), label = "수정", width = '30%', status = "success"),
          actionButton(ns("ticker_del"), label = "삭제", width = '30%', status = "primary"),
          style = 'text-align: center'
        )
      )
    ),
    fluidRow(
      box(
        width = 12, solidHeader = FALSE,
        collapsible = FALSE, headerBorder = FALSE,
        uiOutput(ns("ticker_table"))
      )
    )
  )
}

mod_trade_ticker_server <- function(id, pool, ma, ma_b, sk_b, ctg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(tickers = NULL, ticker_new = NULL)

    # --- 메뉴 설정 ---
    output$manage_ticker <- renderUI({
      fluidRow(
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               selectInput(ns('type1'), label = "운용구분",
                           choices = c("투자자산", "연금자산")),
               selectInput(ns('ass_account'), label = "계좌", choices = NULL)),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               selectInput(ns('new1'), label = "신규/수정", choices = "신규"),
               textInput(ns('ticker'), label = "종목코드", value = "")),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               textInput(ns('ass_name'), label = "종목명", value = ""),
               textInput(ns('comm_name'), label = "상품명", value = "")),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               selectInput(ns('ass_class'), label = "자산군", choices = NULL),
               selectInput(ns('ass_class1'), label = "세부자산군", choices = NULL)),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               selectInput(ns('ass_class2'), label = "세부자산군2", choices = NULL),
               selectInput(ns('ass_cur'), label = "통화", choices = NULL)),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               autonumericInput(ns('eval_price'), label = "평가금액", value = 0),
               airDatepickerInput(ns('maturity_date'), label = "만기일",
                                  addon = "none", value = Sys.Date()))
      )
    })

    # --- 테이블 렌더링 ---
    output$ticker_table <- renderUI({
      if (!is.null(rv$tickers)) {
        rv$tickers |> arrange(desc(행번호)) |>
          flextable() |> theme_vanilla() |>
          set_table_properties(layout = 'autofit') |>
          htmltools_value(ft.align = 'center')
      }
    })

    reset_ticker <- reactive({
      input$ticker_new; input$ticker_mod; input$ticker_del
      if (input$type1 == "투자자산") {
        ma_b()[['assets']] |> filter(계좌 == input$ass_account)
      } else {
        ma_b()[['pension']] |> filter(계좌 == input$ass_account)
      }
    })

    update_manage_ticker <- reactive({
      updateSelectInput(session, 'new1',
                        choices = c('신규', rev(rv$tickers$행번호)),
                        selected = '신규')
    })

    update_categories <- reactive({
      updateSelectInput(session, 'ass_class', choices = ctg()$ass_class)
      updateSelectInput(session, 'ass_class1', choices = ctg()$ass_class1)
      updateSelectInput(session, 'ass_class2', choices = ctg()$ass_class2)
      updateSelectInput(session, 'ass_cur', choices = ctg()$ass_cur)
    })

    # --- 운용구분 변경 ---
    observeEvent(input$type1, {
      mode <- if (input$type1 == "투자자산") 'ass_account' else 'pen_account'
      updateSelectInput(session, 'ass_account', choices = ctg()[[mode]])
      rv$tickers <- reset_ticker()
      update_manage_ticker()
      update_categories()
    })

    observeEvent(input$ass_account, {
      rv$tickers <- reset_ticker()
      update_manage_ticker()
      update_categories()
    })

    # --- 신규/수정 선택 ---
    observeEvent(input$new1, {
      if (input$new1 != "신규") {
        t_rows <- filter(rv$tickers, 행번호 == input$new1)
        updateSelectInput(session, 'ass_account', selected = t_rows$계좌)
        updateTextInput(session, 'ticker', value = t_rows$종목코드)
        updateTextInput(session, 'ass_name', value = t_rows$종목명)
        updateTextInput(session, 'comm_name', value = t_rows$상품명)
        updateSelectInput(session, 'ass_class', selected = t_rows$자산군)
        updateSelectInput(session, 'ass_class1', selected = t_rows$세부자산군)
        updateSelectInput(session, 'ass_class2', selected = t_rows$세부자산군2)
        updateSelectInput(session, 'ass_cur', selected = t_rows$통화)
        updateAutonumericInput(session, 'eval_price', value = t_rows$평가금액)
        updateAutonumericInput(session, 'maturity_date', value = t_rows$만기일)
      } else {
        updateSelectInput(session, 'ass_account', selected = NULL)
        updateTextInput(session, 'ticker', value = '')
        updateTextInput(session, 'ass_name', value = '')
        updateTextInput(session, 'comm_name', value = '')
        updateSelectInput(session, 'ass_class', selected = NULL)
        updateSelectInput(session, 'ass_class1', selected = '')
        updateSelectInput(session, 'ass_class2', selected = '')
        updateSelectInput(session, 'ass_cur', selected = NULL)
        updateAutonumericInput(session, 'eval_price', value = 0)
        updateAutonumericInput(session, 'maturity_date', value = Sys.Date)
      }
    })

    # --- 레코드 조립 ---
    observe({
      rv$ticker_new <- tibble::tibble_row(
        행번호 = 0, 계좌 = input$ass_account,
        종목코드 = input$ticker,
        종목명 = input$ass_name, 평가금액 = input$eval_price,
        상품명 = input$comm_name, 통화 = input$ass_cur,
        자산군 = input$ass_class,
        세부자산군 = input$ass_class1, 세부자산군2 = input$ass_class2,
        만기일 = input$maturity_date,
        기초평가손익 = NULL
      )
    })

    observeEvent(input$ticker_new, {
      if (input$type1 == "투자자산") {
        rv$ticker_new$행번호 <- ma$assets_last_num + 1
        dbxInsert(pool, 'assets', rv$ticker_new)
      } else {
        rv$ticker_new$행번호 <- ma$pension_last_num + 1
        dbxInsert(pool, 'pension', rv$ticker_new)
      }
      sk_b(!sk_b())
      rv$tickers <- reset_ticker()
      update_manage_ticker()
    })

    observeEvent(input$ticker_mod, {
      rv$ticker_new$행번호 <- input$new1
      if (input$type1 == "투자자산") {
        dbxUpdate(pool, 'assets', rv$ticker_new, where_cols = c("행번호"))
      } else {
        dbxUpdate(pool, 'pension', rv$ticker_new, where_cols = c("행번호"))
      }
      sk_b(!sk_b())
      rv$tickers <- reset_ticker()
      update_manage_ticker()
    })

    observeEvent(input$ticker_del, {
      rv$ticker_new$행번호 <- input$new1
      if (input$type1 == "투자자산") {
        dbxDelete(pool, 'assets', rv$ticker_new)
      } else {
        dbxDelete(pool, 'pension', rv$ticker_new)
      }
      sk_b(!sk_b())
      rv$tickers <- reset_ticker()
      update_manage_ticker()
    })
  })
}
