# =============================================================================
# mod_liquidity — 유동성 관리 모듈 (자금유출입 + 총자산추이 + 가용자금추이)
# =============================================================================
# DB CRUD는 pool 객체 직접 주입
# 분석 로직은 순수 함수(calc_maturity_analysis, calc_liquidity_analysis) 호출
# =============================================================================

mod_liquidity_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns('liquid_tabs'), width = 12, status = 'primary', type = 'tabs',

    ## a. 자금유출입 탭
    tabPanel(
      title = "자금유출입",
      fluidRow(
        column(width = 3, class = "col-12 col-md-4 col-lg-3",
               box(width = 12, title = "입력사항", status = "info",
                   solidHeader = TRUE, collapsible = FALSE,
                   uiOutput(ns('manage_inflow')))),
        column(width = 5, class = "col-12 col-md-4 col-lg-5",
               box(width = 12, title = "유출입 내역", status = "info",
                   solidHeader = TRUE, collapsible = FALSE,
                   uiOutput(ns('inflow_table1')))),
        column(width = 4, class = "col-12 col-md-4 col-lg-4",
               box(width = 12, title = "만기도래내역", status = "info",
                   solidHeader = TRUE, collapsible = FALSE,
                   uiOutput(ns('maturity_table'))))
      )
    ),

    ## b. 총자산추이 탭
    tabPanel(
      title = "총자산추이",
      fluidRow(
        box(width = 12, title = "총자산현황", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            uiOutput(ns('current_total_asset_table')))),
      fluidRow(
        box(width = 12, title = "총자산추이", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            uiOutput(ns('inflow_table3'))))
    ),

    ## c. 가용자금추이 탭
    tabPanel(
      title = "가용자금추이",
      fluidRow(
        box(width = 12, title = "현금성자산현황", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            uiOutput(ns('current_cash_asset_table')))),
      fluidRow(
        box(width = 12, title = "가용자금추이", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            uiOutput(ns('inflow_table4'))))
    )
  )
}

mod_liquidity_server <- function(id, pool, ma, ma_b, ma_v, sk_b) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    liq <- reactiveValues(c = NULL, d = NULL)

    # === a. 자금유출입 탭 ===

    ## 메뉴 설정
    output$manage_inflow <- renderUI({
      acct_list <- unique(c(ma_b()$assets$계좌, ma_b()$pension$계좌))
      fluidRow(
        selectInput(ns('new3'), label = "신규/수정", choices = "신규", width = '100%'),
        airDatepickerInput(ns('trading_date2'), label = "거래일자",
                           addon = "none", value = Sys.Date(), width = '100%'),
        selectInput(ns('inflow_acct'), label = "계좌",
                    choices = acct_list, width = '100%'),
        autonumericInput(ns('payment'), label = "자금유출입", value = 0, width = '100%'),
        br(),
        actionButton(ns("inflow_new"), label = "추가", status = "info", width = '100%'),
        br(),
        actionButton(ns("inflow_mod"), label = "수정", status = "success", width = '100%'),
        br(),
        actionButton(ns("inflow_del"), label = "삭제", status = "primary", width = '100%')
      )
    })

    ## 유출입 내역 조회
    reset_inflow <- reactive({
      ma_b()[['inflow']] %>%
        filter(거래일자 >= ma$today) %>%
        select(행번호, 거래일자, 계좌, 자금유출입) %>%
        arrange(거래일자)
    })

    output$inflow_table1 <- renderUI({
      liq$c <- reset_inflow()
      liq$c %>%
        flextable() |> theme_vanilla() %>%
        set_header_labels(자금유출입 = "금액") %>%
        colformat_double(j = "자금유출입", digits = 0) %>%
        set_table_properties(layout = 'autofit') %>%
        htmltools_value(ft.align = 'center')
    })

    update_manage_inflow <- reactive({
      updateSelectInput(session, 'new3',
                        choices = c('신규', liq$c$행번호),
                        selected = '신규')
    })

    ## 신규/구분 선택
    observeEvent(input$new3, {
      if (input$new3 != "신규") {
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

    ## 레코드 조립
    observe({
      liq$d <- tibble::tibble_row(
        행번호 = 0,
        거래일자 = input$trading_date2,
        계좌 = input$inflow_acct,
        자금유출입 = input$payment
      )
    })

    observeEvent(input$inflow_new, {
      liq$d$행번호 <- ma$inflow_last_num + 1
      dbxInsert(pool, 'inflow', liq$d)
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })

    observeEvent(input$inflow_mod, {
      liq$d$행번호 <- input$new3
      dbxUpdate(pool, 'inflow', liq$d, where_cols = c("행번호"))
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })

    observeEvent(input$inflow_del, {
      dbxDelete(pool, 'inflow', tibble::tibble_row(행번호 = input$new3))
      liq$c <- reset_inflow()
      update_manage_inflow()
      sk_b(!sk_b())
    })

    ## 만기도래 테이블
    maturity_data <- reactive({
      ma_obj <- ma_v()
      # calc_maturity_analysis 순수 함수 호출
      calc_maturity_analysis(
        bs_pl_mkt_a = ma_obj$bs_pl_mkt_a,
        bs_pl_mkt_p = ma_obj$bs_pl_mkt_p,
        assets_df   = ma_obj$assets,
        pension_df  = ma_obj$pension,
        today       = ma_obj$today
      )
    })

    output$maturity_table <- renderUI({
      maturity_data() %>%
        flextable() |> theme_vanilla() |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = 3, digits = 0) %>%
        htmltools_value(ft.align = 'center')
    })

    # === b. 총자산추이 탭 ===

    liquidity_data <- reactive({
      ma_obj <- ma_v()
      # calc_liquidity_analysis 순수 함수 호출
      calc_liquidity_analysis(
        t_comm2     = ma_obj$t_comm2,
        inflow_df   = ma_obj$inflow,
        maturity_df = maturity_data(),
        today       = ma_obj$today,
        acct_order  = ma_obj$acct_order
      )
    })

    output$current_total_asset_table <- renderUI({
      liquidity_data()$current_status %>%
        filter(구분 == '총자산') %>%
        flextable() %>% theme_box() %>%
        colformat_double(digits = 0) %>%
        set_table_properties(layout = 'autofit', width = 1) %>%
        htmltools_value()
    })

    output$inflow_table3 <- renderUI({
      liquidity_data()$total_projection %>%
        flextable() %>% theme_vanilla() %>%
        colformat_double(digits = 0) %>%
        set_table_properties(layout = 'autofit') %>%
        htmltools_value()
    })

    # === c. 가용자금추이 탭 ===

    output$current_cash_asset_table <- renderUI({
      liquidity_data()$current_status %>%
        filter(구분 == '현금성자산') %>%
        flextable() %>% theme_box() %>%
        colformat_double(digits = 0) %>%
        set_table_properties(layout = 'autofit', width = 1) %>%
        htmltools_value()
    })

    output$inflow_table4 <- renderUI({
      liquidity_data()$cash_projection %>%
        flextable() %>% theme_vanilla() %>%
        colformat_double(digits = 0) %>%
        set_table_properties(layout = 'autofit') %>%
        htmltools_value()
    })
  })
}
