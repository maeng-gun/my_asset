# =============================================================================
# mod_liquidity — 유동성 관리 모듈 (자금유출입 + 총자산추이 + 가용자금추이)
# =============================================================================
# DB CRUD는 pool 객체 직접 주입
# 분석 로직은 순수 함수(calc_maturity_analysis, calc_liquidity_analysis) 호출
# =============================================================================

mod_liquidity_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns('liquid_tabs'),

## a. 자금유출입 탭 ----
    nav_panel(
      title = "자금유출입",
      fluidRow(
        column(
          width = 12,
          card(
            class = "mb-3 border-info",
            card_header("평가금액 추이", class = "bg-info text-white"),
            card_body(echarts4rOutput(ns("total_profit_trend"), height = "360px"))
          )
        )
      ),
      fluidRow(
        column(width = 2, class = "col-12 col-md-2 col-lg-2",
               card(class = "mb-3 border-info",
                   card_header("입력사항", class = "bg-info text-white"),
                   card_body(uiOutput(ns('manage_inflow'))))),
        column(width = 5, class = "col-12 col-md-5 col-lg-5",
               card(class = "mb-3 border-info",
                   card_header("유출입 내역", class = "bg-info text-white"),
                   card_body(reactableOutput(ns('inflow_table1'))))),
        column(width = 5, class = "col-12 col-md-5 col-lg-5",
               card(class = "mb-3 border-info",
                   card_header("만기도래내역", class = "bg-info text-white"),
                   card_body(reactableOutput(ns('maturity_table')))))
      )
    ),

## b. 총자산추이 탭 ----
    nav_panel(
      title = "총자산추이",
      fluidRow(
        card(class = "mb-3 border-info", fill = FALSE,
            card_header("총자산현황", class = "bg-info text-white"),
            card_body(fill = FALSE, reactableOutput(ns('current_total_asset_table'))))),
      fluidRow(
        card(class = "mb-3 border-info",
            card_header("총자산추이", class = "bg-info text-white"),
            card_body(reactableOutput(ns('inflow_table3')))))
    ),

## c. 가용자금추이 탭 ----
    nav_panel(
      title = "가용자금추이",
      fluidRow(
        card(class = "mb-3 border-info", fill = FALSE,
            card_header("현금성자산현황", class = "bg-info text-white"),
            card_body(fill = FALSE, reactableOutput(ns('current_cash_asset_table'))))),
      fluidRow(
        card(class = "mb-3 border-info",
            card_header("가용자금추이", class = "bg-info text-white"),
            card_body(reactableOutput(ns('inflow_table4')))))
    )
  )
}

mod_liquidity_server <- function(id, pool, ma, ma_b, ma_v, sk_b, menu_tabs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    liq <- reactiveValues(c = NULL, d = NULL)

    # === a. 자금유출입 탭 ===

    ## 평가금액 추이 차트 ----
    output$total_profit_trend <- renderEcharts4r({
      req(menu_tabs() == "pf_liquid")
      ma_obj <- ma_v()

      trend_df <- calc_eval_trend_data(
        return_tbl = ma_obj$read_obj("return"),
        inflow_df  = ma_obj$inflow,
        today      = ma_obj$today,
        t_comm2    = ma_obj$t_comm2,
        acct_order = ma_obj$acct_order
      )

      df_past <- trend_df %>%
        filter(구분 == "과거평가액") %>%
        select(기준일, 과거평가액 = 평가금액)

      df_future <- trend_df %>%
        filter(구분 == "예상평가액(점선)") %>%
        select(기준일, 예상평가액 = 평가금액, 투자가능자산, 현금화가능자산, 인출가능현금)

      df_chart <- full_join(df_past, df_future, by = "기준일") %>%
        arrange(기준일)

      df_chart |>
        e_charts(기준일) |>
        e_line(과거평가액, name = "과거평가액", symbol = "none") |>
        e_line(예상평가액, name = "예상평가액(점선)", symbol = "none", lineStyle = list(type = "dashed")) |>
        e_line(투자가능자산, name = "투자가능자산", symbol = "none") |>
        e_line(현금화가능자산, name = "현금화가능자산", symbol = "none") |>
        e_line(인출가능현금, name = "인출가능현금", symbol = "none") |>
        e_color(c("#2b5c8f", "#4682b4", "#28a745", "#fd7e14", "#dc3545")) |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%", left = "3%") |>
        e_legend(right = 0, top = "center", orient = "vertical")
    })

## 메뉴 설정 ----
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
        actionButton(ns("inflow_new"), label = "추가", class = "btn btn-info", width = '100%'),
        br(), br(),
        actionButton(ns("inflow_mod"), label = "수정", class = "btn btn-success", width = '100%'),
        br(), br(),
        actionButton(ns("inflow_del"), label = "삭제", class = "btn btn-primary", width = '100%')
      )
    })

## 유출입 내역 조회 ----
    reset_inflow <- reactive({
      ma_b()[['inflow']] %>%
        filter(거래일자 >= ma$today) %>%
        select(행번호, 거래일자, 계좌, 자금유출입) %>%
        arrange(거래일자)
    })

    output$inflow_table1 <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      liq$c <- reset_inflow()
      df <- liq$c
      names(df)[names(df) == "자금유출입"] <- "금액"
      render_rt(df, int_cols = c("금액"))
    })

    update_manage_inflow <- reactive({
      updateSelectInput(session, 'new3',
                        choices = c('신규', liq$c$행번호),
                        selected = '신규')
    })

## 신규/구분 선택 ----
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

## 레코드 조립 ----
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

## 만기도래 테이블 ----
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

    output$maturity_table <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      render_rt(maturity_data(), int_cols = 3)
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

    output$current_total_asset_table <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      df <- liquidity_data()$current_status %>% filter(구분 == '총자산')
      # Apply integer format to all numeric columns
      int_c <- names(df)[sapply(df, is.numeric)]
      render_rt(df, int_cols = int_c, dynamic_height = FALSE)
    })

    output$inflow_table3 <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      df <- liquidity_data()$total_projection
      int_c <- names(df)[sapply(df, is.numeric)]
      render_rt(df, int_cols = int_c)
    })

    # === c. 가용자금추이 탭 ===

    output$current_cash_asset_table <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      df <- liquidity_data()$current_status %>% filter(구분 == '현금성자산')
      int_c <- names(df)[sapply(df, is.numeric)]
      render_rt(df, int_cols = int_c, dynamic_height = FALSE)
    })

    output$inflow_table4 <- renderReactable({
      req(menu_tabs() == "pf_liquid")
      df <- liquidity_data()$cash_projection
      int_c <- names(df)[sapply(df, is.numeric)]
      render_rt(df, int_cols = int_c)
    })
  })
}
