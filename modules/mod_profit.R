# =============================================================================
# mod_profit — 손익현황 모듈 (종합손익, 손익변동, 자산군별, 계좌별, 상품별)
# =============================================================================

mod_profit_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns('pf_box1'), width = 12, status = 'primary', type = 'tabs',

    ## a. 종합손익
    tabPanel(
      title = "종합손익",
      fluidRow(uiOutput(ns("t_profit1"))),
      fluidRow(
        column(
          width = 2, class = "col-12 col-md-4 col-lg-2",
          airDatepickerInput(ns('total_s_date'), label = "시작일",
                             addon = "none",
                             value = make_date(year(Sys.Date()), 1, 1) - 1),
          airDatepickerInput(ns('total_e_date'), label = "종료일",
                             addon = "none", value = Sys.Date()),
          uiOutput(ns("graph_limt"))
        ),
        column(
          width = 10, class = "col-12 col-md-8 col-lg-10",
          plotOutput(ns("total_profit"), height = "800px")
        )
      )
    ),

    ## b. 손익변동
    tabPanel(
      title = "손익변동",
      fluidRow(uiOutput(ns("profit_var")))
    ),

    ## c. 자산군별 손익현황
    tabPanel(
      title = "자산군별",
      fluidRow(uiOutput(ns("total_accounts1")))
    ),

    ## d. 계좌별 손익현황
    tabPanel(
      title = "계좌별",
      fluidRow(uiOutput(ns("total_accounts2")))
    ),

    ## e. 계좌별상품 손익현황
    tabPanel(
      title = "계좌별상품",
      fluidRow(uiOutput(ns("bs_pl_mkt_a")))
    ),

    ## f. 자산군별상품 손익현황
    tabPanel(
      title = "자산군별상품",
      fluidRow(uiOutput(ns("bs_pl_mkt_a2")))
    )
  )
}

mod_profit_server <- function(id, ma_v, on_initial_load) {
  moduleServer(id, function(input, output, session) {

    initial_done <- reactiveVal(FALSE)

    ## a. 종합손익 테이블
    output$t_profit1 <- renderUI({
      big_border <- officer::fp_border(color = "black", width = 2)
      ma_v()$compute_t_profit() %>%
        flextable() |> theme_box() |>
        set_table_properties(layout = 'autofit') |>
        vline(j = 7, border = big_border, part = "all") %>%
        vline(j = 8, border = big_border, part = "all") %>%
        hline_top(j = 8, border = big_border, part = "header") %>%
        hline_bottom(j = 8, border = big_border, part = "body") %>%
        colformat_double(j = 2:8, digits = 0) |>
        colformat_double(j = 9:11, digits = 2) |>
        htmltools_value()
    })

    ## 손익 그래프 데이터
    df_graph <- reactive({
      input$total_s_date; input$total_e_date
      ma_v()$plot_total_profit(input$total_s_date, input$total_e_date)
    })

    y_num <- reactive({
      ceiling(max(abs(df_graph()$손익누계)) / 10)
    })

    output$graph_limt <- renderUI({
      searchInput(
        session$ns("graph_limt2"), label = "손익단위",
        value = y_num(),
        btnSearch = icon("search"), btnReset = NULL, width = "100%"
      )
    })

    output$total_profit <- renderPlot({
      req(input$graph_limt2)
      num <- as.numeric(input$graph_limt2)

      fig1 <- ma_v()$plot_total_eval()

      fig2 <- df_graph() %>%
        ggplot(aes(x = 기준일)) +
        geom_line(aes(y = 손익누계)) +
        geom_bar(aes(y = 일간손익), stat = 'identity') +
        scale_y_continuous(
          breaks = function(x) {seq(
            floor(x[1] / num) * num,
            ceiling(x[2] / num) * num,
            by = num
          )}, sec.axis = dup_axis(name = NULL)
        ) +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 45, hjust = 1))

      plot_obj <- gridExtra::grid.arrange(fig2, fig1, nrow = 2)

      # 최초 로딩 완료 콜백
      if (!initial_done()) {
        on_initial_load()
        initial_done(TRUE)
      }

      plot_obj
    })

    ## b. 손익변동
    output$profit_var <- renderUI({
      ma_v()$compute_profit_var() %>%
        flextable() |> theme_vanilla() |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = c(4:6, 8, 10, 12, 14), digits = 0) %>%
        colformat_double(j = c(7, 9, 11, 13, 15), digits = 2) %>%
        htmltools_value()
    })

    ## c. 자산군별 손익현황
    output$total_accounts1 <- renderUI({
      ma_v()$t_comm3 %>%
        flextable() |> theme_vanilla() |>
        merge_v(j = 1:3) |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = c(4:10), digits = 0) |>
        colformat_double(j = c(11:14), digits = 2) |>
        htmltools_value()
    })

    ## d. 계좌별 손익현황
    output$total_accounts2 <- renderUI({
      ma_v()$t_comm4 %>%
        flextable() |> theme_vanilla() |>
        merge_v(j = 1:2) |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = c(3:9), digits = 0) |>
        colformat_double(j = c(10:13), digits = 2) |>
        htmltools_value()
    })

    ## e. 계좌별상품 손익현황
    output$bs_pl_mkt_a <- renderUI({
      ma_v()$comm_profit %>%
        flextable() %>% theme_vanilla() %>%
        merge_v(j = 1:6) %>%
        colformat_double(j = 7:14, digits = 0) %>%
        colformat_double(j = 15:18, digits = 2) %>%
        set_table_properties(layout = 'autofit', width = 1) %>%
        htmltools_value(ft.align = 'center')
    })

    ## f. 자산군별상품 손익현황
    output$bs_pl_mkt_a2 <- renderUI({
      ma_v()$comm_profit2 %>%
        flextable() %>% theme_vanilla() %>%
        merge_v(j = 1:6) %>%
        colformat_double(j = 7:14, digits = 0) %>%
        colformat_double(j = 15:18, digits = 2) %>%
        set_table_properties(layout = 'autofit', width = 1) %>%
        htmltools_value(ft.align = 'center')
    })
  })
}
