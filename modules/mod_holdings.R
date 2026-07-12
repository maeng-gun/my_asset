# =============================================================================
# mod_holdings — 보유현황 모듈 (자산배분 + 상품별 보유현황 1/2/3)
# =============================================================================

mod_holdings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    tabBox(
      id = ns('pf_box2'), width = 12, status = 'primary', type = 'tabs',

      ## 계좌별 자산배분
      tabPanel(
        title = "계좌별 자산배분",
        fluidRow(uiOutput(ns('account_allocation_table')))
      ),

      ## 상품별 보유현황1
      tabPanel(
        title = "상품별 보유현황1",
        fluidRow(uiOutput(ns("t_commodity")))
      ),

      ## 상품별 보유현황2
      tabPanel(
        title = "상품별 보유현황2",
        fluidRow(uiOutput(ns("t_commodity2")))
      ),

      ## 상품별 보유현황3
      tabPanel(
        title = "상품별 보유현황3",
        fluidRow(uiOutput(ns("t_commodity3")))
      )
    )
  )
}

mod_holdings_server <- function(id, ma_v) {
  moduleServer(id, function(input, output, session) {

    ## 계좌별 자산배분
    output$account_allocation_table <- renderUI({
      req(ma_v())
      ma_v()$account_allocation %>%
        flextable() %>%
        theme_vanilla() %>%
        colformat_double(j = 4:15, digits = 0) %>%
        colformat_double(j = 16, digits = 2) %>%
        set_table_properties(layout = 'autofit') %>%
        align(align = "center", part = "all") %>%
        htmltools_value()
    })

    ## 상품별 보유현황1
    output$t_commodity <- renderUI({
      ma_v()$t_comm |>
        flextable() |> theme_vanilla() |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = 5:10, digits = 0) |>
        colformat_double(j = 11, digits = 2) |>
        htmltools_value()
    })

    ## 상품별 보유현황2
    output$t_commodity2 <- renderUI({
      ma_v()$t_comm2 |>
        flextable() |> theme_vanilla() |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = 7:9, digits = 0) |>
        colformat_double(j = 10, digits = 2) |>
        htmltools_value()
    })

    ## 상품별 보유현황3
    output$t_commodity3 <- renderUI({
      ma_v()$t_comm10 |>
        flextable() |> theme_vanilla() |>
        set_table_properties(layout = 'autofit') |>
        colformat_double(j = 5, digits = 0) |>
        htmltools_value()
    })
  })
}
