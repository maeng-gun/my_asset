# =============================================================================
# mod_holdings — 보유현황 모듈 (자산배분 + 상품별 보유현황 1/2/3)
# =============================================================================

mod_holdings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    navset_card_tab(
      id = ns("pf_box2"),

## 계좌별 자산배분 ----
      nav_panel(
        title = "계좌별 자산배분",
        fluidRow(reactableOutput(ns("account_allocation_table")))
      ),

## 상품별 보유현황1 ----
      nav_panel(
        title = "상품별 보유현황1",
        fluidRow(reactableOutput(ns("t_commodity1")))
      ),

## 상품별 보유현황2 ----
      nav_panel(
        title = "상품별 보유현황2",
        fluidRow(reactableOutput(ns("t_commodity2")))
      ),

## 상품별 보유현황3 ----
      nav_panel(
        title = "상품별 보유현황3",
        fluidRow(reactableOutput(ns("t_commodity3")))
      )
    )
  )
}

mod_holdings_server <- function(id, ma_v, menu_tabs) {
  moduleServer(id, function(input, output, session) {
## 계좌별 자산배분 ----
    output$account_allocation_table <- renderReactable({
      req(menu_tabs() == "pf_total")
      req(ma_v())

      df <- ma_v()$account_allocation
      render_rt(
        df,
        int_cols    = 4:15,
        pct_cols    = 16,
        sticky_cols = names(df)[1:3] # 앞 3개 컬럼(자산군, 세부자산군, 계좌 등) 좌측 고정
      )
    })

## 상품별 보유현황1 ----
    output$t_commodity1 <- renderReactable({
      req(menu_tabs() == "pf_total")
      req(ma_v())

      df <- ma_v()$t_comm
      render_rt(
        df,
        int_cols      = 5:10,
        pct_cols      = 11,
        sticky_cols   = names(df)[1:4]
      )
    })

## 상품별 보유현황2 ----
    output$t_commodity2 <- renderReactable({
      req(menu_tabs() == "pf_total")
      req(ma_v())

      df <- ma_v()$t_comm2
      render_rt(
        df,
        int_cols = 7:9,
        pct_cols = 10,
        sticky_cols = names(df)[1:4]
      )
    })

## 상품별 보유현황3 ----
    output$t_commodity3 <- renderReactable({
      req(menu_tabs() == "pf_total")
      req(ma_v())

      df <- ma_v()$t_comm10
      render_rt(
        df,
        int_cols = 5,
        sticky_cols = names(df)[1:3]
      )
    })
  })
}
