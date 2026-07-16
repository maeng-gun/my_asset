# =============================================================================
# mod_holdings — 보유현황 모듈 (자산배분 + 상품별 보유현황 1/2/3)
# =============================================================================

mod_holdings_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        div(style = "text-align: left; margin-bottom: 10px;",
            uiOutput(ns("copy_btn_ui"))
        ),
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

    ## 엑셀 복사 버튼 UI 렌더링 (동기식 복사를 위한 hidden textarea 활용) ----
    output$copy_btn_ui <- renderUI({
      req(ma_v())
      df <- ma_v()$t_comm10
      req(nrow(df) > 0)
      
      # TSV(탭 구분) 형식의 문자열로 변환
      con <- textConnection("tsv_out", "w")
      write.table(df, con, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "")
      close(con)
      tsv_str <- paste(tsv_out, collapse = "\n")
      
      tagList(
        tags$textarea(id = session$ns("hidden_tsv_3"), style = "display:none;", tsv_str),
        tags$button(
          id = session$ns("copy_excel_3"),
          type = "button",
          class = "btn btn-sm btn-outline-primary",
          onclick = sprintf("fallbackCopyTextToClipboard(document.getElementById('%s').value)", session$ns("hidden_tsv_3")),
          tagList(icon("clipboard"), " 엑셀 복사")
        )
      )
    })
  })
}
