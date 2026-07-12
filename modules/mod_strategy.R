# =============================================================================
# mod_strategy — 배분전략 및 성과분석 모듈
# =============================================================================

mod_strategy_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns('bm_box'), width = 12, status = 'primary', type = 'tabs',

    ## a. 자산배분
    tabPanel(
      title = "자산배분",
      fluidRow(
        column(
          width = 4, class = "col-12 col-md-6 col-lg-4",
          box(
            width = 12, title = "자산배분 입력", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            uiOutput(ns('allo_input'))
          )
        ),
        column(
          width = 8, class = "col-12 col-md-6 col-lg-8",
          box(
            width = 12, title = "자산배분 시계열", status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            fluidRow(column(3, uiOutput(ns('allo_year')))),
            fluidRow(uiOutput(ns('allo_table_ui')))
          )
        )
      )
    ),

    ## b. 성과분석
    tabPanel(
      title = "성과분석",
      fluidRow(
        column(width = 12,
               airDatepickerInput(ns("base_month"), label = "기준 연월 선택",
                                  value = Sys.Date(), view = "months",
                                  minView = "months", dateFormat = "yyyy-MM",
                                  width = "300px", addon = "right"))
      ),
      uiOutput(ns("dynamic_boxes"))
    )
  )
}

mod_strategy_server <- function(id, ma, ma_b, ma_v, sk_b) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_delay <- function(text, type) show_alert(title = text, type = type)

    # === a. 자산배분 CRUD ===

    ## 신규/수정 선택 옵션 동적 생성
    observe({
      sk_b()
      df <- ma_b()$read('allo_table') %>% arrange(행번호)
      choices_list <- c("신규 입력" = "신규")
      if (nrow(df) > 0) {
        row_labels <- paste(df$행번호, df$배분일자, df$구분, sep = " | ")
        row_vals <- as.character(df$행번호)
        choices_list <- c(choices_list, setNames(row_vals, row_labels))
      }
      isolate({
        updateSelectInput(session, "allo_new", choices = choices_list, selected = "신규")
      })
    })

    ## 신규/수정 선택 시 기존 데이터 불러오기
    observeEvent(input$allo_new, {
      req(input$allo_new)
      if (input$allo_new != "신규") {
        df <- ma_b()$read('allo_table')
        sel_row <- df %>% filter(행번호 == as.numeric(input$allo_new))
        if (nrow(sel_row) == 1) {
          updateAirDateInput(session, "allo_date", value = as.Date(sel_row$배분일자))
          updateAutonumericInput(session, "allo_stock_dom", value = sel_row$국내주식)
          updateAutonumericInput(session, "allo_stock_ovs", value = sel_row$해외주식)
          updateAutonumericInput(session, "allo_bond_mat", value = sel_row$만기보유채권)
          updateAutonumericInput(session, "allo_bond_mkt", value = sel_row$시장형채권)
          updateAutonumericInput(session, "allo_alter_real", value = sel_row$실물자산)
          updateAutonumericInput(session, "allo_alter_inc", value = sel_row$인컴자산)
          updateSelectInput(session, "allo_mode", selected = sel_row$구분)
        }
      }
    })

    ## 추가
    observeEvent(input$allo_add, {
      df_current <- ma_b()$read('allo_table')
      next_id <- ifelse(nrow(df_current) == 0, 1,
                        max(as.numeric(df_current$행번호), na.rm = TRUE) + 1)
      new_row <- data.frame(
        배분일자 = as.character(input$allo_date),
        국내주식 = as.numeric(input$allo_stock_dom),
        해외주식 = as.numeric(input$allo_stock_ovs),
        만기보유채권 = as.numeric(input$allo_bond_mat),
        시장형채권 = as.numeric(input$allo_bond_mkt),
        실물자산 = as.numeric(input$allo_alter_real),
        인컴자산 = as.numeric(input$allo_alter_inc),
        구분 = input$allo_mode,
        행번호 = as.integer(next_id),
        stringsAsFactors = FALSE
      )
      dbxInsert(conn = ma$con, table = "allo_table", records = new_row)
      sk_b(!sk_b())
      show_delay("자산배분 내역이 추가되었습니다.", "success")
    })

    ## 수정
    observeEvent(input$allo_modi, {
      if (input$allo_new == "신규") return(show_delay("수정할 행을 선택해주세요.", "warning"))
      mod_row <- data.frame(
        배분일자 = as.character(input$allo_date),
        국내주식 = as.numeric(input$allo_stock_dom),
        해외주식 = as.numeric(input$allo_stock_ovs),
        만기보유채권 = as.numeric(input$allo_bond_mat),
        시장형채권 = as.numeric(input$allo_bond_mkt),
        실물자산 = as.numeric(input$allo_alter_real),
        인컴자산 = as.numeric(input$allo_alter_inc),
        구분 = input$allo_mode,
        행번호 = as.numeric(input$allo_new),
        stringsAsFactors = FALSE
      )
      dbxUpdate(conn = ma$con, table = "allo_table", records = mod_row,
                where_cols = c("행번호"))
      sk_b(!sk_b())
      show_delay("성공적으로 수정되었습니다.", "success")
    })

    ## 삭제
    observeEvent(input$allo_del, {
      if (input$allo_new == "신규") return(show_delay("삭제할 행을 선택해주세요.", "error"))
      dbxDelete(conn = ma$con, table = "allo_table",
                where = data.frame(행번호 = as.numeric(input$allo_new)))
      updateSelectInput(session, "allo_new", selected = "신규")
      sk_b(!sk_b())
      show_delay("자산배분 내역이 삭제되었습니다.", "success")
    })

    ## 입력 UI
    output$allo_input <- renderUI({
      column(
        width = 12,
        fluidRow(
          column(width = 6, class = "col-12 col-md-6",
                 airDatepickerInput(ns('allo_date'), "배분일자", addon = "none",
                                    value = Sys.Date(), width = '100%'),
                 autonumericInput(ns('allo_stock_dom'), "국내주식", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 autonumericInput(ns('allo_bond_mat'), "만기보유채권", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 autonumericInput(ns('allo_alter_real'), "실물자산", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 selectInput(ns('allo_new'), "신규/수정", choices = "신규",
                             width = '100%')
          ),
          column(width = 6, class = "col-12 col-md-6",
                 selectInput(ns('allo_mode'), "구분",
                             choices = c("SAA", "TAA1", "TAA2"), width = '100%'),
                 autonumericInput(ns('allo_stock_ovs'), "해외주식", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 autonumericInput(ns('allo_bond_mkt'), "시장형채권", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 autonumericInput(ns('allo_alter_inc'), "인컴자산", value = 0,
                                  width = '100%', decimalPlaces = 3),
                 br(),
                 fluidRow(
                   column(width = 4,
                          actionButton(ns("allo_add"), "추가", status = "info", width = '100%')),
                   column(width = 4,
                          actionButton(ns("allo_modi"), "수정", status = "success", width = '100%')),
                   column(width = 4,
                          actionButton(ns("allo_del"), "삭제", status = "primary", width = '100%'))
                 )
          )
        )
      )
    })

    ## 연도 선택
    output$allo_year <- renderUI({
      sk_b()
      df <- ma_b()$read('allo_table')
      if (nrow(df) == 0) return(NULL)
      y <- df %>% arrange(desc(배분일자)) %>%
        mutate(배분일자 = year(as.Date(배분일자))) %>%
        pull(배분일자) %>% unique()
      selectInput(ns('allo_year_select'), "조회 연도", choices = y,
                  selected = y[1], width = '100%')
    })

    ## 배분 테이블
    output$allo_table_ui <- renderUI({
      sk_b()
      req(input$allo_year_select)
      df <- ma_b()$read('allo_table')
      if (nrow(df) == 0) return(tags$p("입력된 자산배분 기록이 없습니다."))

      df %>%
        filter(year(as.Date(배분일자)) == as.numeric(input$allo_year_select)) %>%
        select(행번호, 배분일자, 구분, 국내주식, 해외주식, 만기보유채권,
               시장형채권, 실물자산, 인컴자산) %>%
        mutate(현금성 = 1 - (국내주식 + 해외주식 + 만기보유채권 + 시장형채권 +
                            실물자산 + 인컴자산)) %>%
        arrange(배분일자, 행번호) %>%
        flextable() %>% theme_vanilla() %>%
        colformat_double(j = 4:10, digits = 3) %>%
        set_table_properties(layout = 'autofit') %>%
        htmltools_value(ft.align = 'center')
    })

    # === b. 성과분석 ===

    raw_bm_data <- reactive({
      req(input$base_month)
      ma_v()$get_benchmark_returns(input$base_month)
    })

    output$dynamic_boxes <- renderUI({
      t_date <- ma_v()$get_target_date(input$base_month)
      fluidRow(
        box(title = paste0(month(t_date), "월 MTD (BM vs MyPF)"),
            width = 6, status = "primary", solidHeader = TRUE,
            plotOutput(ns("plot_mtd_bm"))),
        box(title = paste0(month(t_date), "월 MTD (BMPF vs MyPF)"),
            width = 6, status = "primary", solidHeader = TRUE,
            plotOutput(ns("plot_mtd_pf"))),
        box(title = paste0(quarter(t_date), "분기 QTD (BM vs MyPF)"),
            width = 6, status = "info", solidHeader = TRUE,
            plotOutput(ns("plot_qtd_bm"))),
        box(title = paste0(quarter(t_date), "분기 QTD (BMPF vs MyPF)"),
            width = 6, status = "info", solidHeader = TRUE,
            plotOutput(ns("plot_qtd_pf"))),
        box(title = paste0(year(t_date), "년 YTD (BM vs MyPF)"),
            width = 6, status = "success", solidHeader = TRUE,
            plotOutput(ns("plot_ytd_bm"))),
        box(title = paste0(year(t_date), "년 YTD (BMPF vs MyPF)"),
            width = 6, status = "success", solidHeader = TRUE,
            plotOutput(ns("plot_ytd_pf")))
      )
    })

    cols_bm <- c("기준일", "MyPF", "코스피", "S&P", "금현물", "리츠", "회사채", "시장형채권")
    cols_pf <- c("기준일", "MyPF", "SAA", "TAA1", "TAA2")

    output$plot_mtd_bm <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_bm,
                            floor_date(ma_v()$get_target_date(input$base_month), "month") - days(1))
    })
    output$plot_mtd_pf <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_pf,
                            floor_date(ma_v()$get_target_date(input$base_month), "month") - days(1))
    })
    output$plot_qtd_bm <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_bm,
                            floor_date(ma_v()$get_target_date(input$base_month), "quarter") - days(1))
    })
    output$plot_qtd_pf <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_pf,
                            floor_date(ma_v()$get_target_date(input$base_month), "quarter") - days(1))
    })
    output$plot_ytd_bm <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_bm,
                            floor_date(ma_v()$get_target_date(input$base_month), "year") - days(1))
    })
    output$plot_ytd_pf <- renderPlot({
      ma_v()$plot_pf_return(raw_bm_data(), cols_pf,
                            floor_date(ma_v()$get_target_date(input$base_month), "year") - days(1))
    })
  })
}
