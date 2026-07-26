# =============================================================================
# mod_allo_strategy — 배분전략 모듈
# =============================================================================

mod_allo_strategy_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("allo_box"),

    ## a. 자산배분====
    nav_panel(
      title = "자산배분",
      fluidRow(
        column(
          width = 4, class = "col-12 col-md-6 col-lg-4",
          card(
            class = "mb-3 border-info",
            card_header("자산배분 입력", class = "bg-info text-white"),
            card_body(
              uiOutput(ns("allo_input"))
            )
          )
        ),
        column(
          width = 8, class = "col-12 col-md-6 col-lg-8",
          card(
            class = "mb-3 border-info",
            card_header("자산배분 시계열", class = "bg-info text-white"),
            card_body(
              fluidRow(column(3, uiOutput(ns("allo_year")))),
              fluidRow(reactableOutput(ns("allo_table_ui")))
            )
          )
        )
      )
    ),

    ## b. 배분성과====
    nav_panel(
      title = "배분성과",
      fluidRow(
        column(
          width = 12,
          div(
            class = "d-flex align-items-end gap-2 mb-3",
            div(
              airDatepickerInput(
                ns("base_month"),
                label = "기준 연월 선택",
                value = Sys.Date(), view = "months",
                minView = "months", dateFormat = "yyyy-MM",
                width = "250px", addon = "right"
              )
            ),
            div(
              actionButton(
                ns("allo_query"), "조회",
                class = "btn btn-primary"
              )
            )
          )
        )
      ),
      uiOutput(ns("dynamic_boxes"))
    )
  )
}

mod_allo_strategy_server <- function(id, pool, ma, ma_b, ma_v, sk_b, menu_tabs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_delay <- function(text, type) show_alert(title = text, type = type)

    # === a. 자산배분 CRUD ====

    ## 신규/수정 선택 옵션 동적 생성 ----
    observe({
      sk_b()
      df <- ma_b()$read("allo_table") %>% arrange(행번호)
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

    ## 신규/수정 선택 시 기존 데이터 불러오기 ----
    observeEvent(input$allo_new, {
      req(input$allo_new)
      if (input$allo_new != "신규") {
        df <- ma_b()$read("allo_table")
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

    ## 추가 ----
    observeEvent(input$allo_add, {
      df_current <- ma_b()$read("allo_table")
      next_id <- ifelse(nrow(df_current) == 0, 1,
        max(as.numeric(df_current$행번호), na.rm = TRUE) + 1
      )
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
      dbxInsert(conn = pool, table = "allo_table", records = new_row)
      sk_b(!sk_b())
      show_delay("자산배분 내역이 추가되었습니다.", "success")
    })

    ## 수정 ----
    observeEvent(input$allo_modi, {
      if (input$allo_new == "신규") {
        return(show_delay("수정할 행을 선택해주세요.", "warning"))
      }
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
      dbxUpdate(
        conn = pool, table = "allo_table", records = mod_row,
        where_cols = c("행번호")
      )
      sk_b(!sk_b())
      show_delay("성공적으로 수정되었습니다.", "success")
    })

    ## 삭제 ----
    observeEvent(input$allo_del, {
      if (input$allo_new == "신규") {
        return(show_delay("삭제할 행을 선택해주세요.", "error"))
      }
      dbxDelete(
        conn = pool, table = "allo_table",
        where = data.frame(행번호 = as.numeric(input$allo_new))
      )
      updateSelectInput(session, "allo_new", selected = "신규")
      sk_b(!sk_b())
      show_delay("자산배분 내역이 삭제되었습니다.", "success")
    })

    ## 입력 UI ----
    output$allo_input <- renderUI({
      column(
        width = 12,
        fluidRow(
          column(
            width = 6, class = "col-12 col-md-6",
            airDatepickerInput(ns("allo_date"), "배분일자",
              addon = "none",
              value = Sys.Date(), width = "100%"
            ),
            autonumericInput(ns("allo_stock_dom"), "국내주식",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            autonumericInput(ns("allo_bond_mat"), "만기보유채권",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            autonumericInput(ns("allo_alter_real"), "실물자산",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            selectInput(ns("allo_new"), "신규/수정",
              choices = "신규",
              width = "100%"
            )
          ),
          column(
            width = 6, class = "col-12 col-md-6",
            selectInput(ns("allo_mode"), "구분",
              choices = c("SAA", "TAA1", "TAA2"), width = "100%"
            ),
            autonumericInput(ns("allo_stock_ovs"), "해외주식",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            autonumericInput(ns("allo_bond_mkt"), "시장형채권",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            autonumericInput(ns("allo_alter_inc"), "인컴자산",
              value = 0,
              width = "100%", decimalPlaces = 3
            ),
            br(),
            fluidRow(
              column(
                width = 4,
                actionButton(ns("allo_add"), "추가", class = "btn btn-info", width = "100%")
              ),
              column(
                width = 4,
                actionButton(ns("allo_modi"), "수정", class = "btn btn-success", width = "100%")
              ),
              column(
                width = 4,
                actionButton(ns("allo_del"), "삭제", class = "btn btn-primary", width = "100%")
              )
            )
          )
        )
      )
    })

    ## 연도 선택 ----
    output$allo_year <- renderUI({
      sk_b()
      df <- ma_b()$read("allo_table")
      if (nrow(df) == 0) {
        return(NULL)
      }
      y <- df %>%
        arrange(desc(배분일자)) %>%
        mutate(배분일자 = year(as.Date(배분일자))) %>%
        pull(배분일자) %>%
        unique()
      selectInput(ns("allo_year_select"), "조회 연도",
        choices = y,
        selected = y[1], width = "100%"
      )
    })

    ## 배분 테이블 ----
    output$allo_table_ui <- renderReactable({
      sk_b()
      req(menu_tabs() == "pf_allo_strategy")
      req(input$allo_year_select)
      df <- ma_b()$read("allo_table")
      if (nrow(df) == 0) {
        return(tags$p("입력된 자산배분 기록이 없습니다."))
      }

      df <- df %>%
        filter(year(as.Date(배분일자)) == as.numeric(input$allo_year_select)) %>%
        select(
          행번호, 배분일자, 구분, 국내주식, 해외주식, 만기보유채권,
          시장형채권, 실물자산, 인컴자산
        ) %>%
        mutate(현금성 = 1 - (국내주식 + 해외주식 + 만기보유채권 + 시장형채권 +
          실물자산 + 인컴자산)) %>%
        arrange(배분일자, 행번호)

      render_rt(df, pct_cols = 4:10)
    })

    # === b. 배분성과 ====
    allo_trigger <- reactiveVal(0)
    sel_base_month_rv <- reactiveVal(NULL)

    observeEvent(input$allo_query, {
      req(input$base_month)
      sel_base_month_rv(input$base_month)
      showModal(modalDialog(
        title = NULL,
        div(
          class = "text-center my-4",
          tags$i(class = "fa fa-spinner fa-spin fa-3x text-primary mb-3"),
          h5("배분성과 데이터 산출 중...", class = "mb-2"),
          p("벤치마크 및 포트폴리오 성과를 비교 계산하고 있습니다. 잠시만 기다려주세요.", class = "text-muted small mb-0")
        ),
        footer = NULL,
        easyClose = FALSE,
        size = "m"
      ))
      next_trig <- allo_trigger() + 1
      later::later(function() {
        allo_trigger(next_trig)
      }, delay = 0.1)
    }, ignoreInit = TRUE)

    raw_bm_data <- eventReactive(allo_trigger(), {
      req(sel_base_month_rv())
      ma_obj <- ma_v()

      # calc_benchmark_returns 순수 함수 호출
      res <- calc_benchmark_returns(
        return_tbl    = ma_obj$read_obj("return"),
        cash_in_out   = ma_obj$cash_in_out,
        allo_table_df = ma_obj$read("allo_table"),
        base_month    = sel_base_month_rv(),
        today         = ma_obj$today
      )
      removeModal()
      res
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    output$dynamic_boxes <- renderUI({
      if (allo_trigger() == 0) {
        return(
          div(
            class = "text-center mt-5 text-muted",
            tags$i(class = "fa fa-chart-pie fa-3x mb-3"),
            h5("기준 연월을 선택하고 조회 버튼을 누르세요."),
            p("선택한 연월 기준 MTD, QTD, YTD 배분성과를 BM과 비교 분석합니다.", class = "small")
          )
        )
      }
      req(sel_base_month_rv())
      t_date <- get_target_date(sel_base_month_rv(), ma_v()$today)
      fluidRow(
        column(width = 6, card(
          class = "mb-3 border-primary",
          card_header(paste0(month(t_date), "월 MTD (BM vs MyPF)"), class = "bg-primary text-white"),
          card_body(echarts4rOutput(ns("plot_mtd_bm")))
        )),
        column(width = 6, card(
          class = "mb-3 border-primary",
          card_header(paste0(month(t_date), "월 MTD (BMPF vs MyPF)"), class = "bg-primary text-white"),
          card_body(echarts4rOutput(ns("plot_mtd_pf")))
        )),
        column(width = 6, card(
          class = "mb-3 border-info",
          card_header(paste0(quarter(t_date), "분기 QTD (BM vs MyPF)"), class = "bg-info text-white"),
          card_body(echarts4rOutput(ns("plot_qtd_bm")))
        )),
        column(width = 6, card(
          class = "mb-3 border-info",
          card_header(paste0(quarter(t_date), "분기 QTD (BMPF vs MyPF)"), class = "bg-info text-white"),
          card_body(echarts4rOutput(ns("plot_qtd_pf")))
        )),
        column(width = 6, card(
          class = "mb-3 border-success",
          card_header(paste0(year(t_date), "년 YTD (BM vs MyPF)"), class = "bg-success text-white"),
          card_body(echarts4rOutput(ns("plot_ytd_bm")))
        )),
        column(width = 6, card(
          class = "mb-3 border-success",
          card_header(paste0(year(t_date), "년 YTD (BMPF vs MyPF)"), class = "bg-success text-white"),
          card_body(echarts4rOutput(ns("plot_ytd_pf")))
        ))
      )
    })

    cols_bm <- c("기준일", "MyPF", "코스피", "S&P", "금현물", "리츠", "회사채", "시장형채권")
    cols_pf <- c("기준일", "MyPF", "SAA", "TAA1", "TAA2")

    render_pf_echart <- function(df, cols, base_date) {
      df |>
        filter(기준일 >= base_date) |>
        select(all_of(cols)) |>
        # 일간수익률 → 누적수익률로 변환 (각 자산별 독립 계산)
        mutate(across(-기준일, ~ (cumprod(1 + . / 100) - 1) * 100)) |>
        pivot_longer(-기준일) |>
        group_by(name) |>
        e_charts(기준일) |>
        e_line(value, symbol = "none") |>
        e_tooltip(trigger = "axis") |>
        e_y_axis(
          position = "right", # Y축 우측 배치
          axisLabel = list(formatter = htmlwidgets::JS(
            "function(v) { return v.toFixed(1) + '%'; }"
          ))
        ) |>
        e_datazoom() |>
        e_legend(right = 0, top = "center", orient = "vertical") |>
        e_grid(right = "20%", left = "3%")
    }


    output$plot_mtd_bm <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "month") - days(1))
    })
    output$plot_mtd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "month") - days(1))
    })
    output$plot_qtd_bm <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "quarter") - days(1))
    })
    output$plot_qtd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "quarter") - days(1))
    })
    output$plot_ytd_bm <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "year") - days(1))
    })
    output$plot_ytd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_allo_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(sel_base_month_rv(), ma_v()$today), "year") - days(1))
    })
  })
}
