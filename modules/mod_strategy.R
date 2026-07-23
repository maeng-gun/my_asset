# =============================================================================
# mod_strategy — 배분전략 및 성과분석 모듈
# =============================================================================
# DB CRUD는 pool 객체 직접 주입, 성과분석은 순수 함수 호출
# =============================================================================

mod_strategy_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("bm_box"),

    ## 투자성과 탭 ====
    nav_panel(
      title = "투자성과",
      fluidRow(
        column(
          width = 2, class = "col-12 col-md-4 col-lg-2",
          airDatepickerInput(ns("perf_s_date"),
            label = "시작일", addon = "none",
            value = Sys.Date() %m-% years(1)
          ),
          airDatepickerInput(ns("perf_e_date"),
            label = "종료일", addon = "none",
            value = Sys.Date()
          )
        ),
        column(
          width = 10, class = "col-12 col-md-8 col-lg-10",
          h6(class = "text-muted mt-2 mb-0",
             "선진국 주식 (BM: KODEX 선진국MSCI World, 360200)"),
          echarts4rOutput(ns("perf_line_선진국"), height = "400px"),
          echarts4rOutput(ns("perf_dd_선진국"),   height = "100px"),
          h6(class = "text-muted mt-3 mb-0",
             "국내 주식 (BM: KODEX 코스피, 305050)"),
          echarts4rOutput(ns("perf_line_국내"),   height = "400px"),
          echarts4rOutput(ns("perf_dd_국내"),     height = "100px"),
          h6(class = "text-muted mt-3 mb-0",
             "실물자산 (BM: KODEX 골드선물(H), 411060)"),
          echarts4rOutput(ns("perf_line_실물"),   height = "400px"),
          echarts4rOutput(ns("perf_dd_실물"),     height = "100px"),
          h6(class = "text-muted mt-3 mb-0",
             "인컴자산 (BM: TIGER 리츠부동산인프라, 329200)"),
          echarts4rOutput(ns("perf_line_인컴"),   height = "400px"),
          echarts4rOutput(ns("perf_dd_인컴"),     height = "100px"),
          h6(class = "text-muted mt-3 mb-0",
             "채권 (BM: 회사채 3년)"),
          echarts4rOutput(ns("perf_line_채권"),   height = "400px"),
          echarts4rOutput(ns("perf_dd_채권"),     height = "100px")
        )
      )
    ),

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
          airDatepickerInput(ns("base_month"),
            label = "기준 연월 선택",
            value = Sys.Date(), view = "months",
            minView = "months", dateFormat = "yyyy-MM",
            width = "300px", addon = "right"
          )
        )
      ),
      uiOutput(ns("dynamic_boxes"))
    )
  )
}

mod_strategy_server <- function(id, pool, ma, ma_b, ma_v, sk_b, menu_tabs) {
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
      req(menu_tabs() == "pf_strategy")
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

    raw_bm_data <- reactive({
      req(input$base_month)
      ma_obj <- ma_v()

      # calc_benchmark_returns 순수 함수 호출
      calc_benchmark_returns(
        return_tbl    = ma_obj$read_obj("return"),
        cash_in_out   = ma_obj$cash_in_out,
        allo_table_df = ma_obj$read("allo_table"),
        base_month    = input$base_month,
        today         = ma_obj$today
      )
    })

    output$dynamic_boxes <- renderUI({
      t_date <- get_target_date(input$base_month, ma_v()$today)
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
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(input$base_month, ma_v()$today), "month") - days(1))
    })
    output$plot_mtd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(input$base_month, ma_v()$today), "month") - days(1))
    })
    output$plot_qtd_bm <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(input$base_month, ma_v()$today), "quarter") - days(1))
    })
    output$plot_qtd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(input$base_month, ma_v()$today), "quarter") - days(1))
    })
    output$plot_ytd_bm <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_bm, floor_date(get_target_date(input$base_month, ma_v()$today), "year") - days(1))
    })
    output$plot_ytd_pf <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_pf_echart(raw_bm_data(), cols_pf, floor_date(get_target_date(input$base_month, ma_v()$today), "year") - days(1))
    })

    # === 투자성과 ====
    # 해당 탭에 실제 진입했을 때만 데이터 산출/렌더링
    # req(menu_tabs() == "pf_strategy") 조건으로 제어

    ## BM 데이터 reactive ----
    raw_perf_data <- reactive({
      req(menu_tabs() == "pf_strategy")
      req(input$perf_s_date, input$perf_e_date)
      ma_obj <- ma_v()
      build_asset_bm_data(
        return_tbl = ma_obj$read_obj("return"),
        start      = input$perf_s_date,
        end        = input$perf_e_date
      )
    })

    ## 그래프 헬퍼 ----
    render_perf_line <- function(df, group_id) {
      req(nrow(df) > 0)
      df %>%
        select(기준일, MyPF, BM) %>%
        pivot_longer(-기준일, names_to = "구분", values_to = "value") %>%
        group_by(구분) %>%
        e_charts(기준일) %>%
        e_line(value, symbol = "none") %>%
        e_connect_group(group_id) %>%
        e_y_axis(
          position = "right",
          axisLabel = list(
            formatter = htmlwidgets::JS("function(v){return v.toFixed(1)+'%';}")
          )
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(right = 0, top = "center", orient = "vertical") %>%
        e_grid(right = "20%", left = "3%")
    }

    render_perf_dd <- function(df, group_id) {
      req(nrow(df) > 0)
      df %>%
        select(기준일, DD) %>%
        e_charts(기준일) %>%
        e_area(DD, name = "Drawdown", symbol = "none", color = "#dc3545") %>%
        e_connect_group(group_id) %>%
        e_y_axis(
          max = 0,
          position = "right",
          axisLabel = list(
            formatter = htmlwidgets::JS("function(v){return v.toFixed(1)+'%';}")
          )
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_grid(right = "20%", left = "3%")
    }

    ## 렌더링 (선진국) ----
    output$perf_line_선진국 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_line(raw_perf_data()$선진국, "perf_선진국")
    })
    output$perf_dd_선진국 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_dd(raw_perf_data()$선진국, "perf_선진국")
    })

    ## 렌더링 (국내) ----
    output$perf_line_국내 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_line(raw_perf_data()$국내, "perf_국내")
    })
    output$perf_dd_국내 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_dd(raw_perf_data()$국내, "perf_국내")
    })

    ## 렌더링 (실물자산) ----
    output$perf_line_실물 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_line(raw_perf_data()$실물자산, "perf_실물")
    })
    output$perf_dd_실물 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_dd(raw_perf_data()$실물자산, "perf_실물")
    })

    ## 렌더링 (인컴자산) ----
    output$perf_line_인컴 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_line(raw_perf_data()$인컴자산, "perf_인컴")
    })
    output$perf_dd_인컴 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_dd(raw_perf_data()$인컴자산, "perf_인컴")
    })

    ## 렌더링 (채권) ----
    output$perf_line_채권 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_line(raw_perf_data()$채권, "perf_채권")
    })
    output$perf_dd_채권 <- renderEcharts4r({
      req(menu_tabs() == "pf_strategy")
      render_perf_dd(raw_perf_data()$채권, "perf_채권")
    })
  })
}
