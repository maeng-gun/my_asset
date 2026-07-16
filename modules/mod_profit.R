# =============================================================================
# mod_profit — 손익현황 모듈 (종합손익, 손익변동, 자산군별, 계좌별, 상품별)
# =============================================================================
# 순수 함수(utils_analytics.R)를 호출하여 계산 수행
# R6 인스턴스의 특정 속성만 reactive()로 참조
# =============================================================================

mod_profit_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("pf_box1"),

## a. 종합손익 ----
    nav_panel(
      title = "종합손익",
      fluidRow(reactableOutput(ns("t_profit1"))),
      fluidRow(
        column(
          width = 2, class = "col-12 col-md-4 col-lg-2",
          airDatepickerInput(ns("total_s_date"),
            label = "시작일",
            addon = "none",
            value = make_date(year(Sys.Date()), 1, 1) - 1
          ),
          airDatepickerInput(ns("total_e_date"),
            label = "종료일",
            addon = "none", value = Sys.Date()
          )
        ),
        column(
          width = 10, class = "col-12 col-md-8 col-lg-10",
          # 손익 차트(콤보 막대+꺾은선)를 위에, 평가금액 꺾은선을 아래에 배치
          echarts4rOutput(ns("total_profit_bar"), height = "400px"),
          echarts4rOutput(ns("total_profit_trend"), height = "400px")
        )
      )
    ),

## b. 손익변동 ----
    nav_panel(
      title = "손익변동",
      fluidRow(reactableOutput(ns("profit_var")))
    ),

## c. 자산군별 손익현황 ----
    nav_panel(
      title = "자산군별",
      fluidRow(reactableOutput(ns("total_accounts1")))
    ),

## d. 계좌별 손익현황 ----
    nav_panel(
      title = "계좌별",
      fluidRow(reactableOutput(ns("total_accounts2")))
    ),

## e. 계좌별상품 손익현황 ----
    nav_panel(
      title = "계좌별상품",
      fluidRow(reactableOutput(ns("bs_pl_mkt_a")))
    ),

## f. 자산군별상품 손익현황 ----
    nav_panel(
      title = "자산군별상품",
      fluidRow(reactableOutput(ns("bs_pl_mkt_a2")))
    )
  )
}

mod_profit_server <- function(id, ma_v, menu_tabs, on_initial_load) {
  moduleServer(id, function(input, output, session) {
    initial_done <- reactiveVal(FALSE)

## a. 종합손익 테이블 ----
    output$t_profit1 <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      ma_obj <- ma_v()

      df <- calc_total_profit(
        book_info       = ma_obj$book_info,
        eval_profit_tbl = ma_obj$read_obj("eval_profit"),
        return_tbl      = ma_obj$read_obj("return"),
        today           = ma_obj$today,
        cur_year        = ma_obj$year
      )
      render_rt(df,
        int_cols = 2:8,
        pct_cols = 9:11,
        sticky_cols = names(df)[1],
        height = NULL,
        dynamic_height = FALSE
      )
    })

## 손익 그래프 데이터 (일간손익 + 손익누계) ----
    df_graph <- reactive({
      input$total_s_date
      input$total_e_date
      ma_obj <- ma_v()

      build_profit_trend_data(
        return_tbl  = ma_obj$read_obj("return"),
        cash_in_out = ma_obj$cash_in_out,
        start       = input$total_s_date,
        end         = input$total_e_date
      )
    })

    y_num <- reactive({
      ceiling(max(abs(df_graph()$손익누계)) / 10)
    })

## [위] 손익 콤보 차트 — 일간손익(막대) + 손익누계(꺾은선) ----
    output$total_profit_bar <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")

      # 최초 로딩 완료 콜백
      isolate({
        if (!initial_done()) {
          on_initial_load()
          initial_done(TRUE)
        }
      })

      df_graph() |>
        e_charts(기준일) |>
        e_bar(일간손익, name = "일간손익") |>
        e_line(손익누계, name = "손익누계", symbol = "none") |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%") |>
        e_legend(right = 0, top = "center", orient = "vertical")
    })

## [아래] 평가금액 추이 차트 — 원금 라인 제거, 평가금액만 표시 ----
    output$total_profit_trend <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      ma_obj <- ma_v()

      # build_eval_trend_data: 평가금액만 반환 (원금 제거됨)
      fig1_df <- build_eval_trend_data(
        return_tbl = ma_obj$read_obj("return"),
        inflow_df  = ma_obj$inflow,
        today      = ma_obj$today
      )

      fig1_df |>
        group_by(구분) |>
        e_charts(기준일) |>
        e_line(평가금액, name = "평가금액", symbol = "none") |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%") |>
        e_legend(right = 0, top = "center", orient = "vertical")
    })

## b. 손익변동 ----
    output$profit_var <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      ma_obj <- ma_v()

      df <- calc_profit_variation(
        return_tbl = ma_obj$read_obj("return"),
        t_comm3    = ma_obj$t_comm3,
        today      = ma_obj$today,
        cur_year   = ma_obj$year
      )
      render_rt(df,
        int_cols    = c(4:6, 8, 10, 12, 14),
        pct_cols    = c(7, 9, 11, 13, 15),
        sticky_cols = names(df)[1:3]
      )
    })

## c. 자산군별 손익현황 ----
    output$total_accounts1 <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      df <- ma_v()$t_comm3 %>% arrange(자산군, 세부자산군)
      render_rt(df,
        int_cols    = 4:10,
        pct_cols    = 11:14,
        sticky_cols = names(df)[1:3]
      )
    })

## d. 계좌별 손익현황 ----
    output$total_accounts2 <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      df <- ma_v()$t_comm4 %>% arrange(계좌)
      render_rt(df,
        int_cols    = 3:9,
        pct_cols    = 10:13,
        sticky_cols = names(df)[1:2]
      )
    })

## e. 계좌별상품 손익현황 ----
    output$bs_pl_mkt_a <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      df <- ma_v()$comm_profit %>%
        arrange(계좌, 통화, 자산군, 세부자산군, 세부자산군2)
      render_rt(df,
        int_cols      = 7:14,
        pct_cols      = 15:18,
        sticky_cols   = names(df)[1:6]
      )
    })

## f. 자산군별상품 손익현황 ----
    output$bs_pl_mkt_a2 <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      df <- ma_v()$comm_profit2 %>%
        arrange(자산군, 세부자산군, 세부자산군2, 종목명, 계좌, 통화)
      render_rt(df,
        int_cols      = 7:14,
        pct_cols      = 15:18,
        sticky_cols   = names(df)[1:5]
      )
    })
  })
}
