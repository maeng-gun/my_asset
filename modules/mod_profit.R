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
      fluidRow(reactableOutput(ns("profit_var")))
    ),

    ## b. 손익변동 ----
    nav_panel(
      title = "손익변동",
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
          # [Row 1] 총손익 콤보 차트 (일간손익 막대 + 손익누계 꺾은선)
          fluidRow(
            column(12, echarts4rOutput(ns("total_profit_bar"), height = "360px"))
          ),
          # [Row 2] 선진국 | 신흥국
          fluidRow(
            column(6, echarts4rOutput(ns("chart_선진국"), height = "300px")),
            column(6, echarts4rOutput(ns("chart_신흥국"), height = "300px"))
          ),
          # [Row 3] 실물자산 | 인컴자산
          fluidRow(
            column(6, echarts4rOutput(ns("chart_실물자산"), height = "300px")),
            column(6, echarts4rOutput(ns("chart_인컴자산"), height = "300px"))
          ),
          # [Row 4] 채권 | 현금성
          fluidRow(
            column(6, echarts4rOutput(ns("chart_채권"), height = "300px")),
            column(6, echarts4rOutput(ns("chart_현금성"), height = "300px"))
          ),
          # [Row 5] 평가금액 추이 차트
          fluidRow(
            column(12, echarts4rOutput(ns("total_profit_trend"), height = "360px"))
          )
        )
      )
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

      df <- ma_obj$total_profit
      render_rt(df,
        int_cols = 2:8,
        pct_cols = 9:11,
        sticky_cols = names(df)[1],
        border_cols = "총손익",
        height = NULL,
        dynamic_height = FALSE
      )
    })

    output$profit_var <- renderReactable({
      req(menu_tabs() == "pf_bs_pl")
      ma_obj <- ma_v()

      df <- ma_obj$profit_variation

      # 최초 로딩 완료 콜백
      isolate({
        if (!initial_done()) {
          on_initial_load()
          initial_done(TRUE)
        }
      })

      render_rt(df,
        int_cols    = c(4:6, 8, 10, 12, 14),
        pct_cols    = c(7, 9, 11, 13, 15),
        sticky_cols = names(df)[1:3]
      )
    })

    ## b. 손익변동 ----
    ## 손익 그래프 데이터 (일간손익 + 손익누계) ----
    df_graph <- reactive({
      input$total_s_date
      input$total_e_date
      ma_obj <- ma_v()

      build_profit_trend_data(
        return_tbl  = ma_obj$read_obj("return"),
        start       = input$total_s_date,
        end         = input$total_e_date
      )
    })

    ## 자산군별 손익누계 그래프 데이터 ----
    df_asset_graph <- reactive({
      input$total_s_date
      input$total_e_date
      ma_obj <- ma_v()

      build_asset_profit_data(
        return_tbl = ma_obj$read_obj("return"),
        start      = input$total_s_date,
        end        = input$total_e_date
      )
    })

    ## [위] 손익 콤보 차트 — 일간손익(막대) + 손익누계(꺾은선) ----
    output$total_profit_bar <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")


      df_graph() |>
        e_charts(기준일) |>
        e_bar(일간손익, name = "일간손익") |>
        e_line(손익누계, name = "손익누계", symbol = "none") |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%", left = "3%") |>
        e_legend(right = 0, top = "center", orient = "vertical")
    })

    ## [아래] 평가금액 추이 차트 — 원금 라인 제거, 평가금액만 표시 ----
    output$total_profit_trend <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      ma_obj <- ma_v()

      # build_eval_trend_data: 평가금액만 반환 (원금 제거됨)
      fig1_df <- ma_obj$eval_trend_data

      fig1_df |>
        group_by(구분) |>
        e_charts(기준일) |>
        e_line(평가금액, name = "평가금액", symbol = "none") |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%", left = "3%") |>
        e_legend(right = 0, top = "center", orient = "vertical")
    })


    ## b-1. 자산군별 손익누계 차트 (선진국, 신흥국, 실물자산, 인컴자산, 채권, 현금성) ----
    make_asset_chart <- function(df, title) {
      if (nrow(df) == 0) {
        return(
          echarts4r::e_charts() |>
            echarts4r::e_title(text = title)
        )
      }
      df |>
        e_charts(기준일) |>
        e_line(손익누계, name = title, symbol = "none") |>
        e_title(text = title, left = "center", textStyle = list(fontSize = 13)) |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(x_index = 0, type = "slider") |>
        e_y_axis(position = "right") |>
        e_grid(right = "15%", left = "3%", top = "40px", bottom = "50px") |>
        e_legend(show = FALSE)
    }

    output$chart_선진국 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$선진국, "선진국 주식")
    })

    output$chart_신흥국 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$신흥국, "신흥국(한국 포함) 주식")
    })

    output$chart_실물자산 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$실물자산, "실물자산")
    })

    output$chart_인컴자산 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$인컴자산, "인컴자산")
    })

    output$chart_채권 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$채권, "채권")
    })

    output$chart_현금성 <- renderEcharts4r({
      req(menu_tabs() == "pf_bs_pl")
      make_asset_chart(df_asset_graph()$현금성, "현금성")
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
