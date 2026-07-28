# =============================================================================
# mod_inv_strategy.R — 투자전략 모듈
# =============================================================================

# 1. 투자전략 모듈 UI ----
mod_inv_strategy_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("inv_box"),

    # 1.1 투자성과 탭 ----
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
          ),
          actionButton(
            ns("perf_query"), "조회",
            class = "btn btn-primary w-100 mt-2"
          ),
          actionButton(
            ns("perf_ytd"), "연초 이후",
            class = "btn btn-outline-secondary w-100 btn-sm mt-2"
          ),
          fluidRow(
            class = "mt-2 g-1",
            column(6, actionButton(ns("perf_1m"), "1M",
              class = "btn btn-outline-secondary w-100 btn-sm"
            )),
            column(6, actionButton(ns("perf_3m"), "3M",
              class = "btn btn-outline-secondary w-100 btn-sm"
            )),
            column(6, actionButton(ns("perf_6m"), "6M",
              class = "btn btn-outline-secondary w-100 btn-sm mt-1"
            )),
            column(6, actionButton(ns("perf_12m"), "12M",
              class = "btn btn-outline-secondary w-100 btn-sm mt-1"
            ))
          )
        ),
        column(
          width = 10, class = "col-12 col-md-8 col-lg-10",
          h6(
            class = "text-muted mt-2 mb-0",
            "선진국 주식 (BM: ACE 미국S&P500, 360200)"
          ),
          echarts4rOutput(ns("perf_line_선진국"), height = "400px"),
          echarts4rOutput(ns("perf_dd_선진국"), height = "200px"),
          h6(
            class = "text-muted mt-3 mb-0",
            "국내 주식 (BM: ACE 코스피, 305050)"
          ),
          echarts4rOutput(ns("perf_line_국내"), height = "400px"),
          echarts4rOutput(ns("perf_dd_국내"), height = "200px"),
          h6(
            class = "text-muted mt-3 mb-0",
            "실물자산 (BM: ACE KRX금현물, 411060)"
          ),
          echarts4rOutput(ns("perf_line_실물"), height = "400px"),
          echarts4rOutput(ns("perf_dd_실물"), height = "200px"),
          h6(
            class = "text-muted mt-3 mb-0",
            "인컴자산 (BM: TIGER 리츠부동산인프라, 329200)"
          ),
          echarts4rOutput(ns("perf_line_인컴"), height = "400px"),
          echarts4rOutput(ns("perf_dd_인컴"), height = "200px"),
          h6(
            class = "text-muted mt-3 mb-0",
            "채권 (BM: 회사채 3년(AA-))"
          ),
          echarts4rOutput(ns("perf_line_채권"), height = "400px"),
          echarts4rOutput(ns("perf_dd_채권"), height = "200px")
        )
      )
    ),

    # 1.2 종목탐색 탭 ----
    nav_panel(
      title = "종목탐색",
      fluidRow(
        column(
          width = 2, class = "col-12 col-md-4 col-lg-2",
          div(
            class = "mb-3",
            tags$label("종목 검색", class = "form-label fw-semibold"),
            selectizeInput(
              ns("ticker_select"),
              label = NULL,
              choices = character(0),
              options = list(
                placeholder = "종목명 또는 티커 입력 (예: 삼전, bond ETF)",
                create = FALSE,
                maxOptions = 100L,
                labelField = "label",
                valueField = "value",
                searchField = list("label"),
                score = I("function(search) {
                  var query = search.toLowerCase();
                  var tokens = [];
                  var currentWord = '';
                  for (var i = 0; i < query.length; i++) {
                    var ch = query[i];
                    if (ch === ' ' || ch === '\\t' || ch === '\\n' || ch === '\\r') {
                      if (currentWord !== '') { tokens.push(currentWord); currentWord = ''; }
                    } else if (/[ㄱ-ㅎ|ㅏ-ㅣ|가-힣]/.test(ch)) {
                      if (currentWord !== '') { tokens.push(currentWord); currentWord = ''; }
                      tokens.push(ch);
                    } else {
                      currentWord += ch;
                    }
                  }
                  if (currentWord !== '') { tokens.push(currentWord); }
                  return function(item) {
                    var text = (item.label || '').toLowerCase();
                    for (var i = 0; i < tokens.length; i++) {
                      if (text.indexOf(tokens[i]) === -1) return 0;
                    }
                    return 1;
                  };
                }")
              )
            )
          ),
          div(
            class = "mb-3",
            radioButtons(
              ns("bm_select"),
              label    = tags$span("벤치마크", class = "fw-semibold"),
              choices  = c("코스피" = "226490.KS", "S&P500" = "SPY"),
              selected = "226490.KS"
            )
          ),
          actionButton(
            ns("ticker_query"), "조회",
            class = "btn btn-primary w-100"
          )
        ),
        column(
          width = 10, class = "col-12 col-md-8 col-lg-10",
          div(
            id = ns("ticker_init_msg"),
            class = "text-center mt-5 text-muted",
            tags$i(class = "fa fa-chart-line fa-3x mb-3"),
            h5("종목을 선택하고 조회 버튼을 누르세요."),
            p("최근 10년간 개별 종목의 성과를 BM과 비교 분석합니다.",
              class = "small"
            )
          ),
          div(
            id = ns("ticker_result_box"),
            style = "display: none;",
            uiOutput(ns("ticker_header_ui")),
            div(class = "mb-3", reactableOutput(ns("stats_table"))),
            div(class = "mb-1", echarts4rOutput(ns("cum_chart"), height = "400px")),
            div(class = "mb-3", echarts4rOutput(ns("dd_chart"), height = "200px")),
            div(class = "mb-3", echarts4rOutput(ns("heatmap_chart"), height = "380px")),
            div(class = "mb-3", echarts4rOutput(ns("yearly_chart"), height = "400px")),
            div(class = "mb-3", echarts4rOutput(ns("rolling_vol_chart"), height = "400px")),
            div(class = "mb-3", echarts4rOutput(ns("rolling_sharpe_chart"), height = "400px"))
          )
        )
      )
    )
  )
}

# 2. 투자전략 모듈 서버 ----
mod_inv_strategy_server <- function(id, ma_v, pool) {
  force(pool)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 2.1 투자성과 서브탭 ----
    perf_trigger <- reactiveVal(0)
    perf_s_rv <- reactiveVal(Sys.Date() %m-% years(1))
    perf_e_rv <- reactiveVal(Sys.Date())

    observeEvent(input$perf_query,
      {
        showModal(modalDialog(
          title = NULL,
          div(
            class = "text-center my-4",
            tags$i(class = "fa fa-spinner fa-spin fa-3x text-primary mb-3"),
            h5("투자성과 및 벤치마크 데이터 산출 중...", class = "mb-2"),
            p("데이터를 수집하고 지표를 계산하고 있습니다. 잠시만 기다려주세요.", class = "text-muted small mb-0")
          ),
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        ))
        perf_s_rv(input$perf_s_date)
        perf_e_rv(input$perf_e_date)
        next_trig <- perf_trigger() + 1
        later::later(function() {
          perf_trigger(next_trig)
        }, delay = 0.1)
      },
      ignoreInit = TRUE
    )

    perf_set_period <- function(months_back) {
      showModal(modalDialog(
        title = NULL,
        div(
          class = "text-center my-4",
          tags$i(class = "fa fa-spinner fa-spin fa-3x text-primary mb-3"),
          h5("투자성과 및 벤치마크 데이터 산출 중...", class = "mb-2"),
          p("데이터를 수집하고 지표를 계산하고 있습니다. 잠시만 기다려주세요.", class = "text-muted small mb-0")
        ),
        footer = NULL,
        easyClose = FALSE,
        size = "m"
      ))
      today <- Sys.Date()
      s_date <- today %m-% months(months_back)
      updateAirDateInput(session, "perf_e_date", value = today)
      updateAirDateInput(session, "perf_s_date", value = s_date)
      perf_e_rv(today)
      perf_s_rv(s_date)
      next_trig <- perf_trigger() + 1
      later::later(function() {
        perf_trigger(next_trig)
      }, delay = 0.1)
    }

    observeEvent(input$perf_1m, perf_set_period(1), ignoreInit = TRUE)
    observeEvent(input$perf_3m, perf_set_period(3), ignoreInit = TRUE)
    observeEvent(input$perf_6m, perf_set_period(6), ignoreInit = TRUE)
    observeEvent(input$perf_12m, perf_set_period(12), ignoreInit = TRUE)
    observeEvent(input$perf_ytd,
      {
        showModal(modalDialog(
          title = NULL,
          div(
            class = "text-center my-4",
            tags$i(class = "fa fa-spinner fa-spin fa-3x text-primary mb-3"),
            h5("투자성과 및 벤치마크 데이터 산출 중...", class = "mb-2"),
            p("데이터를 수집하고 지표를 계산하고 있습니다. 잠시만 기다려주세요.", class = "text-muted small mb-0")
          ),
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        ))
        today <- Sys.Date()
        s_date <- as.Date(format(today, "%Y-01-01"))
        updateAirDateInput(session, "perf_e_date", value = today)
        updateAirDateInput(session, "perf_s_date", value = s_date)
        perf_e_rv(today)
        perf_s_rv(s_date)
        next_trig <- perf_trigger() + 1
        later::later(function() {
          perf_trigger(next_trig)
        }, delay = 0.1)
      },
      ignoreInit = TRUE
    )

    raw_perf_data <- eventReactive(perf_trigger(),
      {
        req(perf_s_rv(), perf_e_rv())
        ma_obj <- ma_v()
        res <- build_asset_bm_data(
          return_tbl = ma_obj$read_obj("return"),
          start      = perf_s_rv(),
          end        = perf_e_rv()
        )
        removeModal()
        res
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

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

    output$perf_line_선진국 <- renderEcharts4r({
      render_perf_line(raw_perf_data()$선진국, "perf_선진국")
    })
    output$perf_dd_선진국 <- renderEcharts4r({
      render_perf_dd(raw_perf_data()$선진국, "perf_선진국")
    })

    output$perf_line_국내 <- renderEcharts4r({
      render_perf_line(raw_perf_data()$국내, "perf_국내")
    })
    output$perf_dd_국내 <- renderEcharts4r({
      render_perf_dd(raw_perf_data()$국내, "perf_국내")
    })

    output$perf_line_실물 <- renderEcharts4r({
      render_perf_line(raw_perf_data()$실물자산, "perf_실물")
    })
    output$perf_dd_실물 <- renderEcharts4r({
      render_perf_dd(raw_perf_data()$실물자산, "perf_실물")
    })

    output$perf_line_인컴 <- renderEcharts4r({
      render_perf_line(raw_perf_data()$인컴자산, "perf_인컴")
    })
    output$perf_dd_인컴 <- renderEcharts4r({
      render_perf_dd(raw_perf_data()$인컴자산, "perf_인컴")
    })

    output$perf_line_채권 <- renderEcharts4r({
      render_perf_line(raw_perf_data()$채권, "perf_채권")
    })
    output$perf_dd_채권 <- renderEcharts4r({
      render_perf_dd(raw_perf_data()$채권, "perf_채권")
    })

    # --- 2.2 종목탐색 서브탭 ----
    .pool <- pool
    tickers_map_rv <- reactiveVal(list())
    observe({
      tickers_df <- DBI::dbReadTable(.pool, "tickers") |> as_tibble()
      choices_vec <- setNames(
        tickers_df$티커,
        paste0(tickers_df$종목명, "  (", tickers_df$티커, ")")
      )
      t_map <- setNames(as.character(tickers_df$종목명), as.character(tickers_df$티커))
      tickers_map_rv(t_map)

      suppressWarnings(
        updateSelectizeInput(session, "ticker_select",
          choices = choices_vec, server = FALSE, selected = character(0)
        )
      )
    })

    ticker_data <- reactiveVal(NULL)
    is_queried <- reactiveVal(FALSE)

    observeEvent(input$ticker_query,
      {
        req(input$ticker_select, input$bm_select)

        showModal(modalDialog(
          title = NULL,
          div(
            class = "text-center my-4",
            tags$i(class = "fa fa-spinner fa-spin fa-3x text-primary mb-3"),
            h5("종목 성과 및 지표 분석 중...", class = "mb-2"),
            p("선택한 종목과 벤치마크 데이터를 분석하고 있습니다. 잠시만 기다려주세요.", class = "text-muted small mb-0")
          ),
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        ))

        t_val <- input$ticker_select
        b_val <- input$bm_select
        t_map <- tickers_map_rv()

        later::later(function() {
          shiny::withReactiveDomain(session, {
            t_name <- if (t_val %in% names(t_map)) t_map[[t_val]] else t_val
            b_name <- if (b_val == "226490.KS" || b_val == "305050" || b_val == "305050.KS") "코스피" else if (b_val == "SPY") "S&P500" else b_val

            res <- tryCatch(
              {
                build_ticker_analysis_data(
                  ticker      = t_val,
                  bm_ticker   = b_val,
                  ticker_name = t_name,
                  bm_name     = b_name
                )
              },
              error = function(e) NULL
            )

            removeModal()

            if (is.null(res)) {
              shinyjs::show("ticker_init_msg")
              shinyjs::hide("ticker_result_box")
              showModal(modalDialog(
                title = "안내",
                div(class = "alert alert-warning mb-0", "데이터를 불러오지 못했습니다. 티커를 확인하세요."),
                easyClose = TRUE,
                footer = modalButton("확인")
              ))
              return()
            }

            shinyjs::hide("ticker_init_msg")
            shinyjs::show("ticker_result_box")

            is_queried(TRUE)
            ticker_data(res)

            shinyjs::runjs("setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 100);")
          })
        }, delay = 0.1)
      },
      ignoreInit = TRUE
    )

    output$ticker_header_ui <- renderUI({
      req(is_queried(), ticker_data())
      d <- ticker_data()
      div(
        class = "mt-2 mb-1 text-muted",
        strong(d$ticker_label), " vs ", strong(d$bm_label),
        span(
          class = "ms-2 small",
          paste0(
            "(", format(min(d$cum_df$date), "%Y-%m-%d"),
            " ~ ", format(max(d$cum_df$date), "%Y-%m-%d"), ")"
          )
        )
      )
    })

    pct_fmt <- htmlwidgets::JS("function(v){ return (v !== null && v !== undefined && !isNaN(v)) ? Number(v).toFixed(1) + '%' : ''; }")

    output$stats_table <- renderReactable({
      req(ticker_data())
      df <- ticker_data()$stats_df
      req(nrow(df) > 0)
      num_cols <- setdiff(names(df), "지표")
      render_rt(
        df,
        dec_cols = num_cols,
        sticky_cols = "지표",
        sortable = FALSE,
        dynamic_height = FALSE
      )
    })

    output$cum_chart <- renderEcharts4r({
      req(ticker_data())
      d <- ticker_data()
      lbl_t <- d$ticker_label
      lbl_b <- d$bm_label
      if (lbl_t == lbl_b) lbl_b <- paste0(lbl_b, " (BM)")
      d$cum_df %>%
        rename(!!lbl_t := ticker, !!lbl_b := bm) %>%
        pivot_longer(-date, names_to = "구분", values_to = "value") %>%
        group_by(구분) %>%
        e_charts(date) %>%
        e_line(value, symbol = "none") %>%
        e_title(text = "누적수익률", left = "center", textStyle = list(fontSize = 13)) %>%
        e_tooltip(trigger = "axis") %>%
        e_datazoom(x_index = 0, type = "slider") %>%
        e_y_axis(position = "right", axisLabel = list(formatter = pct_fmt)) %>%
        e_grid(right = "15%", left = "3%", bottom = "50px") %>%
        e_legend(right = 0, top = "center", orient = "vertical")
    })

    output$dd_chart <- renderEcharts4r({
      req(ticker_data())
      ticker_data()$dd_df %>%
        e_charts(date) %>%
        e_area(dd, name = "DrawDown", symbol = "none", color = "#dc3545") %>%
        e_tooltip(trigger = "axis") %>%
        e_datazoom(x_index = 0, type = "slider") %>%
        e_y_axis(
          max = 0, position = "right",
          axisLabel = list(formatter = pct_fmt)
        ) %>%
        e_grid(right = "15%", left = "3%", bottom = "50px") %>%
        e_legend(show = FALSE)
    })

    output$heatmap_chart <- renderEcharts4r({
      req(ticker_data())
      df_heat <- ticker_data()$monthly_ret
      req(nrow(df_heat) > 0)

      years_list <- as.character(sort(unique(df_heat$year), decreasing = FALSE))
      months_list <- month.abb

      df_heat <- df_heat %>%
        mutate(
          m_idx = month - 1L,
          y_idx = match(as.character(year), years_list) - 1L,
          ret   = round(as.numeric(ret), 2)
        )

      min_ret <- floor(min(df_heat$ret, na.rm = TRUE))
      max_ret <- ceiling(max(df_heat$ret, na.rm = TRUE))
      lim <- max(abs(min_ret), abs(max_ret), 5)

      df_heat %>%
        e_charts(m_idx) %>%
        e_heatmap(y_idx, ret) %>%
        e_x_axis(type = "category", data = months_list) %>%
        e_y_axis(type = "category", data = years_list) %>%
        e_labels(
          show = TRUE,
          position = "inside",
          textStyle = list(fontSize = 11),
          formatter = htmlwidgets::JS("function(p){ return p.value[2] !== undefined && p.value[2] !== null ? Number(p.value[2]).toFixed(1) + '%' : ''; }")
        ) %>%
        e_visual_map(
          min        = -lim,
          max        =  lim,
          color      = c("#1a9850", "#ffffff", "#d73027"),
          calculable = TRUE,
          orient     = "horizontal",
          left       = "center",
          bottom     = 0
        ) %>%
        e_title(
          text = "월별 수익률 히트맵", left = "center",
          textStyle = list(fontSize = 13)
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS(paste0("
            function(p) {
              var years = [", paste(sprintf("'%s'", years_list), collapse = ", "), "];
              var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
              var v = p.value;
              var m = months[v[0]] !== undefined ? months[v[0]] : v[0];
              var y = years[v[1]] !== undefined ? years[v[1]] : v[1];
              return y + ' ' + m + '<br/>' +
                (v[2] !== undefined && v[2] !== null ? Number(v[2]).toFixed(1) + '%' : 'N/A');
            }
          "))
        ) %>%
        e_grid(bottom = "80px", top = "40px", left = "5%", right = "5%")
    })

    output$yearly_chart <- renderEcharts4r({
      req(ticker_data())
      d <- ticker_data()
      lbl_t <- d$ticker_label
      lbl_b <- d$bm_label
      if (lbl_t == lbl_b) lbl_b <- paste0(lbl_b, " (BM)")
      d$yearly_ret %>%
        rename(!!lbl_t := ticker, !!lbl_b := bm) %>%
        mutate(year = as.character(year)) %>%
        pivot_longer(-year, names_to = "구분", values_to = "수익률") %>%
        group_by(구분) %>%
        e_charts(year) %>%
        e_bar(수익률) %>%
        e_title(
          text = "연도별 수익률", left = "center",
          textStyle = list(fontSize = 13)
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_y_axis(position = "right", axisLabel = list(formatter = pct_fmt)) %>%
        e_grid(right = "15%", left = "3%") %>%
        e_legend(right = 0, top = "center", orient = "vertical")
    })

    output$rolling_vol_chart <- renderEcharts4r({
      req(ticker_data())
      df <- ticker_data()$rolling_vol
      req(nrow(df) > 0)
      df %>%
        e_charts(date) %>%
        e_line(rolling_vol, name = "롤링변동성(3년)", symbol = "none") %>%
        e_title(
          text = "롤링 변동성 (3년)", left = "center",
          textStyle = list(fontSize = 13)
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_datazoom(x_index = 0, type = "slider") %>%
        e_y_axis(position = "right", axisLabel = list(formatter = pct_fmt)) %>%
        e_grid(right = "15%", left = "3%", bottom = "50px") %>%
        e_legend(show = FALSE)
    })

    output$rolling_sharpe_chart <- renderEcharts4r({
      req(ticker_data())
      df <- ticker_data()$rolling_sharpe
      req(nrow(df) > 0)
      df %>%
        e_charts(date) %>%
        e_line(rolling_sharpe, name = "롤링샤프(3년)", symbol = "none") %>%
        e_title(
          text = "롤링 샤프지수 (3년)", left = "center",
          textStyle = list(fontSize = 13)
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_datazoom(x_index = 0, type = "slider") %>%
        e_y_axis(position = "right") %>%
        e_mark_line(
          data = list(yAxis = 0),
          lineStyle = list(color = "gray", type = "dashed")
        ) %>%
        e_grid(right = "15%", left = "3%", bottom = "50px") %>%
        e_legend(show = FALSE)
    })
  })
}
