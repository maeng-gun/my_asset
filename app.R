# =============================================================================
# app.R — 엔트리포인트 (모듈 조립 전용)
# =============================================================================
# pool 기반 DB 커넥션 관리, R6 의존성 주입, 모듈 라우팅
# global.R에서 모든 패키지와 소스 파일을 로드한 상태에서 실행
# =============================================================================

# 명시적으로 global.R을 로드하여 패키지 및 모듈 인식 오류 방지
library(shiny)
source("global.R", encoding = "UTF-8")
options(shiny.autoreload.legacy_warning = FALSE)


# 1. User Interface ----

ui <- page_navbar(
  id = "menu_tabs",
  selected = "pf_bs_pl",
  title = "포트폴리오 관리",
  window_title = "가족자산관리",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  header = tagList(
    pwa(
      domain = "https://hailey-family.shinyapps.io/my_asset/",
      title = "가족자산관리",
      output = "www",
      icon = "www/3890929_chart_growth_invest_market_stock_icon.png"
    ),
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    useShinyjs(),
    extendShinyjs(
      text = "
        shinyjs.closeWindow = function() { window.close(); }
        shinyjs.enterToClick = function(params) {
          var inputId = params.inputId;
          var buttonId = params.buttonId;
          $(document).on('keydown', '#' + inputId, function (e) {
            if (e.keyCode == 13) {
              e.preventDefault();
              $('#' + buttonId).click();
            }
          });
        }
      ",
      functions = c("closeWindow", "enterToClick")
    ),
    tags$script(HTML("
      window.fallbackCopyTextToClipboard = function(text) {
        var textArea = document.createElement('textarea');
        textArea.value = text;
        textArea.style.top = '0';
        textArea.style.left = '0';
        textArea.style.position = 'fixed';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
          var successful = document.execCommand('copy');
          if(successful) {
            if (typeof Swal !== 'undefined' && typeof Swal.fire === 'function') {
              Swal.fire('성공', '표 내용이 클립보드에 복사되었습니다.\\n엑셀에 붙여넣기 하세요.', 'success');
            } else {
              alert('표 내용이 클립보드에 복사되었습니다.\\n엑셀에 붙여넣기 하세요.');
            }
          } else {
            alert('클립보드 복사에 실패했습니다.');
          }
        } catch (err) {
          console.error('Fallback: Oops, unable to copy', err);
          alert('클립보드 복사 중 오류가 발생했습니다.');
        }
        document.body.removeChild(textArea);
      };
    ")),
    useSweetAlert(),
    useWaiter()
  ),

  # --- 1.1 운용기록 탭 ----
  nav_panel(
    title = "운용기록",
    value = "trading_record",
    icon = icon("receipt"),
    navset_card_tab(
      id = "trading_box",
      mod_trade_history_ui("trading"),
      mod_trade_ticker_ui("ticker"),
      mod_trade_category_ui("category"),
      mod_trade_total_ui("total_trade")
    )
  ),

  # --- 1.2 보유현황 탭 ----
  nav_panel(
    title = "보유현황", value = "pf_total", icon = icon("sack-dollar"),
    mod_holdings_ui("holdings")
  ),

  # --- 1.3 손익현황 탭 ----
  nav_panel(
    title = "손익현황", value = "pf_bs_pl", icon = icon("sack-dollar"),
    mod_profit_ui("profit")
  ),

  # --- 1.4 투자전략 탭 ----
  nav_panel(
    title = "투자전략", value = "pf_inv_strategy", icon = icon("lightbulb"),
    mod_inv_strategy_ui("inv_strategy")
  ),

  # --- 1.5 배분전략 탭 ----
  nav_panel(
    title = "배분전략", value = "pf_allo_strategy", icon = icon("chess-board"),
    mod_allo_strategy_ui("allo_strategy")
  ),

  # --- 1.6 유동성 관리 탭 ----
  nav_panel(
    title = "유동성 관리", value = "pf_liquid", icon = icon("chart-line"),
    mod_liquidity_ui("liquidity")
  ),

  # --- Navbar 우측 메뉴 ----
  nav_spacer(),
  nav_item(actionButton("reval", "평가금액 재계산", class = "btn-info btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(actionButton("renew_last_eval_profit", "기초평가손익갱신", class = "btn-primary btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(actionButton("close_win", "프로그램 종료", class = "btn-primary btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(tags$div("developed by H.M. Choi", style = "font-size: 0.8em; color: gray; margin-top: 15px; margin-right: 15px; margin-left: 10px;"))
)


# 2. Server ----

server <- function(input, output, session) {
  # --- 2.1 Waiter & 알림 초기화 ----
  w1 <- Waiter$new(
    html = tagList(spin_loader(), "로딩중..."),
    color = transparent(.5)
  )

  show_delay <- function(text, type) {
    show_alert(title = text, type = type)
  }

  # --- 2.2 인증 모듈 ----
  auth_rv <- mod_auth_server("auth", is_local = is_local)

  # --- 2.3 인증 성공 후 메인 데이터 및 모듈 초기화 ----
  observeEvent(auth_rv$authenticated,
    {
      req(auth_rv$authenticated == TRUE)

      show_delay("앱 구동중...", "info")

      cfg <- yaml::read_yaml(file = "ccc.yaml", readLines.warn = FALSE)
      db_pool <- dbPool(
        drv = RPostgres::Postgres(),
        host = cfg$c,
        port = 5432,
        dbname = "postgres",
        user = cfg$a,
        password = auth_rv$pg_pass
      )

      sk_b <- reactiveVal(TRUE)
      sk_v <- reactiveVal(TRUE)
      sk_c <- reactiveVal(TRUE)

      ma <- MyAssets$new(pool = db_pool)

      ma_b <- reactive({
        ma$run_book()
        ma
      }) %>% bindEvent(sk_b())

      observeEvent(input$reval, {
        w1$show()
        sk_v(!sk_v())
        w1$hide()
      })

      ma_v <- reactive({
        ma$run_valuation()
        ma
      }) %>% bindEvent(sk_v())

      ctg <- reactive({
        sk_c()
        df <- ma$read("categories")
        split(df$value, df$key)
      })

      # --- 2.4 모듈 서버 호출 (화면 메뉴 탭 순서와 1:1 일치) ----
      # (1) 운용기록 하위 모듈들
      mod_trade_history_server("trading",
        pool = db_pool, ma = ma, ma_b = ma_b, sk_b = sk_b,
        menu_tabs = reactive(input$menu_tabs)
      )
      mod_trade_ticker_server("ticker",
        pool = db_pool, ma = ma, ma_b = ma_b, sk_b = sk_b, ctg = ctg
      )
      mod_trade_category_server("category",
        pool = db_pool, ma = ma, sk_c = sk_c, ctg = ctg
      )
      mod_trade_total_server("total_trade",
        pool = db_pool, ma_b = ma_b
      )

      # (2) 보유현황 모듈
      mod_holdings_server("holdings",
        ma_v = ma_v,
        menu_tabs = reactive(input$menu_tabs)
      )

      # (3) 손익현황 모듈
      mod_profit_server("profit",
        ma_v = ma_v,
        menu_tabs = reactive(input$menu_tabs),
        on_initial_load = function() show_delay("완료!", "success")
      )

      # (4) 투자전략 모듈
      mod_inv_strategy_server("inv_strategy",
        ma_v = ma_v,
        pool = db_pool
      )

      # (5) 배분전략 모듈
      mod_allo_strategy_server("allo_strategy",
        pool = db_pool, ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b,
        menu_tabs = reactive(input$menu_tabs)
      )

      # (6) 유동성 관리 모듈
      mod_liquidity_server("liquidity",
        pool = db_pool, ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b,
        menu_tabs = reactive(input$menu_tabs)
      )

      # --- 2.5 상단 헤더 액션 핸들러 ----
      observeEvent(input$close_win, {
        js$closeWindow()
        stopApp()
      })

      observeEvent(input$renew_last_eval_profit, {
        ma$renew_last_eval_profit()
      })

      # --- 2.6 세션 종료 시 pool 해제 ----
      session$onSessionEnded(function() {
        tryCatch(
          {
            if (pool::dbIsValid(db_pool)) {
              pool::poolClose(db_pool)
              message("[INFO] 세션 종료 — pool 연결 해제 완료")
            }
          },
          error = function(e) {
            message("[WARN] pool 종료 중 오류: ", e$message)
          }
        )
      })
    },
    ignoreInit = FALSE
  )

  onStop(function() {
    message("[INFO] 앱 종료 — 자원 정리 완료")
  })
}


shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
