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


# <User Interface> ====

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
  nav_panel(
    title = "자산운용 내역 기록",
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
  nav_panel(
    title = "보유자산 현황", value = "pf_total", icon = icon("sack-dollar"),
    mod_holdings_ui("holdings")
  ),
  nav_panel(
    title = "손익현황", value = "pf_bs_pl", icon = icon("sack-dollar"),
    mod_profit_ui("profit")
  ),
  nav_panel(
    title = "배분전략 및 성과분석", value = "pf_strategy", icon = icon("chess-board"),
    mod_strategy_ui("strategy")
  ),
  nav_panel(
    title = "유동성 관리", value = "pf_liquid", icon = icon("chart-line"),
    mod_liquidity_ui("liquidity")
  ),
  nav_spacer(),
  nav_item(actionButton("reval", "평가금액 재계산", class = "btn-info btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(actionButton("renew_last_eval_profit", "기초평가손익갱신", class = "btn-primary btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(actionButton("close_win", "프로그램 종료", class = "btn-primary btn-sm", style = "margin-top: 8px; margin-right: 5px;")),
  nav_item(tags$div("developed by H.M. Choi", style = "font-size: 0.8em; color: gray; margin-top: 15px; margin-right: 15px; margin-left: 10px;"))
)


# <Server> ====

server <- function(input, output, session) {
  # --- Waiter 초기화 ----
  w1 <- Waiter$new(
    html = tagList(spin_loader(), "로딩중..."),
    color = transparent(.5)
  )

  show_delay <- function(text, type) {
    show_alert(title = text, type = type)
  }

  # --- 인증 모듈 호출 ----
  auth_rv <- mod_auth_server("auth", is_local = IS_LOCAL)

  # --- 인증 성공 후 메인 로직 초기화 ----
  observeEvent(auth_rv$authenticated,
    {
      req(auth_rv$authenticated == TRUE)

      show_delay("앱 구동중...", "info")

      # --- DB Pool 생성 (외부 관리) ----
      cfg <- yaml::read_yaml(file = "ccc.yaml", readLines.warn = FALSE)
      db_pool <- dbPool(
        drv = RPostgres::Postgres(),
        host = cfg$c,
        port = 5432,
        dbname = "postgres",
        user = cfg$a,
        password = auth_rv$pg_pass
      )

      # 갱신 트리거 (토글 방식)
      sk_b <- reactiveVal(TRUE)
      sk_v <- reactiveVal(TRUE)
      sk_c <- reactiveVal(TRUE)

      # MyAssets R6 인스턴스 생성 (pool 주입)
      ma <- MyAssets$new(pool = db_pool)

      # 장부금액 반응성 데이터
      ma_b <- reactive({
        ma$run_book()
        ma
      }) %>% bindEvent(sk_b())

      # 평가금액 재계산
      observeEvent(input$reval, {
        w1$show()
        sk_v(!sk_v())
        w1$hide()
      })

      # 평가금액 반응성 데이터
      ma_v <- reactive({
        ma$run_valuation()
        ma
      }) %>% bindEvent(sk_v())

      # 카테고리 반응성 데이터
      ctg <- reactive({
        sk_c()
        df <- ma$read("categories")
        split(df$value, df$key)
      })

      # --- 모듈 서버 호출 (pool 주입) ----
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
      mod_holdings_server("holdings",
        ma_v = ma_v,
        menu_tabs = reactive(input$menu_tabs)
      )
      mod_profit_server("profit",
        ma_v = ma_v,
        menu_tabs = reactive(input$menu_tabs),
        on_initial_load = function() show_delay("완료!", "success")
      )
      mod_strategy_server("strategy",
        pool = db_pool, ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b,
        menu_tabs = reactive(input$menu_tabs)
      )
      mod_liquidity_server("liquidity",
        pool = db_pool, ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b,
        menu_tabs = reactive(input$menu_tabs)
      )

      # --- 프로그램 종료 ----
      observeEvent(input$close_win, {
        js$closeWindow()
        stopApp()
      })

      # --- 기초평가손익 갱신 ----
      observeEvent(input$renew_last_eval_profit, {
        ma$renew_last_eval_profit()
      })

      # --- 세션 종료 시 pool 안전 종료 ----
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

  # --- 앱 전체 종료 시 안전장치 ----
  onStop(function() {
    message("[INFO] 앱 종료 — 자원 정리 완료")
  })
}


shinyApp(ui = ui, server = server)
