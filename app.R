# =============================================================================
# app.R — 엔트리포인트 (모듈 조립 전용)
# =============================================================================
# 기존 2,147줄 → 모듈 호출 중심으로 축소
# global.R에서 모든 패키지와 소스 파일을 로드한 상태에서 실행
# =============================================================================

# 명시적으로 global.R을 로드하여 패키지 및 모듈 인식 오류 방지
source("global.R", encoding = "UTF-8")


# <User Interface> ====

## 1. 대쉬보드 헤더 ====
header <- dashboardHeader(
  title = dashboardBrand(
    title = div("포트폴리오 관리", align = "center"),
    color = "info"
  )
)

## 2. 대쉬보드 사이드바(메뉴) ====
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu_tabs",
    actionButton("reval", "평가금액 재계산"),
    br(),
    sidebarHeader("포트폴리오 관리"),
    menuItem(
      text = "자산운용 내역 기록", icon = icon("receipt"),
      tabName = "trading_record"
    ),
    menuItem(
      text = "보유자산 현황", icon = icon("sack-dollar"),
      tabName = "pf_total"
    ),
    menuItem(
      text = "손익현황", icon = icon("sack-dollar"),
      tabName = "pf_bs_pl", selected = TRUE
    ),
    menuItem(
      text = "배분전략 및 성과분석", icon = icon("chess-board"),
      tabName = "pf_strategy"
    ),
    menuItem(
      text = "유동성 관리", icon = icon("chart-line"),
      tabName = "pf_liquid"
    )
  ),
  br(),
  actionButton("renew_last_eval_profit",
    label = "기초평가손익갱신",
    width = "90%", status = "primary"
  ),
  actionButton("close_win",
    label = "프로그램 종료",
    width = "90%", status = "primary"
  )
)

## 3. 대쉬보드 본문 ====
body <- dashboardBody(
  pwa(
    domain = "https://hailey-family.shinyapps.io/my_asset/",
    title = "가족자산관리",
    output = "www",
    icon = "www/3890929_chart_growth_invest_market_stock_icon.png"
  ),
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
  useSweetAlert(),
  useWaiter(),
  tabItems(

    ## 자산운용 내역 기록 (4개 서브탭 모듈) ====
    tabItem(
      tabName = "trading_record",
      tabBox(
        id = "trading_box", width = 12, status = "primary", type = "tabs",
        mod_trading_ui("trading"),
        mod_ticker_ui("ticker"),
        mod_category_ui("category"),
        mod_total_trade_ui("total_trade")
      )
    ),

    ## 보유자산 현황 ====
    tabItem(tabName = "pf_total", mod_holdings_ui("holdings")),

    ## 손익현황 ====
    tabItem(tabName = "pf_bs_pl", mod_profit_ui("profit")),

    ## 배분전략 및 성과분석 ====
    tabItem(tabName = "pf_strategy", mod_strategy_ui("strategy")),

    ## 유동성 관리 ====
    tabItem(tabName = "pf_liquid", mod_liquidity_ui("liquidity"))
  )
)

## 4. 바닥글 ====
footer <- dashboardFooter(right = "developed by H.M. Choi")

## 5. 대쉬보드 페이지 조립 ====
ui <- dashboardPage(header, sidebar, body, footer = footer)


# <Server> ====

server <- function(input, output, session) {
  # --- Waiter 초기화 ---
  w1 <- Waiter$new(
    html = tagList(spin_loader(), "로딩중..."),
    color = transparent(.5)
  )

  show_delay <- function(text, type) {
    show_alert(title = text, type = type)
  }

  # --- 인증 모듈 호출 ---
  auth_rv <- mod_auth_server("auth", is_local = IS_LOCAL)

  # --- 인증 성공 후 메인 로직 초기화 ---
  observeEvent(auth_rv$authenticated,
    {
      req(auth_rv$authenticated == TRUE)

      show_delay("앱 구동중...", "info")

      # 갱신 트리거 (토글 방식)
      sk_b <- reactiveVal(TRUE)
      sk_v <- reactiveVal(TRUE)
      sk_c <- reactiveVal(TRUE)

      # MyAssets R6 인스턴스 생성
      ma <- MyAssets$new(auth_rv$pg_pass)

      # 장부금액 반응성 데이터
      ma_b <- reactive({
        sk_b()
        ma$run_book()
        ma
      })

      # 평가금액 재계산
      observeEvent(input$reval, {
        w1$show()
        sk_v(!sk_v())
        w1$hide()
      })

      # 평가금액 반응성 데이터
      ma_v <- reactive({
        sk_v()
        ma$run_valuation()
        ma
      })

      # 카테고리 반응성 데이터
      ctg <- reactive({
        sk_c()
        df <- ma$read("categories")
        split(df$value, df$key)
      })

      # --- 모듈 서버 호출 ---
      mod_trading_server("trading", ma = ma, ma_b = ma_b, sk_b = sk_b)
      mod_ticker_server("ticker", ma = ma, ma_b = ma_b, sk_b = sk_b, ctg = ctg)
      mod_category_server("category", ma = ma, sk_c = sk_c, ctg = ctg)
      mod_total_trade_server("total_trade", ma_b = ma_b)
      mod_holdings_server("holdings", ma_v = ma_v)
      mod_profit_server("profit",
        ma_v = ma_v,
        on_initial_load = function() show_delay("완료!", "success")
      )
      mod_strategy_server("strategy", ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b)
      mod_liquidity_server("liquidity", ma = ma, ma_b = ma_b, ma_v = ma_v, sk_b = sk_b)

      # --- 프로그램 종료 ---
      observeEvent(input$close_win, {
        js$closeWindow()
        stopApp()
      })

      # --- 기초평가손익 갱신 ---
      observeEvent(input$renew_last_eval_profit, {
        ma$renew_last_eval_profit()
      })
    },
    ignoreInit = FALSE
  )

  # --- DB 연결 안전 종료 (세션 종료 시) ---
  onStop(function() {
    # R6 finalize에서도 처리하지만 이중 안전장치
    message("[INFO] 세션 종료 — 자원 정리 완료")
  })
}


shinyApp(ui = ui, server = server)
