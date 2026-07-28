# =============================================================================
# mod_trade_history.R — 거래내역 기록 모듈
# =============================================================================
# DB CRUD는 pool 객체 직접 주입받아 사용
# =============================================================================

# 1. 거래내역 기록 탭 UI ----
mod_trade_history_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "거래내역",
    card(
      id = ns("trading_input_box"),
      class = "mb-3 border-info",
      card_header("입력사항", class = "bg-info text-white py-2"),
      card_body(
        fluidRow(
          column(
            width = 1, class = "col-12 col-md-2 col-lg-1",
            selectInput(ns("type2"),
              label = "운용구분",
              choices = c("투자자산", "연금자산")
            ),
            selectInput(ns("ass_account2"), label = "계좌", choices = NULL)
          ),
          column(
            width = 1, class = "col-12 col-md-2 col-lg-1",
            selectInput(ns("ass_cur2"), label = "통화", choices = NULL),
            selectInput(ns("new2"), label = "신규/수정", choices = "신규")
          ),
          column(
            width = 1, class = "col-12 col-md-2 col-lg-1",
            airDatepickerInput(ns("trading_date"),
              label = "거래일자",
              addon = "none", value = Sys.Date()
            ),
            selectInput(ns("ass_name2"), label = "종목명", choices = NULL)
          ),
          column(
            width = 1, class = "col-12 col-md-2 col-lg-1",
            numericInput(ns("buy_q"), label = "매입수량", value = 0),
            numericInput(ns("sell_q"), label = "매도수량", value = 0)
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            autonumericInput(ns("buy_p"), label = "매입액", value = 0),
            autonumericInput(ns("sell_b"), label = "매도원금", value = 0)
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            autonumericInput(ns("buy_c"), label = "현금지출", value = 0),
            autonumericInput(ns("sell_p"), label = "매도액", value = 0)
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            autonumericInput(ns("int_dev"), label = "이자배당액", value = 0),
            autonumericInput(ns("sell_c"), label = "현금수입", value = 0)
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            autonumericInput(ns("in_out_c"), label = "입출금", value = 0),
            searchInput(ns("trade_limit"),
              label = "조회건수",
              placeholder = "20", value = 20,
              btnSearch = icon("search"), btnReset = NULL, width = "100%"
            )
          )
        ),
        div(
          actionButton(ns("ass_trade_new"), label = "추가", class = "btn-info", style = "width: 30%;"),
          actionButton(ns("ass_trade_mod"), label = "수정", class = "btn-success", style = "width: 30%;"),
          actionButton(ns("ass_trade_del"), label = "삭제", class = "btn-primary", style = "width: 30%;"),
          style = "text-align: center"
        )
      )
    ),
    card(
      card_body(
        reactableOutput(ns("trade_table"))
      )
    )
  )
}

# 2. 거래내역 기록 탭 서버 ----
mod_trade_history_server <- function(id, pool, ma, ma_b, sk_b, menu_tabs) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      type2 = NULL,
      trade = NULL,
      trade_new = NULL
    )

    # --- 2.1 통화별 컬럼 포맷 결정 헬퍼 ----
    build_trade_col_defs <- function(cur) {
      qty_cols <- c("매입수량", "매도수량")
      money_cols <- c(
        "매입액", "매입비용", "매도원금", "현금지출", "매도액", "매매수익",
        "매도비용", "순수익", "잔액", "이자배당액", "현금수입", "입출금"
      )

      if (cur %in% c("달러", "엔화")) {
        dec_cols_use <- money_cols
        int_cols_use <- qty_cols
      } else {
        dec_cols_use <- character(0)
        int_cols_use <- c(qty_cols, money_cols)
      }
      list(int_cols = int_cols_use, dec_cols = dec_cols_use)
    }

    # --- 2.2 테이블 렌더링 ----
    output$trade_table <- renderReactable({
      req(menu_tabs() == "trading_record")
      if (!is.null(rv$trade) && nrow(rv$trade) > 0) {
        cur <- isolate(input$ass_cur2)
        fmt <- build_trade_col_defs(cur)
        render_rt(
          rv$trade,
          int_cols      = fmt$int_cols,
          dec_cols      = fmt$dec_cols,
          sticky_cols   = names(rv$trade)[1:5],
          long_str_cols = intersect(c("종목명", "종목코드"), names(rv$trade))
        )
      }
    })

    # --- 2.3 거래내역 리셋 및 입력필드 초기화 ----
    reset_trade <- reactive({
      input$ass_trade_new
      input$ass_trade_mod
      input$ass_trade_del

      ma_b()$get_trading_record(
        table   = input$type2,
        acct    = input$ass_account2,
        cur     = input$ass_cur2,
        limit_n = as.numeric(input$trade_limit)
      )
    })

    reset_trade_inputs <- function() {
      updateAirDateInput(session, "trading_date", value = Sys.Date())
      updateNumericInput(session, "buy_q", value = 0)
      updateNumericInput(session, "sell_q", value = 0)
      updateAutonumericInput(session, "buy_p", value = 0)
      updateAutonumericInput(session, "sell_b", value = 0)
      updateAutonumericInput(session, "buy_c", value = 0)
      updateAutonumericInput(session, "sell_p", value = 0)
      updateAutonumericInput(session, "int_dev", value = 0)
      updateAutonumericInput(session, "sell_c", value = 0)
      updateAutonumericInput(session, "in_out_c", value = 0)
    }

    update_new_trade <- function() {
      updateSelectInput(session, "new2",
        choices = c("신규", rv$trade$행번호),
        selected = "신규"
      )

      updateSelectInput(
        session, "ass_name2",
        choices = (ma_b()[[rv$type2]] |>
          filter(
            계좌 == input$ass_account2,
            통화 == input$ass_cur2
          ))$종목명
      )
    }

    # --- 2.4 드롭다운 연동 이벤트 ----
    observeEvent(input$type2, {
      rv$type2 <- if (input$type2 == "투자자산") "assets" else "pension"
      updateSelectInput(session, "ass_account2",
        choices = unique(ma_b()[[rv$type2]]$계좌)
      )
      rv$trade <- reset_trade()
      update_new_trade()
    })

    observeEvent(input$ass_account2, {
      updateSelectInput(
        session, "ass_cur2",
        choices = unique((ma_b()[[rv$type2]] |>
          filter(계좌 == input$ass_account2))$통화)
      )
      rv$trade <- reset_trade()
      update_new_trade()
    })

    observeEvent(input$ass_cur2, {
      rv$trade <- reset_trade()
      update_new_trade()
    })

    observeEvent(input$trade_limit, {
      rv$trade <- reset_trade()
      update_new_trade()
    })

    observeEvent(input$new2, {
      if (!is.null(input$new2) && input$new2 != "신규") {
        t_rows2 <- filter(rv$trade, 행번호 == input$new2)
        if (nrow(t_rows2) > 0) {
          updateAirDateInput(session, "trading_date", value = t_rows2$거래일자)
          updateSelectInput(session, "ass_name2", selected = t_rows2$종목명)
          updateNumericInput(session, "buy_q", value = t_rows2$매입수량)
          updateNumericInput(session, "sell_q", value = t_rows2$매도수량)
          updateAutonumericInput(session, "buy_p", value = t_rows2$매입액)
          updateAutonumericInput(session, "sell_b", value = t_rows2$매도원금)
          updateAutonumericInput(session, "buy_c", value = t_rows2$현금지출)
          updateAutonumericInput(session, "sell_p", value = t_rows2$매도액)
          updateAutonumericInput(session, "int_dev", value = t_rows2$이자배당액)
          updateAutonumericInput(session, "sell_c", value = t_rows2$현금수입)
          updateAutonumericInput(session, "in_out_c", value = t_rows2$입출금)
        }
      } else {
        reset_trade_inputs()
      }
    })

    # --- 2.5 거래 레코드 조립 ----
    observe({
      if (!is.null(input$ass_name2)) {
        trade_ticker <-
          bind_rows(ma_b()[["assets"]], ma_b()[["pension"]]) |>
          filter(
            계좌 == input$ass_account2,
            통화 == input$ass_cur2,
            종목명 == input$ass_name2
          ) |>
          pull(종목코드)

        if (length(trade_ticker) > 0) {
          rv$trade_new <- tibble::tibble_row(
            행번호 = 0, 거래일자 = input$trading_date,
            계좌 = input$ass_account2,
            종목코드 = trade_ticker,
            매입수량 = input$buy_q, 매입액 = input$buy_p,
            현금지출 = input$buy_c, 매도수량 = input$sell_q,
            매도원금 = input$sell_b, 매도액 = input$sell_p,
            이자배당액 = input$int_dev, 현금수입 = input$sell_c,
            입출금 = input$in_out_c
          )
        }
      }
    })

    # --- 2.6 CRUD 액션 핸들러 ----
    observeEvent(input$ass_trade_new, {
      req(rv$trade_new)
      if (input$type2 == "투자자산") {
        rv$trade_new$행번호 <- ma$assets_daily_last_num + 1
        dbxInsert(pool, "assets_daily", rv$trade_new)
      } else {
        rv$trade_new$행번호 <- ma$pension_daily_last_num + 1
        dbxInsert(pool, "pension_daily", rv$trade_new)
      }
      sk_b(!sk_b())
      rv$trade <- reset_trade()
      update_new_trade()
      reset_trade_inputs()
    })

    observeEvent(input$ass_trade_mod, {
      req(rv$trade_new)
      rv$trade_new$행번호 <- input$new2
      if (input$type2 == "투자자산") {
        dbxUpdate(pool, "assets_daily", rv$trade_new, where_cols = c("행번호"))
      } else {
        dbxUpdate(pool, "pension_daily", rv$trade_new, where_cols = c("행번호"))
      }
      sk_b(!sk_b())
      rv$trade <- reset_trade()
      update_new_trade()
      reset_trade_inputs()
    })

    observeEvent(input$ass_trade_del, {
      req(rv$trade_new)
      rv$trade_new$행번호 <- input$new2
      if (input$type2 == "투자자산") {
        dbxDelete(pool, "assets_daily", rv$trade_new)
      } else {
        dbxDelete(pool, "pension_daily", rv$trade_new)
      }
      sk_b(!sk_b())
      rv$trade <- reset_trade()
      update_new_trade()
      reset_trade_inputs()
    })
  })
}
