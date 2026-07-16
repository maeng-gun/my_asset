# =============================================================================
# mod_trade_total — 종합거래내역 모듈
# =============================================================================
# (구 mod_total_trade.R에서 이름 변경)
# 순수 함수 calc_total_trading() 사용
# =============================================================================

mod_trade_total_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "종합거래내역",
    card(
      id = ns("total_trade_box"),
      class = "mb-3 border-info",
      card_header("입력사항", class = "bg-info text-white py-2"),
      card_body(
        fluidRow(
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            airDatepickerInput(ns("total_trade_date"),
              label = "거래일자",
              range = TRUE, addon = "none",
              value = c(Sys.Date(), Sys.Date())
            )
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            selectInput(ns("total_ass1"),
              label = "자산군",
              choices = c("전체", "주식", "대체자산", "채권", "현금성")
            )
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            selectInput(ns("total_ass2"),
              label = "세부자산군",
              choices = c(
                "전체", "선진국", "국내", "신흥국", "실물자산",
                "인컴자산", "상품", "부동산인프라",
                "만기보유", "시장형",
                "국채", "투자등급", "하이일드", "만기무위험",
                "만기회사채", "금융상품", "현금"
              )
            )
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            selectInput(ns("total_ass3"),
              label = "세부자산군2",
              choices = c(
                "전체", "인덱스", "종목", "테마", "귀금속", "원자재",
                "에너지", "국내", "해외", "안전자산", "크레딧",
                "부동산", "인프라", "선진국", "신흥국", "단기ETF",
                "원화상품", "외화상품", "외환", "원화"
              )
            )
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            selectInput(ns("total_curr"),
              label = "통화",
              choices = c("전체", "원화", "달러", "엔화")
            )
          ),
          column(
            width = 2, class = "col-12 col-md-4 col-lg-2",
            textInput(ns("total_comm_name"), label = "상품명", value = "")
          )
        )
      )
    ),
    card(
      card_body(
        reactableOutput(ns("total_trade_table"))
      )
    )
  )
}

mod_trade_total_server <- function(id, pool, ma_b) {
  moduleServer(id, function(input, output, session) {
    output$total_trade_table <- renderReactable({
      input$total_trade_date
      input$total_ass1
      input$total_ass2
      input$total_ass3
      input$total_curr

      if (!is.null(input$total_trade_date)) {
        ma_obj <- ma_b()

        # calc_total_trading 순수 함수 호출
        df <- calc_total_trading(
          assets_df         = ma_obj$assets,
          pension_df        = ma_obj$pension,
          assets_daily_tbl  = ma_obj$read_obj("assets_daily"),
          pension_daily_tbl = ma_obj$read_obj("pension_daily"),
          dates             = input$total_trade_date
        )

        if (input$total_ass1 != "전체") {
          df <- df %>% filter(자산군 == input$total_ass1)
        }
        if (input$total_ass2 != "전체") {
          df <- df %>% filter(세부자산군 == input$total_ass2)
        }
        if (input$total_ass3 != "전체") {
          df <- df %>% filter(세부자산군2 == input$total_ass3)
        }
        if (input$total_curr != "전체") {
          df <- df %>% filter(통화 == input$total_curr)
        }
        if (input$total_comm_name != "") {
          df <- df %>% filter(상품명 == input$total_comm_name)
        }

        render_rt(df |> arrange(pick(1:5)),
          int_cols = 8:13,
          sticky_cols = names(df)[1:7]
        )
      }
    })
  })
}
