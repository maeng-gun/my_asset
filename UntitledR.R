library(dplyr)
library(lubridate)
library(tidyr)
# library(httr)
# library(jsonlite)
library(rvest)
library(R6)
library(readxl)

get_exchange_rate <- function(cur='달러'){
  
  num <- c('달러'= 1, '엔'= 2, '유로'=3, '위안'=4)
  
  (read_html("http://finance.naver.com/marketindex/") |> 
      html_nodes("div.head_info > span.value")
  )[num[cur]] |>
    html_text() |> 
    readr::parse_number()
}

MyAssets <- R6Class(
  classname = "MyAssets",
  
  public = list(
    today = NULL, year = NULL,
    assets = NULL, ex_usd = NULL,
    ex_jpy = NULL, daily_trading = NULL,
    bs_pl_book = NULL, bs_pl_mkt = NULL,
    bl = NULL, my = NULL, allo0 = NULL,
    allo1 = NULL, allo2 = NULL,
    allo3 = NULL, allo4 = NULL,
    allo5 = NULL, plot_pie = NULL,
    plot_pie2 = NULL, plot_pie3 = NULL,
  
    #속성 초기화
    initialize = function(base_dt=NULL) {
      if (!is.null(base_dt)) {
        self$today <- ymd(base_dt)
      } else {
        self$today <- today()
      }
      self$year <- year(self$today)
      self$assets <- read_excel('trade.xlsx', sheet = '자산정보')
      self$ex_usd <- get_exchange_rate('달러')
      self$ex_jpy <- get_exchange_rate('엔')/100
      
      self$daily_trading <- self$get_daily_trading()
      self$bs_pl_book <- self$get_bs_pl()
    },
    
    #(메서드) 일일거래내역 산출====
    get_daily_trading = function(){
      days <- seq(make_date(self$year,1,1), make_date(self$year,12,31),by='day')
    
      usd1 <- read_excel('trade.xlsx', sheet = '불리오달러')
      usd2 <- read_excel('trade.xlsx', sheet = '한투달러')
      jpy <- read_excel('trade.xlsx', sheet = '한투엔화')
      # krw1 <- read_excel('trade.xlsx', sheet = '나무원화')
      krw2 <- read_excel('trade.xlsx', sheet = '한투원화')
      # krw3 <- read_excel('trade.xlsx', sheet = '한투CMA')
      krw4 <- read_excel('trade.xlsx', sheet = '한투ISA')
      krw5 <- read_excel('trade.xlsx', sheet = '별도원화')
      fiw <- read_excel('trade.xlsx', sheet = '외화자산평가') %>% select(-c('외화입출금'))
      
      as_list <- expand_grid(종목코드 = self$assets$종목코드, 거래일자 = days)
      
      trade_raw <- bind_rows(usd1, usd2, jpy, krw2, krw4, krw5, fiw) %>%
        mutate(across(매입수량:누적,as.numeric)) |> 
        select(-종목명, -상품명)
      
      as_list %>%
        left_join(select(self$assets, 종목코드, 종목명, 통화, 계좌), by = "종목코드") %>%
        left_join(trade_raw, by = c("종목코드", "거래일자")) %>%
        mutate(across(매입수량:누적, ~coalesce(.,0)))%>%
        arrange(종목코드, 거래일자) |> 
        mutate(
          순매입수량 = 매입수량 - 매도수량,
          수익 = 매매수익 + 이자배당액,
          비용 = 매입비용 + 매도비용,
          실현손익 = 수익 - 비용
        ) %>%
        select(종목코드:계좌, 순매입수량, 매입액, 매도원금, 수익, 비용, 실현손익, 현금수입, 입출금, 현금지출)
    },
    
    #(메서드)운용자산 잔액-손익 테이블 생성====
    get_bs_pl = function() {
      
      trade <- self$daily_trading
      
      #(1) 기본 테이블 생성
      
      bs_pl1 <- trade %>%
        select(종목코드, 거래일자, 종목명, 통화, 계좌)
      
      bs_pl2 <- trade |> 
        group_by(종목코드) %>%
        transmute(
          보유수량 = cumsum(순매입수량),
          장부금액 = cumsum(매입액 - 매도원금),
          평잔 = cummean(장부금액)
        ) %>%
        ungroup() |> 
        select(-종목코드)
      
      bs_pl3 <- trade %>%
        group_by(종목코드) %>%
        transmute(
          수익 = cumsum(수익),
          비용 = cumsum(비용),
          실현손익 = cumsum(실현손익)
        )%>%
        ungroup() |> 
        select(-종목코드)
      
      bs_pl <- bind_cols(bs_pl1, bs_pl2, bs_pl3)  
      
      #(2) 계좌별 현금성자산 산출
      
      cash_w <- trade %>%
        filter(통화 == '원화') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출),
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) |> 
        ungroup()
      
      cash_w_b <- cash_w %>%
        mutate(across(-거래일자, cumsum))
      
      cash_w_e <- cash_w_b %>%
        mutate(across(-거래일자, cummean))
      
      
      cash_d <- trade %>%
        filter(통화 == '달러') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) |> 
        ungroup()
      
      cash_d_b <- cash_d %>%
        mutate(across(-거래일자, cumsum))
      
      cash_d_e <- cash_d_b %>%
        mutate(across(-거래일자, cummean))
      
      cash_y <- trade %>%
        filter(통화 == '엔화') %>%
        group_by(거래일자, 계좌) %>%
        summarise(현금 = sum(현금수입 + 입출금 - 현금지출), 
                  .groups = 'keep') %>%
        pivot_wider(names_from = 계좌, values_from = 현금) %>%
        ungroup()
      
      cash_y_b <- cash_y %>%
        mutate(across(-거래일자, cumsum))
      
      cash_y_e <- cash_y_b %>%
        mutate(across(-거래일자, cummean))
      
      df <- bs_pl %>%
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='나무예수금', cash_w_b$나무)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='한투예수금', cash_w_b$한투)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='한투CMA예수금', cash_w_b$한투CMA)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='한투ISA예수금', cash_w_b$한투ISA)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='나무예수금', cash_w_e$나무)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='한투예수금', cash_w_e$한투)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='한투CMA예수금', cash_w_e$한투CMA)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='한투ISA예수금', cash_w_e$한투ISA)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='불리오달러', cash_d_b$불리오)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='직접운용달러', cash_d_b$한투)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='불리오달러', cash_d_e$불리오)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='직접운용달러', cash_d_e$한투)) |> 
        mutate(장부금액 = 
                 replace(장부금액, 종목명=='직접운용엔', cash_y_b$한투)) |> 
        mutate(평잔 = 
                 replace(평잔, 종목명=='직접운용엔', cash_y_e$한투)) |> 
        mutate(실현수익률 = 실현손익 / 평잔 * 100) |> 
        left_join(select(self$assets, 종목명, 자산군, 세부자산군, 세부자산군2), by = '종목명') %>%
        arrange(종목명, 거래일자)
    }
  )
)
