library(R6)
library(lubridate)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(scales)
library(glue)
library(tidyverse)

# 1. Utils List: 포트폴리오 조작====
qs_utils <- list(
  
  add_portfolio = function(weights, name = "Portfolio", rebalance_on = "months") {
    # 1. 가중치 합 검증
    if (abs(sum(weights) - 1) > 0.001) stop("Error: 가중치의 합은 1이어야 합니다.")
    
    # 2. 대상 자산 필터링 (수정된 부분: 순수하게 초기 입력한 tickers만 대상으로 함)
    target_assets <- self$returns %>% 
      filter(symbol %in% self$tickers)
    
    # 3. 개수 일치 여부 검증 (에러 방지용 방어 로직 추가)
    unique_assets <- unique(target_assets$symbol)
    if (length(weights) != length(unique_assets)) {
      stop(glue("Error: 비중 개수({length(weights)}개)와 대상 자산 개수({length(unique_assets)}개)가 다릅니다.\n대상 자산: {paste(unique_assets, collapse=', ')}"))
    }
    
    # 4. 포트폴리오 수익률 계산
    port_ret <- suppressWarnings(
      target_assets %>%
      tq_portfolio(assets_col = symbol, 
                   returns_col = returns, 
                   weights = weights, 
                   col_rename = "returns",
                   rebalance_on = rebalance_on)
      ) %>% mutate(symbol = name)
    
    # 5. 기존 데이터베이스(self$returns)에 병합 (중복 이름 방지)
    # 만약 같은 이름의 포트폴리오가 이미 있다면 덮어쓰기(삭제 후 추가)
    if (name %in% unique(self$returns$symbol)) {
      self$returns <- self$returns %>% filter(symbol != name)
      message(glue("  [Utils] 기존 '{name}' 포트폴리오를 덮어씁니다."))
    }
    
    self$returns <- bind_rows(self$returns, port_ret)
    message(glue("  [Utils] '{name}' 포트폴리오 추가 완료"))
  }
)


# 2. Stats List: 핵심 성과 지표 ====

qs_stats <- list(
  
  calc_stats_table = function() {
    
    # 1. [Risk/Return] 기본 성과 (계산 직후 transmute로 최종 이름/단위 확정)
    perf_basic <- self$returns %>%
      group_by(symbol) %>%
      tq_performance(Ra = returns, 
                     performance_fun = table.AnnualizedReturns, 
                     scale = 252, 
                     Rf = 0) %>%
      rename(
        `Ann.Ret(%)` = 2,
        Sharpe       = 3,
        `Ann.Vol(%)` = 4
      ) %>% 
      mutate(across(c(1,3), ~.x *100))
    
    # 2. [Downside Risk] 하락 위험 (tibble 생성 단계에서부터 100 곱하고 이름 확정)
    calc_my_risks <- function(df) {
      ret_xts <- timetk::tk_xts(df, select = returns, date_var = date)
      tibble(
        `MDD(%)`    = maxDrawdown(ret_xts) %>% as.numeric() * 100,
        Sortino     = SortinoRatio(ret_xts, MAR = 0) %>% as.numeric(),
        Calmar      = CalmarRatio(ret_xts) %>% as.numeric(),
        `VaR.95(%)` = VaR(ret_xts, p = 0.95, method = "historical") %>% as.numeric() * 100
      )
    }
    
    perf_risk <- self$returns %>%
      group_by(symbol) %>%
      group_modify(~ calc_my_risks(.x)) %>%
      ungroup()
    
    # 3. [Consistency] 승률 (계산 시 100 곱하고 바로 이름 지정)
    perf_win <- self$returns %>%
      group_by(symbol) %>%
      summarise(`Win.Rate(%)` = (sum(returns > 0, na.rm=TRUE) / n()) * 100)
    
    # 4. 데이터 1차 병합
    stats_df <- left_join(perf_basic, perf_risk, by = "symbol") %>%
      left_join(perf_win, by = "symbol")
    
    # 5. [CAPM] 벤치마크 상대 지표 병합
    if (!is.null(self$benchmark_ticker)) {
      bm_data <- self$returns %>% filter(symbol == self$benchmark_ticker)
      asset_data <- self$returns %>% filter(symbol != self$benchmark_ticker)
      
      combined <- left_join(asset_data, bm_data, by = "date", suffix = c("", ".bm"))
      
      # 캡슐화된 transmute로 약칭 및 % 한 번에 처리
      perf_capm <- combined %>%
        group_by(symbol) %>%
        tq_performance(Ra = returns, Rb = returns.bm, performance_fun = table.CAPM) %>%
        transmute(
          `Alpha(%)` = Alpha * 100,
          Beta       = Beta,
          R2         = `R-squared`,
          Treynor    = TreynorRatio
        )
      
      perf_ir <- combined %>%
        group_by(symbol) %>%
        tq_performance(Ra = returns, Rb = returns.bm, performance_fun = InformationRatio) %>%
        rename(IR = 2)
      
      stats_df <- stats_df %>%
        left_join(perf_capm, by = "symbol") %>%
        left_join(perf_ir, by = "symbol")
    }
    
    # 6. [행열 교체 및 소수점 정리] (불필요한 mutate/select 로직 완전 제거)

    final_table <- stats_df %>%
      pivot_longer(cols = -symbol, names_to = "Metric", values_to = "Value") %>%
      mutate(Value = round(Value, 2)) %>%
      pivot_wider(names_from = symbol, values_from = Value)
    
    return(final_table)
  }
)


# 3. Plots List: 시각화====

qs_plots <- list(
  
  plot_wealth_index = function(title = "Cumulative Returns") {
    self$returns %>%
      group_by(symbol) %>%
      mutate(wealth = cumprod(1 + returns) - 1) %>%
      ggplot(aes(x = date, y = wealth, color = symbol)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = scales::percent) +
      theme_tq() +
      scale_color_tq() +
      labs(title = title, y = "Cumulative Return", x = "")
  },
  
  plot_underwater = function() {
    self$returns %>%
      group_by(symbol) %>%
      mutate(wealth = cumprod(1 + returns),
             peak = cummax(wealth),
             drawdown = (wealth - peak) / peak) %>%
      ggplot(aes(x = date, y = drawdown, fill = symbol)) +
      geom_area(alpha = 0.4, position = 'identity') +
      geom_hline(yintercept = 0, color = "black") +
      scale_y_continuous(labels = scales::percent) +
      theme_tq() +
      scale_fill_tq() +
      labs(title = "Drawdown Plot", y = "Drawdown", x = "")
  },
  
  plot_monthly_heatmap = function(target_symbol) {
    target_data <- self$returns %>% filter(symbol == target_symbol)
    if(nrow(target_data) == 0) stop("해당 심볼 없음")
    
    # [수정됨] periodReturn 제거, apply.monthly로 일별 수익률 복리 누적 계산
    target_data %>%
      tq_transmute(select = returns,
                   mutate_fun = apply.monthly,
                   FUN = function(x) prod(1 + x, na.rm = TRUE) - 1,
                   col_rename = "ret") %>%
      mutate(year = year(date), 
             month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
      ggplot(aes(x = month, y = factor(year), fill = ret)) +
      geom_tile(color = "white") +
      geom_text(aes(label = scales::percent(ret, accuracy = 0.1)), size = 3) +
      scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", 
                           midpoint = 0, labels = scales::percent) +
      theme_minimal() +
      labs(title = glue("Monthly Returns: {target_symbol}"), x = "", y = "")
  },
  
  # --- [추가] 롤링 변동성 (이동 표준편차) ---
  plot_rolling_volatility = function(width = 252 * 3, title = "Rolling Volatility (Annualized)") {
    # width: 252(1년), 504(2년), 756(3년)
    
    # 윈도우 기간보다 데이터가 짧으면 에러 방지
    if (nrow(self$returns) < width) {
      message("데이터 기간이 롤링 윈도우보다 짧아 그래프를 그릴 수 없습니다.")
      return(NULL)
    }
    
    self$returns %>%
      group_by(symbol) %>%
      tq_mutate(select = returns,
                mutate_fun = runSD,  # TTR::runSD (이동 표준편차)
                n = width,
                col_rename = "rolling_sd") %>%
      filter(!is.na(rolling_sd)) %>%
      mutate(rolling_vol = rolling_sd * sqrt(252)) %>% # 연환산
      ggplot(aes(x = date, y = rolling_vol, color = symbol)) +
      geom_line(size = 0.8) +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste0(title, " - ", round(width/252, 1), " Years Rolling"),
           y = "Annualized Volatility", x = "")
  },
  
  # --- [추가] 롤링 샤프지수 (위험 대비 수익 효율성) ---
  plot_rolling_sharpe = function(width = 252 * 3, rf = 0) {
    
    if (nrow(self$returns) < width) return(NULL)
    
    # Rolling Sharpe 계산 함수 정의 (tq_mutate용)
    rolling_sharpe_fun <- function(x) {
      ret <- prod(1 + x)^(252/length(x)) - 1 # CAGR
      vol <- sd(x) * sqrt(252)               # Volatility
      return((ret - rf) / vol)
    }
    
    self$returns %>%
      group_by(symbol) %>%
      tq_mutate(select = returns,
                mutate_fun = rollapply,
                width = width,
                FUN = rolling_sharpe_fun,
                by.column = FALSE,
                col_rename = "rolling_sharpe") %>%
      filter(!is.na(rolling_sharpe)) %>%
      ggplot(aes(x = date, y = rolling_sharpe, color = symbol)) +
      geom_line(size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      theme_tq() +
      scale_color_tq() +
      labs(title = paste0("Rolling Sharpe Ratio - ", round(width/252, 1), " Years"),
           y = "Sharpe Ratio", x = "")
  }
  
  
)


# 4. Reports List: 보고서 ====

qs_reports <- list(
  
  get_monthly_returns_table = function() {
    # [수정됨] 일별 수익률을 복리 누적(prod)하여 월간 수익률로 집계
    self$returns %>%
      group_by(symbol) %>%
      tq_transmute(select = returns,                 
                   mutate_fun = apply.monthly,
                   FUN = function(x) (prod(1 + x, na.rm = TRUE) - 1)*100,
                   col_rename = "returns") %>%
      mutate(ym = format(date, "%Y-%m")) %>%
      pivot_wider(names_from = symbol, values_from = returns) %>%
      arrange(desc(ym)) %>% select(-date)
  },
  
  get_yearly_returns_table = function() {
    # [수정됨] 일별 수익률을 복리 누적(prod)하여 연간 수익률로 집계
    self$returns %>%
      group_by(symbol) %>%
      tq_transmute(select = returns,                 
                   mutate_fun = apply.yearly,
                   FUN = function(x) (prod(1 + x, na.rm = TRUE) - 1)*100,
                   col_rename = "returns") %>%
      mutate(year = year(date)) %>%
      pivot_wider(names_from = symbol, values_from = returns) %>%
      arrange(desc(year)) %>% select(-date)
  }
)


#[클래스] QuantStatsR ====

QuantStatsR <- R6Class(
  classname = "QuantStatsR",
  
  public = c(
    ## (Core) 속성 및 초기화 ====
    list(
      tickers = NULL,
      benchmark_ticker = NULL,
      rf_ticker = NULL,
      data_raw = NULL,
      returns = NULL,
      
      initialize = function(tickers, start_date, end_date = Sys.Date(), 
                            benchmark_ticker = NULL, rf_ticker = NULL) {
        
        self$tickers <- tickers
        self$benchmark_ticker <- benchmark_ticker
        self$rf_ticker <- rf_ticker
        
        self$fetch_data(start_date, end_date)
      },
      
      fetch_data = function(start, end) {
        targets <- unique(c(self$tickers, self$benchmark_ticker, self$rf_ticker))
        targets <- targets[!is.null(targets)]
        
        message(glue("Fetching data for {length(targets)} tickers..."))
        
        tryCatch({
          self$data_raw <- tq_get(targets, get = "stock.prices", from = start, to = end)
        }, error = function(e) { stop("데이터 다운로드 실패") })
        
        # 일별 수익률 계산 ====
        self$returns <- self$data_raw %>%
          group_by(symbol) %>%
          tq_transmute(select = adjusted, 
                       mutate_fun = periodReturn, 
                       period = "daily", 
                       type = "arithmetic", 
                       col_rename = "returns") %>%
          ungroup()
        
        message("Initialization Complete.")
      }
    ),
    
    ## (Mixin) 기능별 리스트 병합 ====
    qs_utils,
    qs_stats,
    qs_plots,
    qs_reports
  )
)

