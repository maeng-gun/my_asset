library(R6)
library(lubridate)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(scales)
library(glue)
library(tidyverse)
library(flextable)

# 1. Utils List ====
qs_utils <- list(
  
## 1)포트폴리오 구성====
  add_portfolio = function(tickers, weights, 
                           name = "Portfolio", rebalance_on = "months") {
    if (length(tickers) != length(weights)) stop("Error: 티커와 비중의 개수가 일치해야 합니다.")
    
    target_assets <- self$base_returns %>% filter(symbol %in% tickers)
    
    # [추가] 모든 티커가 공통적으로 존재하는 날짜 범위 찾기
    common_dates <- target_assets %>%
      group_by(symbol) %>%
      summarise(start = min(date), end = max(date), .groups = "drop") %>%
      summarise(max_start = max(start), min_end = min(end))
    
    # [추가] 공통 범위로 데이터 자르기
    target_assets <- target_assets %>%
      filter(date >= common_dates$max_start, date <= common_dates$min_end)
    
    port_ret <- suppressWarnings(
      target_assets %>%
        tq_portfolio(assets_col = symbol, 
                     returns_col = returns, 
                     weights = weights, 
                     col_rename = "returns",
                     rebalance_on = rebalance_on)
    ) %>% mutate(symbol = name)
    
    self$base_returns <- self$base_returns %>% 
      filter(symbol != name) %>% bind_rows(port_ret)
    
    message(glue("  [Utils] '{name}' 포트폴리오가 생성되었습니다."))
  },
  
## 2) 분석대상 설정====
  set_targets = function(target_symbols, bm_symbol = NULL, 
                         start_date=NULL, end_date=NULL) {
    
# 1. 타겟과 BM 데이터 임시 추출 ----
    temp_returns <- self$base_returns %>% filter(symbol %in% target_symbols)
    temp_bm <- if(!is.null(bm_symbol)) self$base_returns %>% filter(symbol == bm_symbol) else NULL
    
    # [추가] 타겟들과 BM 전체를 아우르는 공통 날짜 계산
    all_involved <- if(!is.null(temp_bm)) bind_rows(temp_returns, temp_bm) else temp_returns
    
    if(!is.null(start_date)){
      all_involved <- all_involved %>% 
        add_row(symbol='temp', date=as.Date(start_date))
    }
    
    if(!is.null(end_date)){
      all_involved <- all_involved %>% 
        add_row(symbol='temp', date=as.Date(end_date))
    }
    
    common_range <- all_involved %>%
      group_by(symbol) %>%
      summarise(start = min(date), end = max(date), .groups = "drop") %>%
      summarise(max_start = max(start), min_end = min(end))
    
    self$returns <- temp_returns %>% 
      filter(date >= common_range$max_start, date <= common_range$min_end)
    
    if (!is.null(temp_bm)) {
      self$bm <- temp_bm %>% 
        filter(date >= common_range$max_start, date <= common_range$min_end)
    }

    message(glue("  [Utils] 분석 타겟({paste(target_symbols, collapse=', ')}) 및 BM({bm_symbol}) 설정 완료"))
  }
)


# 2. Stats List ====

qs_stats <- list(
  calc_stats_table = function() {
# 1. 분석 대상과 벤치마크 통합 데이터 준비 ----
    # 타겟 자산들과 BM을 합쳐서 계산 루프에 넣음 (BM도 기초 통계 산출 대상)
    assets_to_analyze <- self$returns
    if (!is.null(self$bm)) {
      assets_to_analyze <- bind_rows(assets_to_analyze, self$bm)
    }
    
    data_merged <- assets_to_analyze %>%
      left_join(self$rf_data %>% select(date, rf_ret = returns), by = "date")%>%
      replace_na(list(rf_ret = 0))
    
    # 벤치마크 수익률은 상대 지표(CAPM 등) 계산을 위해 별도 컬럼으로 모든 행에 Join
    if (!is.null(self$bm)) {
      bm_series <- self$bm %>% select(date, bm_ret = returns)
      data_merged <- data_merged %>% left_join(bm_series, by = "date")
    }
    
# 2. 그룹별 지표 산출 ----
    stats_df <- data_merged %>%
      group_by(symbol) %>%
      group_modify(~ {
        # 시계열 객체(xts)로 변환
        ra_xts <- timetk::tk_xts(.x, select = returns, date_var = date)
        rf_xts <- timetk::tk_xts(.x, select = rf_ret, date_var = date)
        
        # (A) 기초 통계 (Raw 기반)
        ann_ret <- Return.annualized(ra_xts, scale = 252)
        ann_vol <- StdDev.annualized(ra_xts, scale = 252)
        mdd     <- maxDrawdown(ra_xts)
        calmar  <- ann_ret / abs(mdd) # Calmar 추가
        win_rate <- sum(.x$returns > 0, na.rm = TRUE) / sum(!is.na(.x$returns))
        var_95  <- VaR(ra_xts, p = 0.95, method = "historical")
        
        # (B) 성과/위험 지표 (Rf 시계열 직접 투입)
        sharpe  <- SharpeRatio.annualized(ra_xts, Rf = rf_xts, scale = 252)
        sortino <- SortinoRatio(ra_xts, MAR = rf_xts)
        
        res <- tibble(
          `Ann.Ret(%)`  = as.numeric(ann_ret) * 100,
          `Ann.Vol(%)`  = as.numeric(ann_vol) * 100,
          `Sharpe`      = as.numeric(sharpe),
          `Sortino`     = as.numeric(sortino),
          `Calmar`      = as.numeric(calmar), # Calmar 추가
          `MDD(%)`      = as.numeric(mdd) * 100,
          `Win.Rate(%)` = win_rate * 100,
          `VaR.95(%)`   = as.numeric(var_95) * 100
        )
        
        # (C) CAPM 및 상대 지표 (BM이 있고, 현재 symbol이 BM이 아닐 때만 계산)
        bm_ticker <- if(!is.null(self$bm)) unique(self$bm$symbol) else NULL
        current_symbol <- .y$symbol
        
        if (!is.null(bm_ticker)) {
          if (current_symbol != bm_ticker) {
            rb_xts <- timetk::tk_xts(.x, select = bm_ret, date_var = date)
            capm_stats <- table.CAPM(ra_xts, rb_xts, Rf = rf_xts, scale = 252)
            ir <- InformationRatio(ra_xts, rb_xts) # IR 추가
            
            res <- res %>% mutate(
              `Alpha(%)` = capm_stats["Alpha", 1] * 100,
              `Beta`     = capm_stats["Beta", 1],
              `R2`       = capm_stats["R-squared", 1], # R2 추가
              `Treynor`  = capm_stats["Treynor Ratio", 1],
              `IR`       = as.numeric(ir) # IR 추가
            )
          } else {
            # 현재 분석 대상이 BM인 경우 상대 지표는 NA 처리
            res <- res %>% mutate(
              `Alpha(%)` = NA_real_,
              `Beta`     = NA_real_,
              `R2`       = NA_real_,
              `Treynor`  = NA_real_,
              `IR`       = NA_real_
            )
          }
        }
        return(res)
      }) %>% ungroup()
    
# 3. 테이블 정리 (Long -> Wide) ----
    final_table <- stats_df %>%
      pivot_longer(cols = -symbol, names_to = "Metric", values_to = "Value") %>%
      mutate(Value = round(Value, 2)) %>%
      pivot_wider(names_from = symbol, values_from = Value)
    
    # [추가] BM 컬럼을 가장 마지막 열로 배치
    if (!is.null(self$bm)) {
      bm_name <- unique(self$bm$symbol)
      other_tickers <- setdiff(colnames(final_table), c("Metric", bm_name))
      final_table <- final_table %>% 
        select(Metric, all_of(other_tickers), all_of(bm_name))
    }
    
    return(final_table)
  },
  
  get_monthly_returns_table = function() {
    # [수정됨] 일별 수익률을 복리 누적(prod)하여 월간 수익률로 집계
    
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
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
    
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
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

# 3. Plots List====

qs_plots <- list(
  
  plot_wealth_index = function(title = "Cumulative Returns") {
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
      group_by(symbol) %>%
      mutate(wealth = cumprod(1 + returns) - 1) %>%
      ggplot(aes(x = date, y = wealth, color = symbol)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = scales::percent) +
      theme_tq() + scale_color_tq() +
      labs(title = title, y = "Cumulative Return", x = "")
  },
  
  plot_underwater = function() {
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
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
  
  plot_monthly_heatmap = function(sym=NULL) {
    
    if(is.null(sym)){
      sym <- self$returns$symbol[1]
    }
    
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
      filter(symbol==sym) %>% 
      tq_transmute(select = returns, mutate_fun = apply.monthly, FUN = function(x) prod(1 + x) - 1, col_rename = "ret") %>%
      mutate(year = year(date), month = month(date, label = TRUE, abbr = TRUE)) %>%
      ggplot(aes(x = month, y = factor(year), fill = ret)) +
      geom_tile(color = "white") +
      geom_text(aes(label = scales::percent(ret, accuracy = 0.1)), size = 2.5) +
      scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0) +
      theme_minimal() + labs(title = glue("Monthly Returns Heatmap : {sym}"), x = "", y = "")
  },
  
  plot_yearly_returns = function() {
# 1. 분석 대상(Targets)과 벤치마크(BM) 통합 데이터 준비 ----
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
# 2. 연도별 수익률 계산 ----
    plot_data %>%
      group_by(symbol, year = year(date)) %>%
      summarise(ret = prod(1 + returns) - 1, .groups = "drop") %>%
      mutate(year = factor(year)) %>%
      
# 3. 시각화: x축은 연도, fill은 자산별 구분 ----
      ggplot(aes(x = year, y = ret, fill = symbol)) +
      # position_dodge를 통해 막대를 나란히 배치
      geom_col(position = position_dodge(width = 0.8)) +
      # 텍스트 라벨도 막대 위치에 맞춰 정렬
      geom_text(aes(label = scales::percent(ret, accuracy = 0.1)), 
                position = position_dodge(width = 0.8), 
                vjust = -0.5, 
                size = 3) +
      scale_y_continuous(labels = scales::percent) +
      theme_tq() + 
      scale_fill_tq() +
      labs(title = "Yearly Returns Comparison: Targets vs Benchmark", 
           y = "Annual Return", 
           x = "Year") +
      theme(legend.position = "bottom")
  },
  
# --- [추가] 롤링 변동성 (이동 표준편차) ----
  plot_rolling_volatility = function(width = 252 * 3, 
                                     title = "Rolling Volatility (Annualized)") {
    # width: 252(1년), 504(2년), 756(3년)
    
    # 윈도우 기간보다 데이터가 짧으면 에러 방지
    if (length(unique(self$returns$date)) < width) {
      message("데이터 기간이 롤링 윈도우보다 짧아 그래프를 그릴 수 없습니다.")
      return(NULL)
    }
    
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    plot_data %>%
      group_by(symbol) %>%
      tq_mutate(select = returns,
                mutate_fun = runSD,  # TTR::runSD (이동 표준편차)
                n = width,
                col_rename = "rolling_sd") %>%
      filter(!is.na(rolling_sd)) %>%
      mutate(rolling_vol = rolling_sd * sqrt(252)) %>% # 연환산
      ggplot(aes(x = date, y = rolling_vol, color = symbol)) +
      geom_line(linewidth = 0.8) +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste0(title, " - ", round(width/252, 1), " Years Rolling"),
           y = "Annualized Volatility", x = "")
  },
  
# --- [추가] 롤링 샤프지수 (위험 대비 수익 효율성) ----
  plot_rolling_sharpe = function(width = 252 * 3, rf = 0) {
    
    if (length(unique(self$returns$date)) < width) {
      message("데이터 기간이 롤링 윈도우보다 짧아 그래프를 그릴 수 없습니다.")
      return(NULL)
    }
    
    plot_data <- self$returns
    if (!is.null(self$bm)) plot_data <- bind_rows(plot_data, self$bm)
    
    
    # Rolling Sharpe 계산 함수 정의 (tq_mutate용)
    rolling_sharpe_fun <- function(x) {
      ret <- prod(1 + x)^(252/length(x)) - 1 # CAGR
      vol <- sd(x) * sqrt(252)               # Volatility
      return((ret - rf) / vol)
    }
    
    plot_data %>%
      group_by(symbol) %>%
      tq_mutate(select = returns,
                mutate_fun = rollapply,
                width = width,
                FUN = rolling_sharpe_fun,
                by.column = FALSE,
                col_rename = "rolling_sharpe") %>%
      filter(!is.na(rolling_sharpe)) %>%
      ggplot(aes(x = date, y = rolling_sharpe, color = symbol)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      theme_tq() +
      scale_color_tq() +
      labs(title = paste0("Rolling Sharpe Ratio - ", round(width/252, 1), " Years"),
           y = "Sharpe Ratio", x = "")
  }
)


# 4. Reports List: 보고서 ====

qs_reports <- list(
  
  report_full = function() {
    if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable 패키지가 필요합니다.")
    library(flextable)
    library(ggplot2)
    
# 1. 성과 요약 통계 (flextable) ----
    stats_df <- self$calc_stats_table()
    ft_stats <- stats_df %>%
      flextable() %>%
      theme_vanilla() %>%
      set_caption("1. Performance Statistics Summary") %>%
      autofit() %>%
      colformat_double(digits = 2) %>%
      bg(i = ~ Metric %in% c("Ann.Ret(%)", "Sharpe", "MDD(%)"), bg = "#f2f2f2", part = "body") %>%
      bold(i = ~ Metric %in% c("Ann.Ret(%)", "Sharpe"), part = "body")
    
    
    nm <- names(stats_df)[-1]
    
    print(ft_stats)
    print(self$plot_wealth_index())
    print(self$plot_underwater())
    print(self$plot_yearly_returns())
    print(map(nm, ~qs_plots$plot_monthly_heatmap(.x)))
    print(self$plot_rolling_volatility())
    print(self$plot_rolling_sharpe())
    
  }
)


#[클래스] QuantStatsR ====

QuantStatsR <- R6Class(
  "QuantStatsR",
  public = c(
    list(
      tickers = NULL, base_returns = NULL, returns = NULL, 
      bm = NULL, rf_data = NULL,
      
#1. 초기화 ====
      initialize = function(tickers, rf_ticker=NULL, rf_type = c("etf", "yield"), start_date, end_date = Sys.Date()) {
        self$tickers <- tickers
        self$fetch_data(start_date, end_date, rf_ticker, rf_type)
      },
      
      fetch_data = function(start, end, rf_ticker, rf_type) {
        all_tickers <- unique(c(self$tickers, rf_ticker))
        raw_data <- tq_get(all_tickers, from = start, to = end)
        
        if(!is.null(rf_ticker)){
          raw_data_ <- raw_data %>% filter(symbol != rf_ticker)
        } else {
          raw_data_ <- raw_data
        }
          
        # Base Returns (원본 자산)
        self$base_returns <- raw_data_ %>%
          group_by(symbol) %>%
          tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", col_rename = "returns") %>%
          ungroup()
        
        # Rf 처리 (Fill-down 적용)
        if(!is.null(rf_ticker)){
          rf_raw <- raw_data %>% filter(symbol == rf_ticker)
          if (rf_type == "yield") {
            self$rf_data <- rf_raw %>%
              select(date, adjusted) %>% 
              mutate(returns = adjusted / 100 / 252)
          } else {
            self$rf_data <- rf_raw %>%
              tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                           period = "daily", col_rename = "returns")
          }
        } else {
          self$rf_data <- self$base_returns %>% select(date) %>% 
            mutate(returns = 0) %>% distinct(date, .keep_all = T)
        }
        
        

        
        message("Data fetch & Rf preparation complete.")
      }
    ),
#2. 기능별 리스트 추가====
    qs_utils, qs_stats, qs_plots, qs_reports
  )
)
