# =============================================================================
# utils_reactable.R — reactable 렌더링 유틸리티 함수
# =============================================================================
# 앱 전체에서 반복 사용되는 reactable 파이프라인을 재사용 가능한 함수로 추출
# =============================================================================

library(reactable)

# 1. 숫자 포매터 생성 헬퍼 ----
fmt_number <- function(digits = 0) {
  colFormat(separators = TRUE, digits = digits)
}

# 2. 퍼센트 포매터 생성 헬퍼 ----
fmt_percent <- function(digits = 2) {
  colFormat(separators = TRUE, digits = digits)
}

# 3. 말줄임 셀 렌더러 생성 ----
ellipsis_cell <- function() {
  function(value) {
    v <- htmltools::htmlEscape(as.character(value %||% ""))
    sprintf(
      '<div title="%s" style="overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">%s</div>',
      v, v
    )
  }
}

# 4. reactable 메인 렌더러 ----
render_rt <- function(df,
                      int_cols = NULL,
                      dec_cols = NULL,
                      pct_cols = NULL,
                      sticky_cols = NULL,
                      long_str_cols = NULL,
                      border_cols = NULL,
                      groupBy = NULL,
                      align = "center",
                      sortable = FALSE,
                      searchable = FALSE,
                      height = NULL,
                      dynamic_height = TRUE) {
  to_names <- function(cols) if (is.numeric(cols)) names(df)[cols] else cols
  int_cols <- to_names(int_cols)
  dec_cols <- to_names(dec_cols)
  pct_cols <- to_names(pct_cols)
  sticky_cols <- to_names(sticky_cols)
  long_str_cols <- to_names(long_str_cols)
  border_cols <- to_names(border_cols)

  col_defs <- list()
  for (col in names(df)) {
    col_args <- list()

    if (col %in% c("상품명", "종목명", "계좌")) {
      vals <- as.character(df[[col]])
      max_len <- max(c(0, nchar(vals)), na.rm = TRUE)
      col_name_len <- nchar(col)
      calculated_width <- max(max_len * 14, col_name_len * 16) + 40
      col_args$minWidth <- min(max(calculated_width, 80), 500)
    }

    if (col %in% int_cols) {
      col_args$format <- fmt_number(0)
    } else if (col %in% dec_cols) {
      col_args$format <- fmt_number(2)
    } else if (col %in% pct_cols) {
      col_args$format <- fmt_percent(2)
    }

    if (col %in% sticky_cols) {
      col_args$sticky <- "left"
    }

    if (col %in% long_str_cols) {
      col_args$html <- TRUE
      if (is.null(col_args$minWidth)) {
        col_args$minWidth <- 150
      } else {
        col_args$minWidth <- max(col_args$minWidth, 150)
      }
      col_args$cell <- ellipsis_cell()
      if (is.null(col_args$style)) col_args$style <- list()
      col_args$style$maxWidth <- "200px"
      col_args$style$overflow <- "hidden"
      col_args$style$textOverflow <- "ellipsis"
      col_args$style$whiteSpace <- "nowrap"
    }

    if (col %in% border_cols) {
      if (is.null(col_args$style)) col_args$style <- list()
      col_args$style$borderLeft <- "2px solid #444"
      col_args$style$borderRight <- "2px solid #444"

      if (is.null(col_args$headerStyle)) col_args$headerStyle <- list()
      col_args$headerStyle$borderLeft <- "2px solid #444"
      col_args$headerStyle$borderRight <- "2px solid #444"
      col_args$headerStyle$borderTop <- "2px solid #444"
    }

    if (length(col_args) > 0) {
      col_defs[[col]] <- do.call(colDef, col_args)
    }
  }

  rt <- reactable(
    data = df,
    groupBy = groupBy,
    columns = col_defs,
    defaultColDef = colDef(
      align = align,
      style = list(whiteSpace = "nowrap")
    ),
    pagination = FALSE,
    resizable = TRUE,
    height = height,
    wrap = FALSE,
    sortable = sortable,
    searchable = searchable,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    theme = reactableTheme(
      style = list(
        overflowX = "auto",
        WebkitOverflowScrolling = "touch",
        fontSize = "clamp(12px, 3vw, 14px)"
      )
    )
  )

  if (dynamic_height) {
    htmlwidgets::onRender(rt, "
      function(el, x) {
        function resize() {
          if (el.offsetWidth === 0 && el.offsetHeight === 0) return;
          var rect = el.getBoundingClientRect();
          if (rect.top <= 0) return;
          
          var remaining = window.innerHeight - rect.top - 30;
          if (remaining > 200) {
            el.style.height = remaining + 'px';
          }
        }
        setTimeout(resize, 100);
        window.addEventListener('resize', resize);
        
        if (window.jQuery) {
          $('a[data-bs-toggle=\"tab\"], a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
            setTimeout(resize, 50);
          });
        }
        
        var observer = new MutationObserver(function() {
          resize();
        });
        observer.observe(document.body, { childList: true, subtree: true });
      }
    ")
  } else {
    rt
  }
}
