# =============================================================================
# reactable 렌더링 유틸리티 함수
# =============================================================================
# 앱 전체에서 반복 사용되는 reactable 파이프라인을 재사용 가능한 함수로 추출
# =============================================================================

library(reactable)

#' 숫자 컬럼 포매터 생성 헬퍼 (소수점 자리수, 천단위 쉼표)
#' @param digits 소수점 자리수
#' @return reactable::colFormat
fmt_number <- function(digits = 0) {
  colFormat(separators = TRUE, digits = digits)
}

#' 퍼센트 컬럼 포매터 생성 헬퍼 (퍼센트 형식 대신 단순 소수점 표기 유지)
#' @param digits 소수점 자리수
#' @return reactable::colFormat
fmt_percent <- function(digits = 2) {
  colFormat(separators = TRUE, digits = digits)
}

#' 텍스트 말줄임 + 호버 툴팁 셀 렌더러 생성
#' @details htmltools::tag 대신 HTML 문자열 반환 → reactable의 html=TRUE와 함께 사용.
#'          div() 객체는 JSON 직렬화 과정에서 JS 레벨 오류를 유발할 수 있음.
#' @return character — HTML 문자열
ellipsis_cell <- function() {
  function(value) {
    v <- htmltools::htmlEscape(as.character(value %||% ""))
    sprintf(
      '<div title="%s" style="overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">%s</div>',
      v, v
    )
  }
}


#' 기본 reactable 렌더링 (모바일 최적화 및 정렬 적용)
#' @param df 데이터프레임
#' @param int_cols 정수 포맷(천단위 쉼표)을 적용할 컬럼 이름 또는 인덱스
#' @param dec_cols 소수점 2자리 포맷을 적용할 컬럼 이름 또는 인덱스 (예: 외화금액)
#' @param pct_cols 퍼센트(소수) 포맷을 적용할 컬럼 이름 또는 인덱스
#' @param sticky_cols 좌측 고정(sticky)할 컬럼 이름 또는 인덱스. 최대 5개 권장.
#' @param long_str_cols 말줄임 + 호버 툴팁을 적용할 긴 문자열 컬럼 이름 또는 인덱스
#' @param groupBy 그룹화할 컬럼 이름 벡터 (reactable의 groupBy 옵션과 직접 연결)
#' @param align 전체 기본 정렬 ("center", "left", "right")
#' @param searchable 검색창 표시 여부
#' @param sortable 정렬 가능 여부
#' @param height 테이블 고정 높이(px). NULL이면 전체 표시.
#' @return reactable 객체
render_rt <- function(df,
                      int_cols = NULL,
                      dec_cols = NULL,
                      pct_cols = NULL,
                      sticky_cols = NULL,
                      long_str_cols = NULL,
                      groupBy = NULL,
                      align = "center",
                      sortable = FALSE,
                      searchable = FALSE,
                      height = NULL,
                      dynamic_height = TRUE) {
  # 인덱스로 지정된 경우 컬럼 이름으로 변환
  to_names <- function(cols) if (is.numeric(cols)) names(df)[cols] else cols
  int_cols <- to_names(int_cols)
  dec_cols <- to_names(dec_cols)
  pct_cols <- to_names(pct_cols)
  sticky_cols <- to_names(sticky_cols)
  long_str_cols <- to_names(long_str_cols)

  # 컬럼 정의 리스트 초기화 및 동적 너비 계산
  col_defs <- list()
  for (col in names(df)) {
    col_args <- list()
    
    # 특정 컬럼만 동적 너비 계산
    if (col %in% c("상품명", "종목명", "계좌")) {
      vals <- as.character(df[[col]])
      max_len <- max(c(0, nchar(vals)), na.rm = TRUE)
      col_name_len <- nchar(col)
      # 한글 등 폭 넉넉히 계산 (글자수 * 14px + 여백 40px)
      calculated_width <- max(max_len * 14, col_name_len * 16) + 40
      col_args$minWidth <- min(max(calculated_width, 80), 500)
    }
    
    # 포맷 적용
    if (col %in% int_cols) {
      col_args$format <- fmt_number(0)
    } else if (col %in% dec_cols) {
      col_args$format <- fmt_number(2)
    } else if (col %in% pct_cols) {
      col_args$format <- fmt_percent(2)
    }
    
    # 좌측 고정(sticky) 컬럼 적용
    if (col %in% sticky_cols) {
      col_args$sticky <- "left"
    }
    
    # 긴 문자열 컬럼 — CSS 말줄임 + 호버 툴팁
    # html = TRUE: cell 함수가 HTML 문자열을 반환하며, reactable이 HTML로 해석하도록 설정
    if (col %in% long_str_cols) {
      col_args$html <- TRUE
      if (is.null(col_args$minWidth)) {
        col_args$minWidth <- 150
      } else {
        col_args$minWidth <- max(col_args$minWidth, 150)
      }
      col_args$cell <- ellipsis_cell()
      col_args$style <- list(
        maxWidth = "200px", overflow = "hidden",
        textOverflow = "ellipsis", whiteSpace = "nowrap"
      )
    }
    
    if (length(col_args) > 0) {
      col_defs[[col]] <- do.call(colDef, col_args)
    }
  }

  # 테이블 렌더링
  rt <- reactable(
    data = df,
    groupBy = groupBy,
    columns = col_defs,
    defaultColDef = colDef(
      align = align,
      style = list(whiteSpace = "nowrap")
    ),
    pagination = FALSE, # 한 화면에 모두 표시
    resizable = TRUE,
    height = height, # 세로 스크롤 고정 높이 (기본 NULL)
    wrap = FALSE, # 텍스트 줄바꿈 방지
    sortable = sortable, # 정렬 기능
    searchable = searchable, # 검색 기능
    striped = TRUE, # 짝수행 배경색 다르게
    highlight = TRUE, # 마우스 호버 시 강조
    compact = TRUE, # 패딩을 줄여 컴팩트하게
    theme = reactableTheme(
      style = list(
        overflowX = "auto",
        WebkitOverflowScrolling = "touch",
        fontSize = "clamp(12px, 3vw, 14px)"
      )
    )
  )

  # 브라우저 창 크기에 맞춰 동적으로 테이블 높이 재계산 (JS 주입)
  if (dynamic_height) {
    htmlwidgets::onRender(rt, "
      function(el, x) {
        function resize() {
          // 화면에 숨겨진 상태면 계산하지 않음
          if (el.offsetWidth === 0 && el.offsetHeight === 0) return;
          var rect = el.getBoundingClientRect();
          if (rect.top <= 0) return;
          
          // 창 높이에서 테이블 윗부분의 위치와 여유 공간(30px)을 뺌
          var remaining = window.innerHeight - rect.top - 30;
          if (remaining > 200) {
            el.style.height = remaining + 'px';
          }
        }
        setTimeout(resize, 100); // 렌더링 직후 높이 조정
        window.addEventListener('resize', resize); // 창 크기 조절 시 대응
        
        // 탭 전환 이벤트 감지 (Bootstrap 5)
        if (window.jQuery) {
          $('a[data-bs-toggle=\"tab\"], a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
            setTimeout(resize, 50);
          });
        }
        
        // 혹시 UI가 동적으로 변해 위치가 바뀌면 대응하도록 Observer 추가
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
