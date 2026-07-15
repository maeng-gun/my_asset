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

#' 퍼센트 컬럼 포매터 생성 헬퍼
#' @param digits 소수점 자리수
#' @return reactable::colFormat
fmt_percent <- function(digits = 2) {
  colFormat(digits = digits)
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
                      height = "calc(100vh - 200px)") {

  # 인덱스로 지정된 경우 컬럼 이름으로 변환
  to_names <- function(cols) if (is.numeric(cols)) names(df)[cols] else cols
  int_cols      <- to_names(int_cols)
  dec_cols      <- to_names(dec_cols)
  pct_cols      <- to_names(pct_cols)
  sticky_cols   <- to_names(sticky_cols)
  long_str_cols <- to_names(long_str_cols)

  # 컬럼 정의 리스트 초기화
  col_defs <- list()

  # 정수 컬럼 적용 (천단위 쉼표, 소수점 0자리)
  for (col in int_cols) {
    col_defs[[col]] <- colDef(format = fmt_number(0))
  }

  # 외화 소수점 2자리 컬럼 적용
  for (col in dec_cols) {
    col_defs[[col]] <- colDef(format = fmt_number(2))
  }

  # 퍼센트(소수) 컬럼 적용
  for (col in pct_cols) {
    col_defs[[col]] <- colDef(format = fmt_percent(2))
  }

  # 좌측 고정(sticky) 컬럼 적용 — 기존 colDef가 있으면 덮어쓰지 않고 sticky 속성만 추가
  for (col in sticky_cols) {
    existing <- col_defs[[col]]
    if (is.null(existing)) {
      col_defs[[col]] <- colDef(sticky = "left")
    } else {
      # 기존 정의에 sticky 추가: 리스트 병합 방식
      col_defs[[col]] <- modifyList(existing, list(sticky = "left"))
    }
  }

  # 긴 문자열 컬럼 — CSS 말줄임 + 호버 툴팁
  # html = TRUE: cell 함수가 HTML 문자열을 반환하며, reactable이 HTML로 해석하도록 설정
  for (col in long_str_cols) {
    existing <- col_defs[[col]]
    cell_fn  <- ellipsis_cell()
    if (is.null(existing)) {
      col_defs[[col]] <- colDef(
        html     = TRUE,
        minWidth = 150,
        cell     = cell_fn,
        style    = list(maxWidth = "200px", overflow = "hidden",
                        textOverflow = "ellipsis", whiteSpace = "nowrap")
      )
    } else {
      col_defs[[col]] <- modifyList(existing, list(
        html     = TRUE,
        minWidth = 150,
        cell     = cell_fn,
        style    = list(maxWidth = "200px", overflow = "hidden",
                        textOverflow = "ellipsis", whiteSpace = "nowrap")
      ))
    }
  }

  # 테이블 렌더링
  reactable(
    data = df,
    groupBy = groupBy,
    columns = col_defs,
    defaultColDef = colDef(
      align = align,
      minWidth = 100,
      style = list(whiteSpace = "nowrap")
    ),
    pagination = FALSE,      # 한 화면에 모두 표시
    resizable = TRUE,
    height = height,         # 세로 스크롤 고정 높이 (헤더 고정)
    wrap = FALSE,            # 텍스트 줄바꿈 방지
    sortable = sortable,     # 정렬 기능
    searchable = searchable, # 검색 기능
    striped = TRUE,          # 짝수행 배경색 다르게
    highlight = TRUE,        # 마우스 호버 시 강조
    compact = TRUE           # 패딩을 줄여 컴팩트하게
  )
}
