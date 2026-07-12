# =============================================================================
# flextable 렌더링 유틸리티 함수
# =============================================================================
# 앱 전체에서 반복 사용되는 flextable → htmltools_value 파이프라인을
# 재사용 가능한 함수로 추출
# =============================================================================

#' flextable → htmltools_value 기본 렌더링
#' @param ft flextable 객체
#' @param align 정렬 ('center', 'left', 'right')
#' @return htmltools 값
render_ft <- function(ft, align = "center") {
  ft |>
    set_table_properties(layout = 'autofit') |>
    htmltools_value(ft.align = align)
}

#' theme_vanilla 적용 렌더링
#' @param df 데이터프레임
#' @param merge_cols 병합할 열 인덱스 (예: 1:3). NULL이면 병합 안함
#' @param double_cols 소수점 없이 표시할 열 인덱스
#' @param pct_cols 소수점 2자리로 표시할 열 인덱스
#' @param align 정렬
#' @return htmltools 값
render_ft_vanilla <- function(df,
                              merge_cols = NULL,
                              double_cols = NULL,
                              pct_cols = NULL,
                              align = "center") {
  ft <- df |>
    flextable() |>
    theme_vanilla()

  if (!is.null(merge_cols)) {
    ft <- ft |> merge_v(j = merge_cols)
  }

  if (!is.null(double_cols)) {
    ft <- ft |> colformat_double(j = double_cols, digits = 0)
  }

  if (!is.null(pct_cols)) {
    ft <- ft |> colformat_double(j = pct_cols, digits = 2)
  }

  ft |> render_ft(align = align)
}

#' theme_box 적용 렌더링
#' @param df 데이터프레임
#' @param merge_cols 병합할 열 인덱스
#' @param double_cols 소수점 없이 표시할 열 인덱스
#' @param pct_cols 소수점 2자리로 표시할 열 인덱스
#' @param align 정렬
#' @return htmltools 값
render_ft_box <- function(df,
                          merge_cols = NULL,
                          double_cols = NULL,
                          pct_cols = NULL,
                          align = "center") {
  ft <- df |>
    flextable() |>
    theme_box()

  if (!is.null(merge_cols)) {
    ft <- ft |> merge_v(j = merge_cols)
  }

  if (!is.null(double_cols)) {
    ft <- ft |> colformat_double(j = double_cols, digits = 0)
  }

  if (!is.null(pct_cols)) {
    ft <- ft |> colformat_double(j = pct_cols, digits = 2)
  }

  ft |> render_ft(align = align)
}

#' 자산배분 테이블 전용 렌더링
#' @param df 데이터프레임
#' @return htmltools 값
render_allo <- function(df) {
  df |>
    flextable() |>
    theme_vanilla() |>
    merge_v(j = 1:2) |>
    set_table_properties(layout = 'autofit', width = 0.9) |>
    htmltools_value()
}
