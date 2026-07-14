# =============================================================================
# MyData R6 클래스 — Supabase PostgreSQL 데이터 접근 계층 (베이스 클래스)
# =============================================================================
# pool 패키지를 사용한 DB 커넥션 풀 관리 및 기본 CRUD 메서드 제공
# AutoInvest, MyAssets 클래스의 부모 클래스
# pool 객체는 외부(app.R)에서 생성하여 주입받고, 종료도 외부에서 관리
# =============================================================================

MyData <- R6Class(

  classname = 'MyData',
  public = list(

    con = NULL, config = NULL,

    ## 1. 속성 초기화 ====
    initialize = function(pool) {

      self$con <- pool
      self$config <- self$read('config')
    },

    ## 2.(메서드) 테이블 추가 ====
    add_table = function(table, name) {
      dbWriteTable(self$con, name, table, overwrite = TRUE)
    },

    ## 3.(메서드) 테이블 읽기 ====
    read = function(name) {
      dbReadTable(self$con, name) %>% tibble()
    },

    ## 4.(메서드) 테이블 읽기(dbplyr 객체) ====
    read_obj = function(name) {
      tbl(self$con, name)
    },

    ## 5.(메서드) 테이블 목록 ====
    table_list = function() {
      dbListTables(self$con)
    },

    ## 6.(메서드) 추가/갱신하기 ====
    upsert = function(df, name, cols) {
      dbxUpsert(conn = self$con, table = name, records = df,
                where_cols = cols)
    }
  )
  # NOTE: finalize (poolClose) 제거됨 — pool 생명주기는 app.R에서 관리
)
