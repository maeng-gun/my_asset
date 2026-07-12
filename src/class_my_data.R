# =============================================================================
# MyData R6 클래스 — Supabase PostgreSQL 데이터 접근 계층 (베이스 클래스)
# =============================================================================
# pool 패키지를 사용한 DB 커넥션 풀 관리 및 기본 CRUD 메서드 제공
# AutoInvest, MyAssets 클래스의 부모 클래스
# =============================================================================

MyData <- R6Class(

  classname = 'MyData',
  public = list(

    con = NULL, config = NULL,

    ## 1. 속성 초기화 ====
    initialize = function(pw) {

      cfg <- yaml::read_yaml(file = 'ccc.yaml',
                             readLines.warn = FALSE)

      self$con <- dbPool(
        drv = RPostgres::Postgres(),
        host = cfg$c,
        port = 5432,
        dbname = "postgres",
        user = cfg$a,
        password = pw
      )

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
  ),

  private = list(
    ## 7.(메서드) 소멸자: 객체가 삭제될 때 DB 연결 종료 ====
    finalize = function() {
      if (!is.null(self$con)) {
        pool::poolClose(self$con)
      }
    }
  )
)
