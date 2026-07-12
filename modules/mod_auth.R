# =============================================================================
# mod_auth — 인증 및 DB 연결 모듈
# =============================================================================
# 로컬 환경: .Renviron의 SUPABASE_PW로 자동 연결
# 배포 환경: showModal() 비밀번호 입력 후 연결
# =============================================================================

#' 인증 모듈 UI
#' @param id 모듈 네임스페이스 ID
mod_auth_ui <- function(id) {
  ns <- NS(id)
  # 모달 방식이므로 별도 UI 요소 없음
  tagList()
}

#' 인증 모듈 서버
#' @param id 모듈 네임스페이스 ID
#' @param is_local 로컬 환경 여부 (logical)
#' @return reactiveValues(authenticated, pg_pass)
mod_auth_server <- function(id, is_local) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 반환용 반응성 값
    auth_rv <- reactiveValues(authenticated = FALSE, pg_pass = NULL)

    # --- 로컬 환경: .Renviron에서 비밀번호 자동 읽기 ---
    if (is_local) {
      local_pw <- Sys.getenv("SUPABASE_PW")

      if (nchar(local_pw) > 0) {
        # .Renviron에 비밀번호가 존재하면 자동 연결 시도
        cfg <- yaml::read_yaml(file = 'ccc.yaml', readLines.warn = FALSE)
        tryCatch({
          temp_con <- DBI::dbConnect(
            RPostgres::Postgres(),
            host     = cfg$c,
            port     = 5432,
            dbname   = "postgres",
            user     = cfg$a,
            password = local_pw
          )
          DBI::dbDisconnect(temp_con)
          auth_rv$pg_pass       <- local_pw
          auth_rv$authenticated <- TRUE
        }, error = function(e) {
          # 자동 연결 실패 시 모달 표시로 폴백
          show_login_modal(session, ns)
        })
      } else {
        # .Renviron에 비밀번호가 없으면 모달 표시
        show_login_modal(session, ns)
      }
    } else {
      # --- 배포 환경: 항상 비밀번호 모달 표시 ---
      show_login_modal(session, ns)
    }

    # --- 로그인 버튼 이벤트 ---
    observeEvent(input$login_button, {
      req(input$db_password_input)

      w_modal <- Waiter$new(
        id = ns("login_button"),
        html = tagList(spin_loader(), "연결 시도 중..."),
        color = transparent(.5)
      )
      w_modal$show()

      cfg <- yaml::read_yaml(file = 'ccc.yaml', readLines.warn = FALSE)
      temp_con <- NULL

      tryCatch({
        temp_con <- DBI::dbConnect(
          RPostgres::Postgres(),
          host     = cfg$c,
          port     = 5432,
          dbname   = "postgres",
          user     = cfg$a,
          password = input$db_password_input
        )

        # 연결 성공
        DBI::dbDisconnect(temp_con)
        auth_rv$pg_pass       <- input$db_password_input
        auth_rv$authenticated <- TRUE

        w_modal$hide()
        removeModal()

      }, error = function(e) {
        # 연결 실패
        w_modal$hide()
        output$login_error_modal <- renderUI({
          p("비밀번호가 올바르지 않거나 DB에 연결할 수 없습니다.",
            style = "color: red; margin-top: 10px;")
        })
      })
    })

    return(auth_rv)
  })
}


# --- 헬퍼: 비밀번호 입력 모달 표시 ---
show_login_modal <- function(session, ns) {
  login_modal <- modalDialog(
    title = "보안 접속",
    fluidRow(
      column(12,
             passwordInput(ns("db_password_input"),
                           "데이터베이스 비밀번호를 입력하세요:",
                           width = "100%"),
             br(),
             actionButton(ns("login_button"), "접속",
                          status = "info", width = "100%"),
             br(),
             uiOutput(ns("login_error_modal"))
      )
    ),
    footer = NULL,
    easyClose = FALSE
  )

  showModal(login_modal, session = session)

  # Enter 키로 접속 버튼 클릭 (네임스페이스 적용)
  js$enterToClick(
    inputId  = ns("db_password_input"),
    buttonId = ns("login_button")
  )
}
