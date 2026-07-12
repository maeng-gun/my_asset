# =============================================================================
# mod_category — 구분항목 관리 모듈
# =============================================================================

mod_category_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "구분항목 관리",
    box(
      id = ns('list_box'), width = 12, status = 'info',
      solidHeader = TRUE, title = "입력사항", collapsible = FALSE,
      fluidRow(
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('ass_account_list'))),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('pen_account_list'))),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('ass_cur_list'))),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('ass_class_list'))),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('ass_class1_list'))),
        column(width = 2, class = "col-12 col-md-4 col-lg-2",
               uiOutput(ns('ass_class2_list')))
      )
    )
  )
}

mod_category_server <- function(id, ma, sk_c, ctg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ass_ctg <- list('ass_account', 'pen_account', 'ass_cur',
                    'ass_class', 'ass_class1', 'ass_class2')
    ctg_kor <- list('투자계좌', '연금계좌', '통화',
                    '자산군', '세부자산군', '세부자산군2')

    purrr::map2(ass_ctg, ctg_kor, function(i, j) {
      output[[paste0(i, '_list')]] <<- renderUI({
        tagList(
          textInput(ns(paste0('add_', i)), label = j, value = ""),
          selectInput(ns(paste0('select_', i)), NULL, ctg()[[i]]),
          div(
            actionButton(ns(glue("add_{i}_btn")), label = "추가",
                         width = '45%', status = "info"),
            actionButton(ns(glue("del_{i}_btn")), label = "삭제",
                         width = '45%', status = "primary"),
            align = 'center'
          )
        )
      })

      observeEvent(input[[glue("add_{i}_btn")]], {
        new_category_item <- tibble(
          key = i,
          value = input[[glue("add_{i}")]]
        )
        ma$upsert(df = new_category_item,
                  name = 'categories',
                  cols = c('key', 'value'))
        sk_c(!sk_c())
      })

      observeEvent(input[[glue("del_{i}_btn")]], {
        item_to_delete <- tibble(
          key = i,
          value = input[[glue("select_{i}")]]
        )
        dbxDelete(ma$con, 'categories', item_to_delete)
        sk_c(!sk_c())
      })
    })
  })
}
