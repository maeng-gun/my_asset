library(shiny)
library(bs4Dash)

# <User Interface> ####
ui <- dashboardPage(
    
    # 1. 대쉬보드 헤더####
    header = dashboardHeader(
        title = dashboardBrand(
            title = div("포트폴리오 관리", align = 'center'),
            color = "info"
        )
    ),
    # 2. 대쉬보드 사이드바(메뉴)####
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = 'menu_tabs',
            sidebarHeader("주식조회"),
            menuItem(
                text = "경제지표",
                icon = icon(name = "chart-line"),
                tabName = "econ_stat"
            )
        )
    ),
    
    # 3. 대쉬보드 본문####
    body = dashboardBody(
        # useWaitress(),
        # autoWaiter(html=spin_timer(),
        #            color=transparent(0.5)),
        # useSweetAlert(),
        tabItems(
            tabItem(
                tabName = "econ_stat",
                uiOutput('ind_stock')
            )
        ),
    ),
    
    #바닥글
    footer = dashboardFooter(right = "developed by H.M. Choi")
)

server <- function(input, output, session) {
    
    
    #브라우저 창 닫으면 앱 종료
    session$onSessionEnded(
        function(){
            stopApp()
        }
    )
}

shinyApp(ui = ui, server = server)