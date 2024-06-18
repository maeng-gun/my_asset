library(shiny)
library(dplyr)
library(purrr)
import::from(shinyjs, useShinyjs, extendShinyjs, js)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("로또 생성기"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Show a plot of the generated distribution
        sidebarPanel = NULL,
        mainPanel(
            useShinyjs(),
            extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", 
                          functions = c("closeWindow")),
            fluidRow(
                actionButton("btn",
                             "생성하기"),
                tableOutput("lotto_out"),
                actionButton("close_win",
                             "창닫기")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$lotto_out <- renderTable({
        input$btn
        map_dfc(1:5, ~tibble(sort(sample(1:45, 6, replace=F)))) %>% 
            setNames(c(1:5))
    })
    
    observeEvent(input$close_win,{
        js$closeWindow()
        stopApp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
