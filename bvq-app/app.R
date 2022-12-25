#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bvqdev)
library(dplyr)
library(ggplot2)

source("bvq-app/utils.R")

bvq_data <- get_bvq(update = FALSE, verbose = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Barcelona Vocabulary Questionnaire (BVQ)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "age",
                "Age (months)",
                start = min(bvq_data$logs$time_stamp, na.rm = TRUE),
                end = max(bvq_data$logs$time_stamp, na.rm = TRUE),
                min = min(bvq_data$logs$time_stamp, na.rm = TRUE),
                max = max(bvq_data$logs$time_stamp, na.rm = TRUE)
            ),
            selectInput(
                "longitudinal",
                "Logitudinal data",
                selected = "all",
                choices = c("all", "only", "first", "last")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("time_stamp_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$time_stamp_plot <- renderPlot({
        
        bvq_data$logs %>% 
            get_longitudinal(input$longitudinal) %>% 
            filter(between(time_stamp, input$age[1], input$age[2])) %>% 
            count(time_stamp) %>% 
            mutate(cum_sum = cumsum(n)) %>% 
            ggplot(aes(time_stamp, cum_sum)) +
            geom_line(aes(group = 1), size = 0.75)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
