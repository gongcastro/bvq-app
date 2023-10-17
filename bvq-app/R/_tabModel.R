tabModel <- function() {
    tabPanel(
        "Model", 
        icon = icon("wave-square"),
        column(width = 6,
               withMathJax(),
               br(),
               includeMarkdown("docs/model.md")
        ),
        column(width = 6,
               plotOutput("model_draws")
        )
    )
}