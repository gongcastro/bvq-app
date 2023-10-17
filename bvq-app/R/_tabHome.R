tabHome <- function() {
    tabPanel(
        "Home", 
        icon = icon("home"),
        br(),
        h1("Barcelona Vocabulary Questionnaire (BVQ)"),
        br(),
        column(width = 8,
               includeMarkdown("docs/index.md")
        ),
        column(width = 1),
        column(width = 2,
               fluidRow(
                   tags$a(
                       href = "https://github.com/gongcastro/bvq-app", 
                       tags$img(src = "logo.png", 
                                width = "60%",
                                float = "right")
                   )),
               br(),
               fluidRow(
                   tags$a(
                       href = "https://www.upf.edu/web/cbc", 
                       tags$img(src = "cbc.png", 
                                width = "80%",
                                float = "right")
                   )),
               br(),
               fluidRow(
                   tags$a(
                       href = "https://www.upf.edu/en/", 
                       tags$img(src = "upf.png", 
                                width = "80%",
                                float = "right")
                   )),
               br(),
               fluidRow(
                   tags$a(
                       href = "https://www.ciencia.gob.es/en/", 
                       tags$img(src = "mineco.png", 
                                width = "80%",
                                float = "right")
                   ))
        )
    )
}
