tabHome <- function() {
    tabPanel(
        "Home", 
        icon = icon("home"),
        includeMarkdown("docs/index.md"),
        br()
    )
}