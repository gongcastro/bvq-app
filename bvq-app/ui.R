ui <- fluidPage(
    navbarPage(
        header = tags$head(
            tags$link(rel = "shortcut icon", type = "image/png", href = "logo.png"),
            tags$title("Browser tab title")
        ),
        titlePanel("", windowTitle = "BVQ"),
        collapsible = TRUE,
        theme = "bvq.css",
        windowTitle = NA,
        fluid = TRUE,
        lang = "en",
        tabHome(),
        tabParticipants(),
        tabItems(),
        tabPredictions(),
        tabModel()
    )
)
