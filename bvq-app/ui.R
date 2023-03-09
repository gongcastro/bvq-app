ui <- navbarPage(
    
    # Application title
    titlePanel("BVQ"),
    
    tabPanel(
        "Home", 
        icon = icon("home"),
        br(),
        h1("Barcelona Vocabulary Questionnaire (BVQ)"),
        img(src = "logo.png", align = "right", height = "200px"),
        br(),
        p("This app shows the Barcelona Vocabulary Questionnaire (BVQ) database,
          with vocabulary data from monolingual and bilingual children living in the
          Metropolitan Area of Barcelona (Spain).")
    ),
    
    tabPanel(
        "Participants", icon = icon("child"),
        inputPanel(
            sliderInput("participants_age",
                        "Age (months)",
                        min = 0,
                        max = ceiling(max(bvq$logs$age)) + ceiling(max(bvq$logs$age)) %% 10,
                        step = 1,
                        value = c(0, ceiling(max(bvq$logs$age)) + ceiling(max(bvq$logs$age)) %% 10),
                        sep = ",",
                        dragRange = TRUE),
            dateRangeInput("participants_time_stamp",
                           "Response date",
                           min = min(bvq$logs$time_stamp),
                           max = as.Date(Sys.Date()),
                           start = min(bvq$logs$time_stamp),
                           end = max(bvq$logs$time_stamp)),
            selectInput("participants_lp",
                        "Language profile",
                        choices = unique(bvq$logs$lp),
                        selected = unique(bvq$logs$lp),
                        multiple = TRUE)
        ),
        mainPanel(
            fluidRow(
                column(width = 12, plotOutput("responses_age_plot", width = 800, height = 400))
            ),
            fluidRow(
                column(width = 12,
                       plotOutput("responses_date_plot", width = 800, height = 400))
            )
        )
    ),
    
    
    tabPanel(
        "Items", 
        icon = icon("list"),
        column(width = 3,
               inputPanel(
                   sliderInput("items_lv",
                               "Levenshtein",
                               min = min(items$lv),
                               max = max(items$lv),
                               step = 1,
                               value = range(items$lv)),
                   selectInput("items_class",
                               "Class",
                               choices = unique(bvq$pool$class),
                               selected = unique(bvq$pool$class),
                               multiple = TRUE),
                   selectInput("items_semantic_category",
                               "Semantic Category",
                               choices = unique(bvq$pool$semantic_category),
                               selected = unique(bvq$pool$semantic_category),
                               multiple = TRUE, width = "700px")
               )),
        column(width = 9,
               mainPanel(
                   column(width = 12,
                          DTOutput("items_table", width = "150%"))
               ))
    ),
    
    tabPanel(
        "Model", 
        icon = icon("function"),
        fluidRow()
    )
)
