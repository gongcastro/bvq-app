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
                               step = 0.1,
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
               fluidRow(
                   column(width = 9,
                          fluidRow(plotOutput("items_plot_n_phon",
                                              width = 800, 
                                              height = 200)),
                          fluidRow(column(width = 6,
                                          plotOutput("items_plot_class",
                                                     width = 300, 
                                                     height = 200)),
                                   column(width = 6,
                                          plotOutput("items_plot_lv",
                                                     width = 300, 
                                                     height = 200)))),
                   
                   column(width = 3,
                          plotOutput("items_plot_semantic_category",
                                     width = 300, 
                                     height = 400))
               ),
               fluidRow(DTOutput("items_table", width = "100%"))
        )
    ),
    
    tabPanel(
        "Model", 
        icon = icon("wave-square"),
        fluidRow(
            plotOutput("model_draws")
        )
    ),
    
    tabPanel(
        "Predictions", 
        icon = icon("arrow-trend-up"),
        column(width = 3,
               inputPanel(
                   sliderInput("predictions_age",
                               "Age (months)",
                               min = 0,
                               max = ceiling(max(bvq$logs$age)) + ceiling(max(bvq$logs$age)) %% 10,
                               step = 1,
                               value = c(0, ceiling(max(bvq$logs$age)) + ceiling(max(bvq$logs$age)) %% 10),
                               sep = ",",
                               dragRange = TRUE),
                   selectInput("predictions_lp",
                               "Language profile",
                               choices = c("Monolingual", "Bilingual"),
                               selected = c("Monolingual", "Bilingual"),
                               multiple = TRUE),
                   selectInput("predictions_dominance",
                               "Dominance",
                               choices = c("L1", "L2"),
                               selected = c("L1", "L2"),
                               multiple = TRUE),
                   selectInput("predictions_category",
                               "Modality",
                               choices = c("Understands", "Understands and Says"),
                               selected = c("Understands", "Understands and Says"),
                               multiple = TRUE),
                   selectInput("predictions_summary",
                               "Summary",
                               choices = c("none", "mean", "median"),
                               selected = "mean",
                               multiple = FALSE),
                   checkboxInput("predictions_uncertainty",
                                 "Show uncertainty?",
                                 value = TRUE)
               )),
        column(width = 9,
               plotOutput("trajectories_plot")
        )
    )
)
