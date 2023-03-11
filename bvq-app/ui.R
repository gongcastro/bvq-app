ui <- navbarPage(
    
    # Application title
    titlePanel("BVQ"),
    
    tabPanel(
        "Home", 
        icon = icon("home"),
        includeMarkdown("docs/index.md"),
        br(),
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
        column(width = 2,
               inputPanel(
                   selectInput("items_class",
                               "Class",
                               choices = unique(bvq$pool$class),
                               selected = unique(bvq$pool$class),
                               multiple = TRUE),
                   selectInput("items_semantic_category",
                               "Semantic Category",
                               choices = unique(bvq$pool$semantic_category),
                               selected = unique(bvq$pool$semantic_category),
                               multiple = TRUE, width = "700px"),
                   sliderInput("items_lv",
                               "Levenshtein",
                               min = min(items$lv),
                               max = max(items$lv),
                               step = 0.1,
                               value = range(items$lv))
               )),
        column(width = 8,
               fluidRow(
                   tabsetPanel(
                       type = "tabs",
                       tabPanel("Length", icon = icon("ruler-horizontal"),
                                plotOutput("items_plot_n_phon",
                                           width = 1200, 
                                           height = 300)),
                       tabPanel("Class", icon = icon("language"),
                                plotOutput("items_plot_class",
                                           width = 1200, 
                                           height = 300)),
                       tabPanel("Cognateness", icon = icon("arrows-left-right"),
                                plotOutput("items_plot_lv",
                                           width = 1200, 
                                           height = 300)),
                       tabPanel("Semantic category", icon = icon("dog"),
                                plotOutput("items_plot_semantic_category",
                                           width = 1200, 
                                           height = 300))
                   )),
               fluidRow(DTOutput("items_table"))
        )
    ),
    
    tabPanel(
        "Model", 
        icon = icon("wave-square"),
        column(width = 6,
            withMathJax(),
            h3("Model formula"),
            br(),
            includeMarkdown("docs/model.md")
        ),
        column(width = 6,
            plotOutput("model_draws")
        ),
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
        column(
            width = 9,
            tabsetPanel(
                type = "tabs",
                tabPanel("Population-level", icon = icon("globe"),
                         sliderInput("trajectories_ndraws",
                                     "Number of posterior draws",
                                     min = 1,
                                     max = nrow(collect(distinct(predictions, .draw))),
                                     value = nrow(collect(distinct(predictions, .draw)))/2,
                                     sep = ",",
                                     step = 1),
                         plotOutput("trajectories_plot")),
                tabPanel("Word-level", icon = icon("font"),
                         br(),
                         column(width = 6,
                                selectInput("trajectories_te_te",
                                            "Word",
                                            choices = te_labels,
                                            selected = 389,
                                            multiple = FALSE)
                         ),
                         column(width = 6,
                                sliderInput("trajectories_te_ndraws",
                                            "Number of posterior draws",
                                            min = 1,
                                            max = nrow(collect(distinct(predictions_te, .draw))),
                                            value = nrow(collect(distinct(predictions_te, .draw)))/2,
                                            sep = ",",
                                            step = 1)
                         ),
                         br(),
                         plotOutput("trajectories_plot_te")),
                tabPanel("Child-level", icon = icon("child"),
                         br(),
                         selectInput("trajectories_id_id",
                                     "Child",
                                     choices = collect(distinct(responses, id))$id,
                                     selected = collect(distinct(responses, id))$id[1],
                                     multiple = FALSE),
                         sliderInput("trajectories_id_ndraws",
                                     "Number of posterior draws",
                                     min = 1,
                                     max = nrow(collect(distinct(predictions_id, .draw))),
                                     value = nrow(collect(distinct(predictions_id, .draw)))/2,
                                     sep = ",",
                                     step = 1),
                         br(),
                         plotOutput("trajectories_plot_id")
                )
            )
        )
    )
)
