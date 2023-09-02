<<<<<<< HEAD:app/ui.R
# user intercept
ui <- navbarPage(
    # Application title
    titlePanel(title = "", windowTitle = "bvq"),

    tabPanel(
        "Home",
        icon = icon("home"),
        br(),
        column(
            width = 5,
            h1("Barcelona Vocabulary Questionnaire (BVQ)"),
            p(
                "This app shows the Barcelona Vocabulary Questionnaire (BVQ) database,
          with vocabulary data from monolingual and bilingual children living in the
          Metropolitan Area of Barcelona (Spain)."
            ),
            img(src = "logo_bvq.png", width = "200px")
        ),
        column(width = 5),
        column(
            width = 2,
            verticalLayout(
                wellPanel(
                    h3("Universitat Pompeu Fabra"),
                    img(src = "logo_upf.png", align = "right", width = "50px")
                ),
                wellPanel(
                    h3("GitHub"),
                    img(src = "logo_github.png", align = "right", width = "50px")
                ),
                wellPanel(
                    h3("OSF"),
                    img(src = "logo_osf.png", align = "right", width = "50px")
                ),
                wellPanel(
                    h3("Laboratori de Recerca en Infància"),
                    img(src = "logo_babylab.png", align = "right", width = "50px")
                )
            )
        )
    ),
    tabPanel(
        "Participants",
        icon = icon("child"),
        inputPanel(
            sliderInput(
                "participants_age",
                "Age (months)",
                min = 0,
                max = ceiling(max(bvq$logs$age)),
                value = c(0, ceiling(max(bvq$logs$age)))
            ),
            dateRangeInput(
                "participants_time_stamp",
                "Response date",
                min = min(bvq$logs$time_stamp),
                max = as.Date(Sys.Date()),
                start = min(bvq$logs$time_stamp),
                end = max(bvq$logs$time_stamp)
            ),
            selectInput(
                "participants_lp",
                "Language profile",
                choices = unique(bvq$logs$lp),
                selected = unique(bvq$logs$lp),
                multiple = TRUE
            )
        ),
        mainPanel(column(
            width = 6,
            plotOutput("responses_age_plot")
        ),
        column(
            width = 6,
            plotOutput("responses_date_plot")
        ))
    ),


    tabPanel(
        "Items",
        icon = icon("list"),
        column(
            width = 2,
            inputPanel(
                selectInput(
                    "items_language",
                    "Language",
                    choices = unique(bvq$items$language),
                    selected = unique(bvq$items$language),
                    multiple = TRUE
                ),
                selectInput(
                    "items_class",
                    "Class",
                    choices = unique(bvq$items$class),
                    selected = unique(bvq$items$class),
                    multiple = TRUE
                ),
                selectInput(
                    "items_semantic_category",
                    "Semantic category",
                    choices = unique(bvq$items$semantic_category),
                    selected = unique(bvq$items$semantic_category),
                    multiple = TRUE
                ),
                sliderInput(
                    "items_n_lemmas",
                    "Max. lemmas",
                    min = 1,
                    max = max(bvq$items$n_lemmas),
                    value = 1
                ),
                checkboxInput("items_is_multiword",
                              "Allow multi-word items?",
                              value = FALSE),
                sliderInput(
                    "items_n_phon",
                    "Number of phonemes",
                    min = min(bvq$items$n_phon),
                    max = max(bvq$items$n_phon),
                    value = range(bvq$items$n_phon)
                )
            )
        ),
        column(width = 10,
               mainPanel(column(
                   width = 12,
                   tableOutput("items_table")
               )))
    ),

    tabPanel(
        "Estimations",
        icon = icon("bolt"),
        br(),
        fluidRow(
            column(
                width = 3,
                inputPanel(
                    selectizeInput("epreds_te",
                                   "Word",
                                   choices = epred_choices,
                                   selected = epred_choices[2]),
                    selectInput(
                        "epreds_exposure",
                        "Exposure Index",
                        choices = unique(collect(epreds)$exposure_std),
                        selected = 0
                    )
                ),
                wellPanel(
                    fluidRow(column(width = 8, h3("Model details")),
                             column(
                                 width = 4, img(src = "logo_stan.png", width = "60px")
                             )),
                    br(),
                    fluidRow(
                        p("Fitted in Stan using brms with cmdstan backend"),
                        br(),
                        p(paste0("brms version (", fit$version$brms, ")")),
                        p(paste0(
                            "cmdstanr version (", fit$version$cmdstanr, ")"
                        )),
                        p(paste0(
                            "cmdstan version (", fit$version$cmdstan, ")"
                        )),
                        p(paste0(
                            "Last updated: ", file.info("./data/fit.rds")$mtime
                        ))
                    )
                )
            ),
            column(width = 9,
                   mainPanel(
                       plotOutput("epreds_plot", height = 600, width = 1000)
                   ))
        ),
        fluidRow(column(width = 12,
                        wellPanel(
                            br(),
                            p(
                                paste(
                                    "Model formula:",
                                    as.character(fit$formula$formula)[2],
                                    as.character(fit$formula$formula)[1],
                                    as.character(fit$formula$formula)[3]
                                )
                            ),
                            p(paste0("Nº chains: ", dim(fit$fit)[2])),
                            p(paste0(
                                "Nº iterations: ",
                                dim(fit$fit)[1],
                                " (+",
                                dim(fit$fit)[1],
                                " warm-up)"
                            ))
                        )))
    )
)
=======
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
                        max = ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10,
                        step = 1,
                        value = c(0, ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10),
                        sep = ",",
                        dragRange = TRUE),
            dateRangeInput("participants_time_stamp",
                           "Response date",
                           min = min(participants$time_stamp),
                           max = as.Date(Sys.Date()),
                           start = min(participants$time_stamp),
                           end = max(participants$time_stamp)),
            selectInput("participants_lp",
                        "Language profile",
                        choices = unique(participants$lp),
                        selected = unique(participants$lp),
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
                               choices = unique(items$class),
                               selected = unique(items$class),
                               multiple = TRUE),
                   selectInput("items_semantic_category",
                               "Semantic Category",
                               choices = unique(items$semantic_category),
                               selected = unique(items$semantic_category),
                               multiple = TRUE, width = "700px"),
                   sliderInput("items_n_syll",
                               "Syllables",
                               min = min(items$n_syll),
                               max = max(items$n_syll),
                               step = 1,
                               ticks = FALSE,
                               value = range(items$n_syll)),
                   sliderInput("items_n_phon",
                               "Phonemes",
                               min = min(items$n_phon),
                               max = max(items$n_phon),
                               step = 1,
                               value = range(items$n_phon)),
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
                       tabPanel("Syllables", icon = icon("ruler-horizontal"),
                                plotOutput("items_plot_n_syll",
                                           width = 1200, 
                                           height = 300)),
                       tabPanel("Phonemes", icon = icon("ruler-horizontal"),
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
                               max = ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10,
                               step = 1,
                               value = c(0, ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10),
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
>>>>>>> 5652c823239e2088d70ff0f15ed1f9dabbedad9c:bvq-app/ui.R
