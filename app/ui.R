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
