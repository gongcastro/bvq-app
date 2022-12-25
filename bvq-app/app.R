library(shiny)
library(bvqdev)
library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(janitor)

# load functions
source("./utils.R")
theme_set(theme_bvq())
clrs <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40")

options(ggplot.discrete.fill = clrs,
        ggplot.discrete.colour = clrs)

# load data
bvq <- get_bvq_data(update = FALSE)
fit <- readRDS("./bvq-app/data/fit.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    titlePanel("BVQ"),

    tabPanel(
        "Home", icon = icon("home"),
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
                        max = ceiling(max(bvq$logs$age)),
                        value = c(0, ceiling(max(bvq$logs$age)))),
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
            column(width = 6,
                   plotOutput("responses_age_plot")),
            column(width = 6,
                   plotOutput("responses_date_plot"))
        )
    ),


    tabPanel(
        "Items", icon = icon("list"),
        inputPanel(
            selectInput("items_language",
                        "Language",
                        choices = unique(bvq$items$language),
                        selected = unique(bvq$items$language),
                        multiple = TRUE),
            selectInput("items_class",
                        "Class",
                        choices = unique(bvq$items$class),
                        selected = unique(bvq$items$class),
                        multiple = TRUE),
            selectInput("items_semantic_category",
                        "Semantic category",
                        choices = unique(bvq$items$semantic_category),
                        selected = unique(bvq$items$semantic_category),
                        multiple = TRUE),
            sliderInput("items_n_lemmas",
                        "Max. lemmas",
                        min = 1,
                        max = max(bvq$items$n_lemmas),
                        value = 1),
            checkboxInput("items_is_multiword",
                          "Allow multi-word items?",
                          value = FALSE),
            sliderInput("items_n_phon",
                        "Number of phonemes",
                        min = min(bvq$items$n_phon),
                        max = max(bvq$items$n_phon),
                        value = range(bvq$items$n_phon))
        ),

        mainPanel(
            column(width = 12,
                   tableOutput("items_table"))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$responses_age_plot <- renderPlot({
        bvq$logs %>%
            filter(between(age, input$participants_age[1], input$participants_age[2]),
                   between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                   lp %in% input$participants_lp) %>%
            mutate(age = floor(age)) %>%
            count(age) %>%
            ggplot(aes(age, n)) +
            geom_col(fill = clrs[1], colour = "white") +
            geom_text(aes(label = n), size = 3, position = position_nudge(y = 1))
    })

    output$responses_date_plot <- renderPlot({
        bvq$logs %>%
            filter(between(age, input$participants_age[1], input$participants_age[2]),
                   between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                   lp %in% input$participants_lp) %>%
            count(time_stamp, lp) %>%
            group_by(lp) %>%
            mutate(n = cumsum(n)) %>%
            ungroup() %>%
            ggplot(aes(time_stamp, n, colour = lp)) +
            geom_line(aes(group = lp), linewidth = 1)
    })

    output$items_table <- renderTable({
        bvq$items %>%
            distinct(te, language, .keep_all = TRUE) %>%
            # filter(language %in% input$items_language,
            #        class %in% input$items_class,
            #        semantic_category %in% inout$semantic_category,
            #        n_lemmas <= input$items_n_lemmas,
            #        is_multiword == input$items_is_multiword,
            #        between(n_phon, input$items_n_phon[1], input$items_n_phon[2])) %>%
            select(language, te, label, ipa, sampa, class, semantic_category, n_phon, n_lemmas, is_multiword) %>%
            pivot_wider(id_cols = c(te, class, semantic_category),
                        names_from = language,
                        values_from = c(label, ipa, sampa, starts_with("n_")),
                        names_repair = make_clean_names) %>%
            relocate(te, ends_with("_catalan"), ends_with("_spanish"), class, semantic_category)
    }, striped = TRUE, na = "--")
}

# Run the application
shinyApp(ui = ui, server = server)
