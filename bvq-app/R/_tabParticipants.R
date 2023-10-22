tabParticipants <- function() {
    tabPanel(
        "Participants", icon = icon("child"),
        column(width = 3,
               inputPanel(
                   sliderInput("participants_age",
                               "Age (months)",
                               min = 0,
                               max = ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10,
                               step = 1,
                               value = c(0, ceiling(max(participants$age)) + ceiling(max(participants$age)) %% 10),
                               sep = ",",
                               dragRange = TRUE),
                   dateRangeInput("participants_date_finished",
                                  "Response date",
                                  min = min(participants$date_finished),
                                  max = as.Date(Sys.Date()),
                                  start = min(participants$date_finished),
                                  end = max(participants$date_finished)),
                   selectInput("participants_lp",
                               "Language profile",
                               choices = unique(participants$lp),
                               selected = unique(participants$lp),
                               multiple = TRUE)
               )
        ),
        column(width = 9,
               tabsetPanel(
                   type = "tabs",
                   tabPanel("Age", icon = icon("person-breastfeeding"),
                            plotOutput("responses_age_plot", width = 800, height = 400)),
                   tabPanel("Date", icon = icon("calendar-days"),
                            plotOutput("responses_date_plot", width = 800, height = 400)),
                   tabPanel("Language profile", icon = icon("language"),
                            plotOutput("responses_lp_plot", width = 800, height = 400))
                   
               )
        )
    )
}
