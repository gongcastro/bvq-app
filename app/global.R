<<<<<<< HEAD:app/global.R
library(shiny)
library(bvqdev)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(Cairo)
library(arrow)

# load functions
source("utils.R")
theme_set(theme_bvq())
clrs <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40")

options(ggplot.discrete.fill = clrs,
        ggplot.discrete.colour = clrs,
        shiny.launch.browser = TRUE,
        shiny.usecairo = TRUE,
        shiny.maxRequestSize = 500*1024^2)


# load data
bvq <- get_bvq_data(update = FALSE)
fit <- readRDS("data/fit.rds")
epreds <- open_dataset("data/epreds")

te <- bvq$items %>%
    filter(te %in%  pull(distinct(epreds, te), te, as_vector = TRUE)) %>%
    pivot_wider(id_cols = te, names_from = language, values_from = label) %>%
    mutate(label = paste0(Catalan, " - ", Spanish)) %>%
    select(te, label)

epred_choices <- te$te
names(epred_choices) <- te$label
=======
library(shiny)
library(bvqdev)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)
library(brms)
library(tidybayes)
library(arrow)
library(janitor)
library(glue)
library(scales)
library(DT)

# custom ggplot theme
theme_bvq <- function() {
    theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid = element_line(colour = "grey",
                                        linetype = "dotted"))
}

if (!interactive()) {
    Cairo()
}

theme_set(theme_bvq())

clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

# set global options
options(ggplot.discrete.fill = clrs,
        ggplot.discrete.colour = clrs,
        shiny.launch.browser = TRUE,
        shiny.usecairo = TRUE)

# load data
# bvq <- readRDS("bvq-app/data/bvq.rds")
# items <- readRDS("bvq-app/data/items.rds")
# participants <- readRDS("bvq-app/data/participants.rds")
# fit <- readRDS("bvq-app/data/fit.rds")
# responses <- open_dataset("bvq-app/data/responses")
# posterior <- open_dataset("bvq-app/data/posterior")
# predictions <- open_dataset("bvq-app/data/predictions")
# predictions_te <- open_dataset("bvq-app/data/predictions_te")
# predictions_id <- open_dataset("bvq-app/data/predictions_id")

bvq <- readRDS("data/bvq.rds")
items <- readRDS("data/items.rds")
participants <- readRDS("data/participants.rds")

responses <- open_dataset("data/responses")
posterior <- open_dataset("data/posterior")
predictions <- open_dataset("data/predictions")
predictions_te <- open_dataset("data/predictions_te")
predictions_id <- open_dataset("data/predictions_id")

te_labels_df <-  responses |> 
    distinct(te, language) |> 
    inner_join(select(bvq$pool, te, language, label)) |> 
    collect() |> 
    pivot_wider(names_from = language,
                values_from = label,
                values_fn = first,
                names_repair = make_clean_names) |> 
    mutate(label = paste0(catalan, " / ", spanish)) |> 
    select(te, label) |> 
    arrange(label)

te_labels <- te_labels_df$te
names(te_labels) <- te_labels_df$label

>>>>>>> 5652c823239e2088d70ff0f15ed1f9dabbedad9c:bvq-app/global.R
