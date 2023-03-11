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
fit <- readRDS("data/fit.rds")

responses <- open_dataset("data/responses")
posterior <- open_dataset("data/posterior")
predictions <- open_dataset("data/predictions")
predictions_te <- open_dataset("data/predictions_te")
predictions_id <- open_dataset("data/predictions_id")

