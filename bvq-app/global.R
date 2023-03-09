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

Cairo()

theme_set(theme_bvq())

clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

# set global options
options(ggplot.discrete.fill = clrs,
        ggplot.discrete.colour = clrs,
        shiny.launch.browser = TRUE,
        shiny.usecairo = TRUE)

# load data
bvq <- readRDS("data/bvq.rds")
items <- readRDS("data/items.rds")
participants <- readRDS("data/participants.rds")
fit <- readRDS("data/fit.rds")
posterior <- readRDS("data/posterior.rds")
predictions <- readRDS("data/predictions.rds")