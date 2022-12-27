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
