library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)
library(ggdist)
library(arrow)
library(markdown)
library(janitor)
library(scales)
library(DT)

# load functions ---------------------------------------------------------------
source("helpers.R")
invisible(lapply(list.files("R/", pattern = ".R$", full.names = TRUE), source))

# set global options -----------------------------------------------------------
enableBookmarking("url")
Cairo()
theme_set(theme_bvq())
clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")
options(ggplot.discrete.fill = clrs,
        ggplot.discrete.colour = clrs,
        shiny.launch.browser = TRUE,
        shiny.usecairo = TRUE)

# load data --------------------------------------------------------------------

# bvq <- readRDS("bvq-app/data/bvq.rds")
# items <- readRDS("bvq-app/data/items.rds")
# participants <- readRDS("bvq-app/data/participants.rds")
# fit <- readRDS("bvq-app/data/fit.rds")
# responses <- open_dataset("bvq-app/data/responses")
# posterior <- open_dataset("bvq-app/data/posterior")
# predictions <- open_dataset("bvq-app/data/predictions")
# predictions_te <- open_dataset("bvq-app/data/predictions_te")
# predictions_id <- open_dataset("bvq-app/data/predictions_id")

items <- readRDS(file.path("data", "items.rds"))
participants <- readRDS(file.path("data", "participants.rds"))
responses <- arrow::open_dataset(file.path("data", "responses"), format = "parquet")
posterior <- arrow::open_dataset(file.path("data", "posterior"), format = "parquet")
predictions <- arrow::open_dataset(file.path("data", "predictions"), format = "parquet")
predictions_te <- arrow::open_dataset(file.path("data", "predictions_te"), format = "parquet")
predictions_id <- arrow::open_dataset(file.path("data", "predictions_id"), format = "parquet")

# define global variables ------------------------------------------------------
te_labels_df <-  responses |> 
    distinct(te, language) |> 
    inner_join(select(items, te, language, label)) |> 
    collect() |> 
    tidyr::pivot_wider(names_from = language,
                values_from = label,
                values_fn = first,
                names_repair = make_clean_names) |> 
    mutate(label = paste0(catalan, " / ", spanish)) |> 
    tidyr::drop_na(catalan, spanish) |> 
    select(te, label) |> 
    arrange(label)

te_labels <- te_labels_df$te
names(te_labels) <- te_labels_df$label

