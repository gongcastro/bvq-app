library(targets)
library(tarchetypes)
library(conflicted)
library(cli)

# load R functions -------------------------------------------------------------
function_paths <- list.files("R",
                             pattern = ".R",
                             full.names = TRUE)
invisible(lapply(function_paths, source))

# list package dependencies ----------------------------------------------------
tar_option_set(
    packages = c(
        "arrow",
        "bayesplot",
        "bayestestR",
        "brms",
        "bvqdev",
        "childesr",
        "cli",
        "conflicted",
        "dplyr",
        "ggplot2",
        "glue",
        "ipa",
        "janitor",
        "keyring",
        "knitr",
        "lubridate",
        "marginaleffects",
        "patchwork",
        "purrr",
        "quarto",
        "readxl",
        "rlang",
        "scales",
        "shiny",
        "stringdist",
        "stringr",
        "tibble",
        "tidybayes",
        "tidyr",
        "usethis"
    )
)

resolve_conflicts()

# define global options --------------------------------------------------------
options(
    mc.cores = 4,
    brms.backend = "cmdstanr",
    tidyverse.quiet = TRUE,
    knitr.duplicate.label = "allow",
    loo.cores = 1
)

list(
    ## resolve namespace conflicts ---------------------------------------------
    tar_target(namespace_conficts, resolve_conflicts()),
    
    ## import data -------------------------------------------------------------
    tar_target(bvq_data, get_bvq(update = TRUE, longitudinal = "all")),
    
    # items
    tar_target(items, get_items(bvq_data = bvq_data, childes = childes)),

    # participants
    tar_target(
        participants,
        get_participants(
            bvq_data,
            age = c(10, 36),
            lp = c("Monolingual", "Bilingual"),
            other_threshold = 0.1
        )
    ),

    # responses
    tar_target(responses, get_responses(bvq_data, items, participants)),

    # fit models ---------------------------------------------------------------
    
    # model priors: these priors were set so that they generate data similar to
    # what we expect based on Wordbank data (see manuscript and lab notes)
    tar_target(
        model_prior,
        c(
            prior(normal(-0.25, 0.1), class = "Intercept"),
            prior(normal(1, 0.1), class = "sd", group = "te"),
            prior(normal(1, 0.1), class = "sd", group = "id"),
            prior(lkj(2), class = "cor"),
            prior(normal(1, 0.25), class = "b", coef = "age_std"),
            prior(normal(0, 0.25), class = "b", coef = "lp1"),
            prior(normal(0, 0.25), class = "b", coef = "dominance1"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:lp1"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:dominance1"),
            prior(normal(0, 0.25), class = "b", coef = "lp1:dominance1"),
            prior(normal(0, 0.25), class = "b", coef = "age_std:lp1:dominance1")
        )
    ),
    
    # multilevel model with crossed random effects (participants an items)
    # responses are generated from a categorical distribution:
    #   - https://journals.sagepub.com/doi/full/10.1177/2515245918823199
    #   - https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html
    #   - https://bookdown.org/content/3686/ordinal-predicted-variable.html
    # the probability of each response category is adjusted by age (population-level effect)
    # and adjusted for each individual participant and item (group-level effects)
    
    # only intercepts (category boundaries)
    tar_target(
        model_fit,
        fit_model(
            name = "fit",
            formula = bf(
                response ~ age_std * lp * dominance +
                    (1 + age_std * dominance | id) +
                    (1 + age_std * lp * dominance | te),
                family = cumulative(link = "logit") # cumulative, continuation ratio
            ),
            data = responses,
            prior = model_prior,
            sample_prior = "yes"
        )
    ),
    
    tar_target(
        posterior_draws,
        get_posterior_draws(model_fit, data = responses)
    ),
    
    ## marginal effects --------------------------------------------------------
    tar_target(
        marginal_effects_epreds,
        posterior_predictions(
            model = model_fit,
            responses, 
            age_std = scale(seq(12, 50),
                            mean(responses$age),
                            sd(responses$age)),
            dominance = c("L1", "L2"),
            lp = c("Monolingual", "Bilingual")
        )
    ),
    
 
    # R-hat (aka. Gelman-Rubin statistic)
    tar_target(model_rhats, map(lst(model_fit), rhat)),
    # effective sample size
    tar_target(model_neffs, map(lst(model_fit), neff_ratio)),
    
    # posterior predictive checks
    tar_target(
        model_ppcs,
        {
            yrep_char <- posterior_predict(model_fit, ndraws = 50)
            sapply(data.frame(yrep_char, stringsAsFactors = TRUE), as.integer)
        }
    )
)
