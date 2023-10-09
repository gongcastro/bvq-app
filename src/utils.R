# run app
run_app <- function(...) shiny::runApp("bvq-app", launch.browser = TRUE, ...)

# update BVQ data
get_bvq_data <- function(...) {
    p <- bvq_participants()
    r <- bvq_responses(p, ...)
    l <- bvq_logs(p, r)
    i <- bvq::pool %>%
        select(item, language, te, label, xsampa, n_lemmas, is_multiword,
               semantic_category, class) %>%
        mutate(n_phon = nchar(gsub("\\.", "", xsampa))) %>%
        drop_na(xsampa)

    bvq_data <- list(participants = p,
                     responses = r,
                     logs = l,
                     items = i)
    return(bvq_data)
}

# custom ggplot theme
theme_bvq <- function() {
    theme_minimal() +
        theme(panel.grid.major = element_blank(),
              axis.line = element_line(linewidth = 0.65, colour = "black"),
              axis.text = element_text(size = 9),
              axis.title = element_text(face = "bold"),
              panel.grid = element_blank(),
              strip.text = element_text(size = 9))
}

# get posterior expectations
get_epreds <- function(x) {
    stopifnot(is.brmsfit(x))
    distinct_data <- distinct(x[["data"]], te, n_phon_std, lv_std)
    nd <- expand(distinct_data, te, lv_std, age_std = -1:1, exposure_std = -1:1)

    epreds <- epred_draws(x, nd, ndraws = 25, seed = 888, re_formula = ~1|te)

    return(epreds)
}

