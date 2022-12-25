# update BVQ data
get_bvq_data <- function(...) {
    p <- bvq_participants()
    r <- bvq_responses(p, ...)
    l <- bvq_logs(p, r)
    i <- bvqdev::pool %>%
        select(item, language, te, label, ipa, sampa, n_lemmas, is_multiword,
               semantic_category, class) %>%
        mutate(n_phon = nchar(gsub("\\.", "", sampa))) %>%
        drop_na(sampa)

    bvq_data <- list(participants = p,
                     responses = r,
                     logs = l,
                     items = i)
    
    names(bvq_data) <- c("participants", "responses", "logs")
    attr(bvq_data, "updated") <- Sys.time()
    return(bvq_data)
}

# custom ggplot theme
theme_bvq <- function() {
    theme_minimal() +
        theme(panel.grid.major = element_blank())
}

# get posterior expectations
get_epreds <- function(x) {
    stopifnot(is.brmsfit(x))

    distinct_data <- distinct(x[["data"]], te, n_phon_std, lv_std)
    nd <- expand(distinct_data, te, n_phon_std, lv_std, age_std = -1:1, exposure_std = -1:1)

    epreds <- epred_rvars(x, nd, ndraws = 25, seed = 888, re_formula = ~1|te)

    return(epreds)
}
