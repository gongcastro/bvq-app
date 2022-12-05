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
    return(bvq_data)
}

# custom ggplot theme
theme_bvq <- function() {
    theme_minimal() +
        theme(panel.grid.major = element_blank())
}
