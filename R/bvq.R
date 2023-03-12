#' Get BVQ data
#' @param ... Arguments to be passed to [bvqdev::bvq_responses()]
#' @returns A named list of data frames containing questionnaire responses, participant data, and item data from BVQ
get_bvq <- function(...) {
    
    bvq_connect()
    
    # get participant data
    p <- bvq_participants()
    
    # get questionnaire responses
    r <- bvq_responses(p)
    
    # merge participant data with questionnaire responses
    edu_levels <- c("noeducation" = "No education",
                    "primary" = "Primary",
                    "secondary" = "Secondary",
                    "complementary" = "Complementary",
                    "vocational" = "Vocational",
                    "university" = "University")
    
    l <- bvq_logs(p, r) |>
        mutate(id_bvq = id) |> 
        mutate(
            across(starts_with("edu_"), ~ as.numeric(
                factor(.x, levels = names(edu_levels), ordered = TRUE)
            )),
            # get maximum educational attainment of parents
            edu_parent = apply(cbind(edu_parent1, edu_parent2), 1, max, na.rm = FALSE),
            # recode it as factor
            edu_parent = factor(edu_parent, levels = 1:6, labels = edu_levels)
        ) |>
        select(id, id_bvq, time, time_stamp,
               age, lp, dominance, edu_parent,
               version, completed,
               doe_catalan, doe_spanish, doe_others)
    
    pool <- bvqdev::pool |>
        mutate(ipa = xsampa(xsampa, "ipa"),
               xsampa = str_remove_all(xsampa, '\\\"|\\.'),
               syll = strsplit(ipa, 'ˈ|\\.') |> 
                   map(\(x) x[x != ""]))
    
    v <- bvq_vocabulary(p, r)
    
    n <- bvq_norms(p, r)
    
    # get list of all relevant datasets
    bvq_data <- list(participants = p,
                     responses = r,
                     logs = l,
                     pool = pool,
                     vocabulary = v,
                     norms = n)
    
    attr(bvq_data, "updated") <- Sys.time()
    
    saveRDS(bvq_data, "bvq-app/data/bvq.rds")
    
    return(bvq_data)
}
