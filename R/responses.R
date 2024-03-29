
#' Prepare data for analyses
#' 
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param items A data frame resulting from calling [get_items()]
#' @param participants A data frame resulting from calling [get_participants()]
get_responses <- function(bvq_data, items, participants) {
    
    # merge all datasets
    responses_tmp <- bvq_data$responses |>
        mutate(time = as.integer(time),
               language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
               item = gsub("cat_|spa_", "", item)) |>
        # drop missing responses
        # by default datasets are expanded so that every participant has rows for all items,
        # even for those that were not included in their version of the questionnaire
        filter(!is.na(response)) |>
        inner_join(distinct(participants, child_id, response_id),
                   by = join_by(child_id, response_id)) |>
        select(child_id, response_id, time, language, item, response) |> 
        arrange(child_id, time)
    
    responses <- responses_tmp |> 
        inner_join(items, by = join_by(language, item),
                   multiple = "first") |> 
        inner_join(participants,
                   by = join_by(child_id, response_id, time)) |>
        mutate(response = factor(response,
                                 levels = c(1, 2, 3),
                                 labels = c("No", 
                                            "Understands",
                                            "Understands and Says"),
                                 ordered = TRUE),
               # does should have the value of the corresponding language
               doe = if_else(language=="Catalan", doe_catalan, doe_spanish),
               # standardise numeric predictors
               across(c(lv, age, doe), 
                      \(x) scale(x)[, 1], 
                      .names = "{.col}_std"),
               dominance = if_else(doe_catalan >= doe_spanish, "Catalan", "Spanish"),
               dominance = if_else(language==dominance, "L1", "L2"),
               across(c(lp, dominance), as.factor)) |>
        # get only relevant variables
        select(child_id, response_id, time, age, age_std, te, language, item, response, 
               lv, lv_std, lp, dominance) |>
        # reorder rows
        arrange(child_id, time, te, language)
    
    contrasts(responses$lp) <- c(0.5, -0.5)
    contrasts(responses$dominance) <- c(0.5, -0.5)
    
    # export data
    save_files(responses, folder = "data")
    arrow::write_dataset(responses, 
                         path = "bvq-app/data/responses",
                         format = "parquet",
                         partitioning = c("response_id"))
    
    return(responses)
    
}
