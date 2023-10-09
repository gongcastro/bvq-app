
#' Prepare data for analyses
#'
#' @param bvq_data A named list resulting from calling [bvq::get_bvq]
#' @param items A data frame resulting from calling [bvq::get_items]
#' @param participants A data frame resulting from calling [bvq::bvq_participants]
get_responses <- function(bvq_data, items, participants) {

    # merge all datasets
    responses_tmp <- bvq_data$responses |>
        mutate(time = as.integer(time),
               language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
               item = stringr::str_remove(item, "cat_|spa_")) |>
        # drop missing responses
        # by default datasets are expanded so that every participant has rows for all items,
        # even for those that were not included in their version of the questionnaire
        drop_na(response) |>
        inner_join(distinct(participants, child_id),
                   by = join_by(child_id)) |>
        select(child_id, time, response_id, language, item, response) |>
        arrange(child_id, time)

    responses <- responses_tmp |>
        inner_join(items, by = join_by(item, language)) |>
        inner_join(participants,
                   by = join_by(child_id, time)) |>
        mutate(
            # code responses as factor
            response = factor(response,
                              levels = c(1, 2, 3),
                              labels = c("No",
                                         "Understands",
                                         "Understands and Says"),
                              ordered = TRUE),
            # does should have the value of the corresponding language
            doe = ifelse(language=="Catalan", doe_catalan, doe_spanish),
            # standardise numeric predictors
            across(c(lv, age, doe),
                   \(x) scale(x)[, 1],
                   .names = "{.col}_std"),
            dominance = ifelse(doe_catalan >= doe_spanish, "Catalan", "Spanish"),
            dominance = ifelse(language==dominance, "L1", "L2"),
            across(c(lp, dominance), as.factor)
        ) |>
        # get only relevant variables
        select(child_id, time, age, age_std, te, language, item, response,
               lv, lv_std, lp, dominance) |>
        # reorder rows
        arrange(child_id, te, language)

    contrasts(responses$lp) <- c(0.5, -0.5)
    contrasts(responses$dominance) <- c(0.5, -0.5)

    # export data
    save_files(responses, folder = "data")
    arrow::write_dataset(responses,
                         path = "bvq-app/data/responses",
                         format = "parquet",
                         partitioning = c("child_id"))

    return(responses)

}
