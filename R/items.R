#' Get item data
#'
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param class A character vector indicating the word classes to be included in the resulting dataset. Takes "Adjective", "Noun" and/or "Verb" as values.
get_items <- function(bvq_data, .class = "Noun") {
    
    # check arguments
    classes_available <- c("Noun", "Verb", "Adjective")
    if (!all(.class %in% classes_available)) {
        cli_abort("class must be one of {classes_available}")
    }
    
    # find TEs that have one word-form in each language
    duplicated_te <- bvq_data$pool$te[duplicated(bvq_data$pool$te)]
    
    pool_tmp <- bvq_data$pool |>
        # drop items with missing observations in these variables
        tidyr::drop_na(ipa) |>
        dplyr::filter(include,
                      class %in% .class) |>
        add_count(te, name = "n_te") |> # get only items with one translation in each language
        dplyr::filter(n_te == 2) |>
        distinct(language, te, item, label, xsampa, ipa, syll, 
                 class, version, semantic_category)
    
    # compute Levenshtein distances
    lv_df <- pool_tmp |> 
        tidyr::pivot_wider(names_from = language, 
                           values_from = xsampa,
                           id_cols = te,
                           names_repair = janitor::make_clean_names) |> 
        mutate(lv = stringdist::stringsim(catalan, spanish)) |> 
        distinct(te, lv)
    
    # merge datasets
    items <- pool_tmp |>
        left_join(lv_df, by = join_by(te)) |> 
        rename(list = version) |>
        tidyr::drop_na(lv, list) |>
        mutate(n_phon = nchar(xsampa),
               n_syll = purrr::map_int(syll, length),
               item = stringr::str_remove(item, "cat_|spa_")) |>
        select(te, language, item, label, ipa, lv, n_phon, n_syll,
               class, semantic_category) |>
        arrange(te)
    
    # export to data folder
    save_files(items, folder = "data")
    
    saveRDS(items, file.path("bvq-app", "data", "items.rds"))
    
    return(items)
}

