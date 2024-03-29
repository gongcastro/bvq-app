#' Get participant-level data
#'
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param longitudinal Should longitudinal data be included? If `"all"` (default), all responses (including repeated measures) are included. If "no", participants with more than one responses to the questionnaire (regardless of the version) are excluded. If "first", only the first response of each participant is included. If "last", only the last response of each participant is included. If "only", only responses with repeated measures are included.
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param lp Character vector indicating the language profile (LP) of the participants that will be included in the resulting dataset. In takes "Monolingual", "Bilingual", and/or "Other" as values.
#' @param other_threshold Numeric value between 0 and 1 indicating the minimum exposure to a language other than Catalan or Spanish that a participant need to be exposed to to be excluded.
get_participants <- function(bvq_data,
                             longitudinal = "all",
                             age = c(10, 36),
                             lp = c("Monolingual", "Bilingual"),
                             other_threshold = 0.1) {
    
    participants <- bvq_data$logs |>
        dplyr::filter(lp %in% .env$lp,
                      between(age, .env$age[1], .env$age[2]),
                      sum(doe_catalan + doe_spanish) > .env$other_threshold,
                      # exclude participants (duplicated entry)
                      # make sure that degrees of exposure are between 0 and 1
                      between(doe_spanish, 0, 1),
                      between(doe_catalan, 0, 1),
                      between(doe_others, 0, 1)) |>
        mutate(time = as.integer(time)) |> 
        # see ?bvq::get_longitudinal
        get_longitudinal(longitudinal = longitudinal) |>
        select(child_id, response_id, time, date_finished, age, lp,
               doe_catalan, doe_spanish, edu_parent) |> 
        arrange(child_id)
    
    # export data
    save_files(participants, folder = "data")
    
    saveRDS(participants, "bvq-app/data/participants.rds")
    
    return(participants)
}
