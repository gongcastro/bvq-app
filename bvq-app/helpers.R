#' Sample draws from posterior (copied from [tidybayes::sample_draws])
sample_draws <- function (data, ndraws, draw = ".draw", seed = NULL) 
{
    .draw = as.name(draw)
    draw_full = data[[draw]]
    if (!is.null(seed)) 
        set.seed(seed)
    draw_sample = sample(unique(draw_full), ndraws)
    filter(data, !!.draw %in% !!draw_sample)
}

#' Custom ggplot2 theme
#' 
theme_bvq <- function() {
    theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid = element_line(colour = "grey",
                                        linetype = "dotted"))
}

#' Transform months to years and months
#'
#' @param x Age in months
#' @param .sep Separator between years and months, ';' by default
months_to_years <- function(x, .sep = ";") {
    glue::glue(floor(x %/% 12),
         floor(x %% 12),
         .sep = .sep)
}

#' Transform months to years and months
#'
#' @param x Time in days
#' @param .sep Separator between months and days, ';' by default
days_to_months <- function(x, .sep = ";") {
    glue::glue(floor(x %/% 30),
         floor(x %% 30),
         .sep = .sep)
}

#' Rescale standardised variable
#'
#' @param x Numeric vector to be rescaled
#' @param mean Numeric value indicating the mean of the original vector
#' @param sd Numeric value indicating the standard deviation of the original vector
rescale_variable <- function(x, mean, sd) {
    (x * sd) + mean
}

#' Cut age variable into age bins
#'
#' @param x Numeric vector with ages in months
cut_age <- function(x) {
    y <- cut(x, breaks = seq(9, 35, 2), labels = seq(10, 34, 2))
    y <- as.integer(as.character(y))
    return(y)
}

