if ("renv" %in% row.names(utils::installed.packages())) {
    source("renv/activate.R")
    source("src/utils.R")
}

if ("targets" %in% row.names(utils::installed.packages())) {
    library(targets)
}

options(shiny.launch.browser = TRUE,
        shiny.usecairo = TRUE,
        shiny.maxRequestSize = 500*1024^2,
        crayon.enabled = TRUE,
        repos = c(CRAN = "https://cloud.r-project.org",
                  gongcastro = "https://gongcastro.r-universe.dev",
                  stan = "https://mc-stan.org/r-packages/"),
        renv.cache.linkable = TRUE,
        renv.config.cache.symlinks = TRUE)
