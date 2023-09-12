# bvq-app <a href="gongcastro.github.io/bvqdev"><img src="bvq-app/www/logo.png" align="right" height="139" /></a>
<!-- badges: start -->
[![R-CMD-check](https://github.com/gongcastro/bvqdev/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/gongcastro/bvqdev/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gongcastro/bvqdev/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gongcastro/bvqdev?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![GitHub issues](https://img.shields.io/github/issues/gongcastro/bvqdev)
<!-- badges: end -->


The Barcelona Vocabulary Questionnaire (BVQ) is an online tools developed by researchers from the Laboratori de Recerca en Infància at the Universitat Pompeu Fabra (Barcelona, Spain) aimed at assessing the developing mental lexicon of  bilingual infants and toddlers living in the Metropolitan Area of Barcelona. The BVQ Shiny app provides a visual inteface to the [{bvq}](https://github.com/gongcastro/bvqdev) R package to explore the database. This repository contains the data, documentation, and R scripts needed to run the BVQ Shiny app.

# Running the app

1. Clone or download this repository:

```bash
git clone https://github.com/gongcastro/bvq-app.git
```

2. Open the R console and set this folder as your working directory:

```r
setwd("<path-to-folder/bvq-app")
```

If you are working from RStudio and have opened a project session clicking `bvq-app.proj` this step will not be necessary.

3. Restore the R package dependencies using [{renv}](https://github.com/rstudio/renv/):

```r
# install.packages("renv")
renv::restore()
```

4. Run [{targets}](https://github.com/ropensci/targets) to generate the model inputs and outputs:

```r
targets::tar_make()
```

This function will run all scripts in the right order, and will produce the files needed to run the Shiny App. Please note that, depending of your setup, running the script may take several hours, mainly due to the computational cost of fitting the Bayesian model. Contact us (@gongcastro) to ask to a copy of the RDS file containing the model.

