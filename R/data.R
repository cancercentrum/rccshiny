#' Testdata for RCC shiny apps
#'
#' Data set used in the examples given in the function rccShiny.
#'
#' @format A data frame with 20000 rows and 9 variables:
#' \describe{
#'   \item{sjukhus}{A character vector contining hospitals names.}
#'   \item{landsting}{A numeric vector containg codes for the 'landsting' for the respective hospitals.}
#'   \item{region}{A numeric vector containg codes (1-6) for the region for the respective hospitals.}
#'   \item{period}{Year of diagnosis (2012-2016).}
#'   \item{age}{Patients age at diagnosis.}
#'   \item{outcome1}{Dichotomous outcome (logical: TRUE/FALSE).}
#'   \item{outcome2}{Continuous outcome (numeric).}
#'   \item{outcome3}{Categorical outcome with 5 levels (factor).}
#'   \item{stage}{Patients stage at diagnosis.}
#' }
"rccShinyData"
