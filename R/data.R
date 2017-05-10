#' Testdata for RCC shiny apps
#'
#' Data set used in the examples given in the function rccShiny.
#'
#' @format A data frame with 20000 rows and 9 variables:
#' \describe{
#'   \item{sjukhus}{hospital names (character).}
#'   \item{landsting}{county codes for hospital (numeric).}
#'   \item{region}{region codes for hospital (numeric).}
#'   \item{period}{year of diagnosis.}
#'   \item{age}{patients age at diagnosis.}
#'   \item{outcome1}{dichotomous outcome (logical).}
#'   \item{outcome2}{continuous outcome (numeric).}
#'   \item{outcome3}{categorical outcome with 5 levels (factor).}
#'   \item{stage}{patients stage at diagnosis.}
#' }
"rccShinyData"
