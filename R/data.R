#' Testdata for RCC shiny apps
#'
#' Data set used in the examples given in the function rccShiny.
#'
#' @format A data frame with 20000 rows and 9 variables:
#' \describe{
#'   \item{sjukhus}{a character vector containing hospitals names.}
#'   \item{landsting}{a numeric vector containing codes for the 'landsting' for the respective hospitals.}
#'   \item{region}{a numeric vector containg codes (1-6) for the region for the respective hospitals.}
#'   \item{period}{year of diagnosis (2012-2016).}
#'   \item{age}{patients age at diagnosis.}
#'   \item{outcome1}{dichotomous outcome (logical: TRUE/FALSE).}
#'   \item{outcome2}{continuous outcome (numeric).}
#'   \item{outcome3}{categorical outcome with 5 levels (factor).}
#'   \item{stage}{patients stage at diagnosis.}
#' }
"rccShinyData"
