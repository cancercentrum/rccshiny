library(testthat)
library(rccShiny)

# Adopted from https://github.com/rstudio/webdriver/blob/appveyor/tests/testthat.R
if (is.null(webdriver:::find_phantom())) webdriver::install_phantomjs()
cat("Using phantom.js from", webdriver:::find_phantom(), "\n")

# For diagnostics
cat(paste0("Sys.info()[['sysname'']]: ", Sys.info()[["sysname"]], "\n"))
cat(paste0("Sys.getenv('NOT_CRAN'): ", Sys.getenv("NOT_CRAN"), "\n"))
cat(paste0("Sys.getenv('_R_CHECK_FORCE_SUGGESTS_'): ", Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"), "\n"))
cat(paste0("Sys.getenv('R_TESTS'): ", Sys.getenv("R_TESTS"), "\n"))
cat(paste0("packageVersion('shinyWidgets'): ", packageVersion("shinyWidgets"), "\n"))

test_check("rccShiny", reporter = "minimal")
