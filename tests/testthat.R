library(testthat)
library(rccShiny)

# Adopted from https://github.com/rstudio/webdriver/blob/appveyor/tests/testthat.R
if (is.null(webdriver:::find_phantom())) webdriver::install_phantomjs()
cat("Using phantom.js from", webdriver:::find_phantom(), "\n")

# For diagnostics
cat(paste0("Sys.info()[['sysname']]: ", Sys.info()[["sysname"]], "\n"))
cat(paste0("Sys.getenv('_R_CHECK_PACKAGE_NAME_'): ", Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "\n"))
cat(paste0("packageVersion('shinyWidgets'): ", packageVersion("shinyWidgets"), "\n"))

test_check("rccShiny", reporter = "minimal")
