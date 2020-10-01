library(testthat)
library(rccShiny)

# Adopted from https://github.com/rstudio/webdriver/blob/appveyor/tests/testthat.R
if (is.null(webdriver:::find_phantom())) webdriver::install_phantomjs()
cat("Using phantom.js from", webdriver:::find_phantom(), "\n")

test_check("rccShiny")
