# Preparations
#
# 1. Download https://ci.appveyor.com/project/oc1lojo/rccshiny/builds/BUILD_NUMBER/artifacts > failure.zip
# 2. Copy contents of failure\rccShiny.Rcheck\tests\testthat\apps to tests/testthat/apps

library(shinytest)
library(rccShiny)

appdir_list <- list(
  file.path("tests", "testthat", "apps", "sv", "legacy1-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacy2-1.3"),
  file.path("tests", "testthat", "apps", "en", "legacy2-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacy3-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacy1-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacy2-1.4.2"),
  file.path("tests", "testthat", "apps", "en", "legacy2-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacy3-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacy1-1.5.1"),
  file.path("tests", "testthat", "apps", "sv", "legacy2-1.5.1"),
  file.path("tests", "testthat", "apps", "en", "legacy2-1.5.1"),
  file.path("tests", "testthat", "apps", "sv", "legacy3-1.5.1"),
  file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
  file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
  file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
  file.path("tests", "testthat", "apps", "sv", "legacy3-latest")
)

for (appdir in appdir_list) {
  try(snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "appveyor"))
}

appdir <- file.path("tests", "testthat", "apps", "sv", "legacy1-latest")
try(snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "appveyor"))
