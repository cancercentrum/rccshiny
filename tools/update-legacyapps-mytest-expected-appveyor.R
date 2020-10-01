# Preparations
#
# 1. Download https://ci.appveyor.com/project/oc1lojo/rccshiny/builds/BUILD_NUMBER/artifacts > failure.zip
# 2. Copy contents of failure\rccShiny.Rcheck\tests\testthat\apps to tests/testthat/apps

library(shinytest)
library(rccShiny)

appdir_list <- list(
  file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp2-1.3"),
  file.path("tests", "testthat", "apps", "en", "legacyapp2-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp3-1.3"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp2-1.4.2"),
  file.path("tests", "testthat", "apps", "en", "legacyapp2-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp3-1.4.2"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.5.1"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp2-1.5.1"),
  file.path("tests", "testthat", "apps", "en", "legacyapp2-1.5.1"),
  file.path("tests", "testthat", "apps", "sv", "legacyapp3-1.5.1")
)

for (appdir in appdir_list) {
  try(snapshotUpdate(appdir, "mytest", quiet = TRUE, suffix = "appveyor"))
}
