# Preparations
#
# 1. Run update-shinytests-expected-windows.R
# 2. Run update-shinytests-expected-appveyor-1.R
# 3. Run Check Package in RStudio
# 4. Run update-shinytests-expected-appveyor-2.R
# 5. Commit and push to BitBucket
# 6. Download https://ci.appveyor.com/project/oc1lojo/rccshiny/builds/BUILD_NUMBER/artifacts > failure.zip
# 7. Copy contents of failure\rccShiny.Rcheck\tests\testthat\apps to tests/testthat/apps

library(shinytest)

appdir_list <- list(
  file.path("tests", "testthat", "apps", "sv", "app1"),
  file.path("tests", "testthat", "apps", "sv", "app1hc"),
  file.path("tests", "testthat", "apps", "sv", "inca1"),
  file.path("tests", "testthat", "apps", "sv", "inca1hc"),
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
  file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
  file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
  file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
  file.path("tests", "testthat", "apps", "sv", "legacy3-latest")
)
for (appdir in appdir_list) {
  try(snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "appveyor"))
}

appdir_list <- list(
  file.path("tests", "testthat", "apps", "sv", "app1"),
  file.path("tests", "testthat", "apps", "sv", "app1hc"),
  file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
  file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
)
for (appdir in appdir_list) {
  try(snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "appveyor"))
}

appdir_list <- list(
  file.path("tests", "testthat", "apps", "sv", "inca1"),
  file.path("tests", "testthat", "apps", "sv", "inca1hc")
)
for (appdir in appdir_list) {
  try(snapshotUpdate(appdir, "nav-inca1", quiet = TRUE, suffix = "appveyor"))
}
