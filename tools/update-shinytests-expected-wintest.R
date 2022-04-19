# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master", type = "binary")

library(testthat)
library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

# Use latest release/stable version
if (packageVersion("rccShiny") == "1.11.0" & sysname == "windows") {

  # Run tests
  testthat::test_dir("tests/testthat", reporter = "minimal", stop_on_failure = FALSE, package = "rccShiny")

  # Update snapshots of expected shinytest results
  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "app1"),
    file.path("tests", "testthat", "apps", "sv", "app1hc"),
    file.path("tests", "testthat", "apps", "sv", "inca1"),
    file.path("tests", "testthat", "apps", "sv", "inca1hc"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacy2-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "load-app", suffix = "wintest"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "app1"),
    file.path("tests", "testthat", "apps", "sv", "app1hc"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-app1", suffix = "wintest"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "inca1"),
    file.path("tests", "testthat", "apps", "sv", "inca1hc")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-inca1", suffix = "wintest"))
  }

  # Also copy expected shinytest results from -expected-wintest to -expected
  dirs_expected_wintest <- stringr::str_subset(
    list.dirs("tests/testthat/apps/"),
    pattern = "expected-wintest"
  )
  for (dir_expected_wintest in dirs_expected_wintest) {
    for (file_expected_wintest in list.files(dir_expected_wintest, "*.json",  full.names = TRUE)) {
      file.copy(
        from = file_expected_wintest,
        to = stringr::str_replace(
          file_expected_wintest,
          pattern = "expected-wintest",
          replacement = "expected"
        ),
        overwrite = TRUE
      )
    }
  }
}
