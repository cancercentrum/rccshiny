# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "1.7.0", type = "binary")

library(testthat)
library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

if (packageVersion("rccShiny") == "1.7.0" & sysname == "windows") {

  # Run tests using the latest release/stable version
  testthat::test_dir("tests/testthat", reporter = "minimal", stop_on_failure = FALSE, package = "rccShiny")

  # Update snapshots of expected shinytest results
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
    file.path("tests", "testthat", "apps", "sv", "legacy1-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-1.6.1"),
    file.path("tests", "testthat", "apps", "en", "legacy2-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy3-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy3-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "windows"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "app1"),
    file.path("tests", "testthat", "apps", "sv", "app1hc"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "windows"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "inca1"),
    file.path("tests", "testthat", "apps", "sv", "inca1hc")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-inca1", quiet = TRUE, suffix = "windows"))
  }

  # Also copy expected shinytest results from -expected-windows to -expected
  dirs_expected_windows <- stringr::str_subset(
    list.dirs("tests/testthat/apps/"),
    pattern = "expected-windows"
  )
  for (dir_expected_windows in dirs_expected_windows) {
    for (file_expected_windows in list.files(dir_expected_windows, "*.json",  full.names = TRUE)) {
      file.copy(
        from = file_expected_windows,
        to = stringr::str_replace(
          file_expected_windows,
          pattern = "expected-windows",
          replacement = "expected"
        ),
        overwrite = TRUE
      )
    }
  }
}
