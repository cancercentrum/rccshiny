# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "662f407", type = "binary")

library(testthat)
library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

if (packageVersion("rccShiny") == "1.7.1.9000" & sysname == "darwin") {

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
    file.path("tests", "testthat", "apps", "sv", "legacy0-1.3"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-1.4.2"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-1.4.2"),
    file.path("tests", "testthat", "apps", "en", "legacy2-1.4.2"),
    file.path("tests", "testthat", "apps", "sv", "legacy0-1.4.2"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-1.5.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-1.5.1"),
    file.path("tests", "testthat", "apps", "en", "legacy2-1.5.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy0-1.5.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-1.6.1"),
    file.path("tests", "testthat", "apps", "en", "legacy2-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy0-1.6.1"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy0-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "load-app", suffix = "mactest"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "app1"),
    file.path("tests", "testthat", "apps", "sv", "app1hc"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-app1", suffix = "mactest"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "inca1"),
    file.path("tests", "testthat", "apps", "sv", "inca1hc")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-inca1", suffix = "mactest"))
  }
}
