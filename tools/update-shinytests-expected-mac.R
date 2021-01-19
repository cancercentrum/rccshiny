# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "d66737c", type = "binary")

library(testthat)
library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

if (packageVersion("rccShiny") == "1.6.1.9001" & sysname == "darwin") {

  # Run tests using the latest release/stable version
  test_file(test_path("test-create-load-navigate-app1.R"), package = "rccShiny")
  test_file(test_path("test-create-load-navigate-app1hc.R"), package = "rccShiny")
  test_file(test_path("test-create-load-navigate-inca1.R"), package = "rccShiny")
  test_file(test_path("test-create-load-navigate-inca1hc.R"), package = "rccShiny")
  test_file(test_path("test-load-legacy-apps-1.3.R"), package = "rccShiny")
  test_file(test_path("test-load-legacy-apps-1.4.2.R"), package = "rccShiny")
  test_file(test_path("test-load-legacy-apps-1.5.1.R"), package = "rccShiny")
  test_file(test_path("test-load-legacy-apps-latest.R"), package = "rccShiny")
  test_file(test_path("test-navigate-legacy1-latest.R"), package = "rccShiny")
  test_file(test_path("test-navigate-legacy1hc-latest.R"), package = "rccShiny")

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
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy3-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "mac"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "app1"),
    file.path("tests", "testthat", "apps", "sv", "app1hc"),
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "mac"))
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "inca1"),
    file.path("tests", "testthat", "apps", "sv", "inca1hc")
  )
  for (appdir in appdir_list) {
    try(snapshotUpdate(appdir, "nav-inca1", quiet = TRUE, suffix = "mac"))
  }
}
