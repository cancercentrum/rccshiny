# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")

library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

# Expect shinytest results as when running the legacy apps using the latest release
if (packageVersion("rccShiny") == "1.6.0" & sysname == "windows") {

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
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacy2-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy3-latest")
  )
  # load-app-expected (without suffix)
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "load-app",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "load-app", quiet = TRUE)
    }
  }
  # load-app-expected-windows
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "load-app",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE,
      suffix = "windows"
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "windows")
    }
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  # nav-app1-expected (without suffix)
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "nav-app1",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "nav-app1", quiet = TRUE)
    }
  }
  # nav-app1-expected-windows
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "nav-app1",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE,
      suffix = "windows"
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "windows")
    }
  }
}
