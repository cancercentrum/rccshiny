# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")

library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

# Expect shinytest results as when running the legacyapp using the latest release
if (packageVersion("rccShiny") == "1.6.0" & sysname == "windows") {

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
    file.path("tests", "testthat", "apps", "sv", "legacyapp3-1.5.1"),
    file.path("tests", "testthat", "apps", "sv", "legacyapp1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacyapp2-latest"),
    file.path("tests", "testthat", "apps", "en", "legacyapp2-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacyapp3-latest")
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

  # navigate-app1-expected (without suffix)
  appdir <- file.path("tests", "testthat", "apps", "sv", "legacyapp1-latest")
  x <- testApp(
    appDir = appdir,
    testnames = "navigate-app1",
    quiet = TRUE,
    compareImages = FALSE,
    interactive = FALSE
  )
  if (!x$results[[1]]$pass) {
    snapshotUpdate(appdir, "navigate-app1", quiet = TRUE)
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

  # navigate-app1-expected-windows
  appdir <- file.path("tests", "testthat", "apps", "sv", "legacyapp1-latest")
  x <- testApp(
    appDir = appdir,
    testnames = "navigate-app1",
    quiet = TRUE,
    compareImages = FALSE,
    interactive = FALSE,
    suffix = "windows"
  )
  if (!x$results[[1]]$pass) {
    snapshotUpdate(appdir, "navigate-app1", quiet = TRUE, suffix = "windows")
  }
}
