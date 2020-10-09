# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")

library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

# Expect shinytest results as when running the legacy apps using the latest release
if (packageVersion("rccShiny") == "1.6.0" & sysname == "darwin") {
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
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "load-app",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE,
      suffix = "mac"
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "load-app", quiet = TRUE, suffix = "mac")
    }
  }

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "legacy1-latest"),
    file.path("tests", "testthat", "apps", "sv", "legacy1hc-latest")
  )
  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "nav-app1",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE,
      suffix = "mac"
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "nav-app1", quiet = TRUE, suffix = "mac")
    }
  }
}
