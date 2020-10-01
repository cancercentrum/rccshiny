# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")

library(shinytest)
library(rccShiny)

sysname <- tolower(Sys.info()[["sysname"]])

# Expect test results as when running the legacyapp using the latest release
if (packageVersion("rccShiny") == "1.6.0" & sysname == "windows") {

  appdir_list <- list(
    file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.3"),
    file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.4.2"),
    file.path("tests", "testthat", "apps", "sv", "legacyapp1-1.5.1")
  )

  for (appdir in appdir_list) {
    x <- testApp(
      appDir = appdir,
      testnames = "mytest",
      quiet = TRUE,
      compareImages = FALSE,
      interactive = FALSE,
      suffix = "windows"
    )
    if (!x$results[[1]]$pass) {
      snapshotUpdate(appdir, "mytest", quiet = TRUE, suffix = "windows")
    }
  }
}
