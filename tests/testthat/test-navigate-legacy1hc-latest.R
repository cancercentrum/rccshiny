library(shinytest)

sysname <- tolower(Sys.info()[["sysname"]])

if (identical(Sys.getenv("APPVEYOR"), "True")) {
  suffix <- "appveyor"
} else if (sysname == "windows") {
  suffix <- "windows"
} else if (sysname %in% c("darwin", "linux")) {
  suffix <- "mac"
} else {
  suffix <- NULL
}

test_that("navigating legacy1hc-latest works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy1hc-latest")
  expect_pass(testApp(appdir, "nav-app1", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
