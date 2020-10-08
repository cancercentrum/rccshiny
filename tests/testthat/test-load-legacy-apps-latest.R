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

test_that("loading legacy1-latest works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy1-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy2-latest (sv) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy2-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy2-latest (en) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "en", "legacy2-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy3-latest works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy3-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
