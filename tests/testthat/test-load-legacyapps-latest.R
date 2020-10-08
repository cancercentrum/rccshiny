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

test_that("loading legacyapp1-latest works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp1-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp2-latest (sv) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp2-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp2-latest (en) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "en", "legacyapp2-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp3-latest works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp3-latest")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
