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

test_that("loading legacyapp1-1.4.2 works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp1-1.4.2")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp2-1.4.2 (sv) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp2-1.4.2")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp2-1.4.2 (en) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "en", "legacyapp2-1.4.2")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacyapp3-1.4.2 works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp3-1.4.2")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
