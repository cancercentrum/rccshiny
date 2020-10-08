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

test_that("loading legacy1-1.3 works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy1-1.3")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy2-1.3 (sv) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy2-1.3")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy2-1.3 (en) works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "en", "legacy2-1.3")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})

test_that("loading legacy3-1.3 works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacy3-1.3")
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
