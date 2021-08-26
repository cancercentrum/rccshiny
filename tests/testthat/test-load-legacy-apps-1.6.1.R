library(shinytest)

test_that("loading legacy1-1.6.1 works", {
  appdir <- file.path("apps", "sv", "legacy1-1.6.1")
  expect_pass(testApp(appdir, "load-app", compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})

test_that("loading legacy2-1.6.1 (sv) works", {
  appdir <- file.path("apps", "sv", "legacy2-1.6.1")
  expect_pass(testApp(appdir, "load-app", compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})

test_that("loading legacy2-1.6.1 (en) works", {
  appdir <- file.path("apps", "en", "legacy2-1.6.1")
  expect_pass(testApp(appdir, "load-app", compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})
