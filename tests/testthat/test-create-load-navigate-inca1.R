library(shinytest)

appdir <- file.path("apps", "sv", "inca1")

test_that("creating and loading inca1 works", {
  expect_pass(testApp(appdir, "load-app", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})

test_that("creating, loading and navigating inca1 works", {
  expect_pass(testApp(appdir, "nav-inca1", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})
