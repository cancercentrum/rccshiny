library(shinytest)

test_that("navigating legacy1-latest works", {
  appdir <- file.path("apps", "sv", "legacy1-latest")
  expect_pass(testApp(appdir, "nav-app1", compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})
