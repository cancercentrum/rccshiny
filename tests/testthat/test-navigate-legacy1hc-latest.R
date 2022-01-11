library(shinytest)

test_that("navigating legacy1hc-latest works", {
  skip_on_os("linux")

  appdir <- file.path("apps", "sv", "legacy1hc-latest")
  expect_pass(testApp(appdir, "nav-app1", compareImages = FALSE, interactive = FALSE, suffix = shinytest_suffix()))
})
