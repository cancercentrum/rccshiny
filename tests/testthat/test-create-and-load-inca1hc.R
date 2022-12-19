library(shinytest2)

appdir <- file.path("apps", "sv", "inca1hc")

test_that("inca1hc works", {
  expect_no_error(test_app(appdir))
})
