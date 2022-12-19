library(shinytest2)

appdir <- file.path("apps", "sv", "inca1")

test_that("inca1 works", {
  expect_no_error(test_app(appdir))
})
