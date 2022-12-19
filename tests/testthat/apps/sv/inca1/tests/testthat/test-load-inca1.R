library(shinytest2)

test_that("loading inca1 works", {
  app <- AppDriver$new(
    name = "inca1",
    expect_values_screenshot_args = FALSE
  )
  app$wait_for_value(input = "param_outcome")
  app$expect_values(input = "param_outcome")
  app$stop()
})
