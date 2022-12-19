library(shinytest2)

test_that("loading app2 sv works", {
  app <- AppDriver$new(
    name = "app2-sv",
    expect_values_screenshot_args = FALSE
  )
  app$wait_for_value(input = "param_outcome")
  app$expect_values(input = "param_outcome")
  app$stop()
})
