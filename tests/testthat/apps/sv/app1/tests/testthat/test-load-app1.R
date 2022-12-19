library(shinytest2)

test_that("loading app1 works", {
  app <- AppDriver$new(
    name = "app1",
    expect_values_screenshot_args = FALSE
  )
  app$wait_for_value(input = "param_outcome")
  app$expect_values(input = "param_outcome")
  app$stop()
})
