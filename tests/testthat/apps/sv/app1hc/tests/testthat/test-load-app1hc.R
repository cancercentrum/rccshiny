library(shinytest2)

test_that("loading app1hc works", {
  app <- AppDriver$new(
    name = "app1hc",
    expect_values_screenshot_args = FALSE
  )
  app$wait_for_value(input = "param_outcome")
  app$expect_values(input = "param_outcome")
  app$stop()
})
