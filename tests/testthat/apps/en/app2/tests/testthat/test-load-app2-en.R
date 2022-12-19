library(shinytest2)

test_that("loading app2 en works", {
  app <- AppDriver$new(
    name = "app2-en",
    expect_values_screenshot_args = FALSE
  )
  app$wait_for_value(input = "param_outcome")
  app$expect_values(input = "param_outcome")
  app$stop()
})
