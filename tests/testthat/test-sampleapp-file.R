library(shinytest)

test_that("sampleapp works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages = FALSE because (potential) expected image screenshots
  # were (probably) created on a Windows system, and they will differ from
  # screenshots taken on a CI platform that runs on Linux.
  appdir <- system.file(package = "rccShiny", file.path("sv", "sampleapp"))
  expect_pass(testApp(appdir, compareImages = FALSE))
})
