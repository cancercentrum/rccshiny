library(shinytest)

test_that("sampleapp works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Skip on Appveyor, for now
  skip_on_appveyor()

  # Skip on Travis, for now
  skip_on_travis()

  # Use compareImages = FALSE because (potential) expected image screenshots
  # were (probably) created on a Windows system, and they will differ from
  # screenshots taken on a CI platform that runs on Linux.
  appdir <- system.file(package = "rccShiny", file.path("sv", "sampleapp"))
  expect_pass(testApp(appdir, compareImages = FALSE))
})
