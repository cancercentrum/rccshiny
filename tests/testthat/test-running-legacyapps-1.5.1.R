library(shinytest)

test_that("running legacyapp1-1.5.1 works on windows", {
  skip_on_travis()
  # skip_on_appveyor()

  # Skip on other OS than Windows
  skip_on_os(c("mac", "linux", "solaris"))

  appdir <- file.path("apps", "sv", "legacyapp1-1.5.1")
  if (identical(Sys.getenv("APPVEYOR"), "True")) {
    expect_pass(testApp(appdir, quiet = TRUE, compareImages = FALSE, suffix = "appveyor"))
  } else {
    expect_pass(testApp(appdir, quiet = TRUE, compareImages = FALSE, suffix = "windows"))
  }
})

test_that("running legacyapp1-1.5.1 works on mac", {
  skip_on_travis()
  # skip_on_appveyor()

  # Skip on other OS than Mac
  skip_on_os(c("windows", "linux", "solaris"))

  appdir <- file.path("apps", "sv", "legacyapp1-1.5.1")
  expect_pass(testApp(appdir, quiet = TRUE, compareImages = FALSE, suffix = "mac"))
})
