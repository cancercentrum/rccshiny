library(shinytest)

test_that("running legacyapp1-1.5.1 works on windows", {
  skip_on_travis()
  skip_on_appveyor()

  # Skip on other OS than Windows
  skip_on_os(c("mac", "linux", "solaris"))

  appdir <- system.file(package = "rccShiny", file.path("testapps", "sv", "legacyapp1-1.5.1"))
  shinytest::expect_pass(testApp(appdir, compareImages = FALSE, suffix = "windows"))
})
