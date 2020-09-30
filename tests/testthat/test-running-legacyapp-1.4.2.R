library(shinytest)

test_that("running legacyapp-1.4.2 works on windows", {
  skip_on_travis()
  skip_on_appveyor()

  # Skip on other OS than Windows
  skip_on_os(c("mac", "linux", "solaris"))

  appdir <- system.file(package = "rccShiny", file.path("testapps", "sv", "legacyapp-1.4.2"))
  shinytest::expect_pass(testApp(appdir, quiet = TRUE, compareImages = FALSE, suffix = "windows"))
})
