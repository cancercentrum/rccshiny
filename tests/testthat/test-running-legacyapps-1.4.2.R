library(shinytest)

test_that("running legacyapp1-1.4.2 works on windows", {
  skip_on_travis()
  # skip_on_appveyor()

  # Skip on other OS than Windows
  skip_on_os(c("mac", "linux", "solaris"))

  appdir <- system.file(package = "rccShiny", file.path("testapps", "sv", "legacyapp1-1.4.2"))
  expect_pass(testApp(appdir, "mytest", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = "windows"))
})
