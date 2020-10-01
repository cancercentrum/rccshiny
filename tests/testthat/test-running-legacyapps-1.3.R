library(shinytest)

test_that("running legacyapp1-1.3 works on windows", {
  skip_on_travis()
  # skip_on_appveyor()

  # Skip on other OS than Windows
  skip_on_os(c("mac", "linux", "solaris"))

  appdir <- system.file(package = "rccShiny", file.path("testapps", "sv", "legacyapp1-1.3"))
  # expect_pass(testApp(appdir, "mytest", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = "windows"))
  x <- testApp(appdir, "mytest", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = "windows")
  testthat::expect(x$results[[1]]$pass, paste("shinytest failed for", x$appDir))
})

test_that("running legacyapp1-1.3 works on mac", {
  skip_on_travis()
  # skip_on_appveyor()

  # Skip on other OS than Mac
  skip_on_os(c("windows", "linux", "solaris"))

  appdir <- system.file(package = "rccShiny", file.path("testapps", "sv", "legacyapp1-1.3"))
  # expect_pass(testApp(appdir, "mytest", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = "mac"))
  x <- testApp(appdir, "mytest", quiet = TRUE, compareImages = FALSE, interactive = FALSE, suffix = "mac")
  testthat::expect(x$results[[1]]$pass, paste("shinytest failed for", x$appDir))
})
