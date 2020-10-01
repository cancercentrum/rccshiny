library(shinytest)

sysname <- tolower(Sys.info()[["sysname"]])

if (identical(Sys.getenv("APPVEYOR"), "True")) {
  suffix <- "appveyor"
} else if (sysname == "windows") {
  suffix <- "windows"
} else if (sysname %in% c("mac", "linux")) {
  suffix <- "mac"
} else {
  suffix <- NULL
}

test_that("running legacyapp1-1.5.1 works", {
  skip_on_os("solaris")

  appdir <- file.path("apps", "sv", "legacyapp1-1.5.1")
  expect_pass(testApp(appdir, quiet = TRUE, compareImages = FALSE, suffix = suffix))
})
