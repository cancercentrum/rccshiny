# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "1.6.0")

library(shinytest)
library(rccShiny)

# Expect test results as when running the legacyapp using the latest minor version
if (packageVersion("rccShiny") == "1.6.0") {
  appdir <- file.path("inst", "testapps", "sv", "legacyapp-1.4.2")
  testApp(appdir, compareImages = FALSE, suffix = "windows")
}
