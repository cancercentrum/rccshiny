# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "1.5.1")

library(shinytest)
library(rccShiny)

appdir <- file.path("inst", "testapps", "sv", "legacyapp-1.5.1")

if (packageVersion("rccShiny") == "1.5.1") {
  testApp(appdir, compareImages = FALSE, suffix = "windows")
}

# snapshotUpdate(appdir, suffix = "windows")
