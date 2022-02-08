library(rsconnect)

rsconnect::deployDoc(
  doc = file.path("guides", "connect", "index.Rmd"),
  appName = "rccshiny_guide_connect",
  appId = 2443,
  server = "statistik.incanet.se",
  forceUpdate = TRUE
)
