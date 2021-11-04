library(rsconnect)

rsconnect::deployDoc(
  doc = file.path("guides", "startsida", "index.Rmd"),
  appName = "rccshiny_guide_startsida",
  appId = 2333,
  server = "statistik.incanet.se",
  forceUpdate = TRUE
)
