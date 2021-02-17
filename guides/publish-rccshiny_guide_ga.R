library(rsconnect)

rsconnect::deployDoc(
  doc = file.path("guides", "ga", "index.Rmd"),
  appName = "rccshiny_guide_ga",
  appId = 1127,
  server = "statistik.incanet.se",
  forceUpdate = TRUE
)
