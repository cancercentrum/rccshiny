library(rsconnect)

rsconnect::deployDoc(
  doc = file.path("guides", "generiskmall", "index.Rmd"),
  appName = "rccshiny_guide_generiskmall",
  appId = 2444,
  server = "statistik.incanet.se",
  forceUpdate = TRUE
)
