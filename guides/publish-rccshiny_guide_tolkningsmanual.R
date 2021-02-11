library(rsconnect)

rsconnect::deployDoc(
  doc = file.path("guides", "tolkningsmanual", "index.Rmd"),
  appName = "rccshiny_guide_tolkningsmanual",
  appId = 1128,
  server = "statistik.incanet.se",
  forceUpdate = TRUE
)
