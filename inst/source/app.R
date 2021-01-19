library(rccShiny)

load("./data/data.RData")

rccShinyApp(
  optionsList = optionsList,
  pageTitle = optionsList$pageTitle
)
