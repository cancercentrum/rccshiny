
# library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(rccShiny)

load("./data/data.RData")

rccShinyApp(
  optionsList = optionsList,
  pageTitle = optionsList$pageTitle
)
