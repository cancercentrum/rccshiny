
library(shiny)
library(shinyWidgets)
library(DT)
library(rccShiny)

load("./data/data.RData")

rccShinyApp(optionsList = GLOBAL_optionsList)
