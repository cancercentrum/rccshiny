library(shiny)
library(rccShiny)

rccShiny2(
  data = rccShinyData,
  folder = "sampleapp",
  path = "inst",
  outcome = paste0("outcome", 1:3),
  outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
  description = c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
  varOther = list(
    list(
      var = "age",
      label = "Alder vid diagnos"
    ),
    list(
      var = "stage",
      label = "Stadium",
      choices = c("I", "II"),
      selected = "I",
      multiple = TRUE,
      showInTitle = TRUE
    )
  ),
  funnelplot = TRUE
)

