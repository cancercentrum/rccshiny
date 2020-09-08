library(shiny)
library(rccShiny)

ind1 <- rccShiny2(
  data = rccShinyData,
  folder = "Indikator1",
  folderLinkText = "Indikator 1",
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

runApp("./sv/Indikator1")

# For Swedish/English version
rccShinyData$outcome1_en <- rccShinyData$outcome1
rccShiny2(
  language = c("sv", "en"),
  data = rccShinyData,
  folder = "Indikator2",
  folderLinkText = c("Indikator 2", "Indicator 2"),
  outcome = "outcome1",
  outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
  textBeforeSubtitle = c("Nagot pa svenska", "Something in English"),
  description = list(
    c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
    c("Description of the indicator", "Important information!", "Information on variables etc.")
  ),
  varOther = list(
    list(
      var = "age",
      label = c("Alder vid diagnos", "Age at diagnosis"),
      choices = c(0, 120)
    )
  ),
  targetValues = c(95, 99)
)

runApp("./sv/Indikator2")
runApp("./en/Indikator2")
