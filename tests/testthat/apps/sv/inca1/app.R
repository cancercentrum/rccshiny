library(rccShiny)

# Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-94
rccShiny2(
  inca = TRUE,
  incaScript = system.file(package = "rccShiny", file.path("scripts", "inca-dm.R")),
  incaIncludeList = TRUE,
  id = "id",
  idAuthorisedToView = "idAuthorisedToView",
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
  )
)
