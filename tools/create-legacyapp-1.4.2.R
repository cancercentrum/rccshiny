# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "1.4.2")

library(shiny)
library(rccShiny)

if (packageVersion("rccShiny") == "1.4.2") {
  rccShiny2(
    data = rccShinyData,
    folder = "legacyapp-1.4.2",
    path = file.path("inst", "testapps"),
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
}
