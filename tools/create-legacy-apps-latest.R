# remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")

library(shiny)
library(rccShiny)

# Latest release (1.6.0 at th moment)
if (packageVersion("rccShiny") == "1.6.0") {

  # Create legacy1-latest
  # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-94
  rccShiny2(
    data = rccShinyData,
    folder = "legacy1-latest",
    path = file.path("tests", "testthat", "apps"),
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

  # Create legacy2-latest
  # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-124
  rccShinyData$outcome1_en <- rccShinyData$outcome1
  rccShiny2(
    language = c("sv", "en"),
    data = rccShinyData,
    folder = "legacy2-latest",
    path = file.path("tests", "testthat", "apps"),
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

  # Create legacy3-latest
  # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny.R#lines-77
  rccShiny(
    data = rccShinyData,
    folder = "legacy3-latest",
    path = file.path("tests", "testthat"),
    outcome = paste0("outcome", 1:3),
    outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
    comment = "Skovde och Lidkoping tillhor Skaraborg",
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
