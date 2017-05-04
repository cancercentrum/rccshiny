library(shiny)
library(rccShiny)

?rccShiny
rccShiny(
  data = rccShinyData,
  folder = "testRapport2",
  path = "C:/Users/552l/test_rccShiny",
  outcome = "outcome1",
  outcomeTitle = "Dikotom"
)

runApp("C:/Users/552l/test_rccShiny/apps/sv/testRapport2")
