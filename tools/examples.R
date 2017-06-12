library(rccShiny)
library(shiny)
rccShiny(
  data = rccShinyData,
  folder = "Indikator1",
  path = "C:/Users/552l/test_rccShiny",
  outcome = paste0("outcome",1:3),
  outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
  comment = "Skovde och Lidkoping tillhor Skaraborg",
  description = "Att tanka pa vid tolkning ....",
  varOther = list(
    list(
      var = "age"
    )),
  funnelplot = TRUE
     )

 runApp("C:/Users/552l/test_rccShiny/apps/sv/Indikator1")




 # For Swedish/English version
 rccShinyData$outcome1_en <- rccShinyData$outcome1
 rccShiny(
   data = rccShinyData,
   folder = "Indikator2",
   outcome = "outcome1",
   outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
   path = "C:/Users/552l/test_rccShiny",
   textBeforeSubtitle = c("Nagot pa svenska","Something in English"),
   description = c("Superbra att ha!","Supergood to have!"),
   varOther = list(
     list(
       var = "age",
      label = c("Alder vid diagnos","Age at diagnosis"),
       choices = c(0,120)
     )
   ),
   targetValues = c(95,99),
   language = c("sv", "en")
 )
 runApp("C:/Users/552l/test_rccShiny/apps/sv/Indikator2")
 runApp("C:/Users/552l/test_rccShiny/apps/en/Indikator2")
