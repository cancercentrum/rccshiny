 rccShiny(
   data = rccShinyData,
   folder = "Indikator1",
   path = "C:/Users/552l/test_rccShiny",
   outcome = paste0("outcome",1:3),
   outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
   comment = "Skövde och Lidköping tillhör Skaraborg",
   description = "Ett fall per bröst kan ha rapporterats till det nationella kvalitetsregistret för bröstcancer.
   Det innebär att samma person kan finnas med i statistiken upp till två gånger.",
   userInputList = list(
     list(
       var = "age",
       label = "Ålder vid diagnos"
     ),
     list(
       var = "stage",
     label = "Stadium",
     choices = c("I", "II")
   )
   ),
    funnelplot = TRUE
 )
 runApp("C:/Users/552l/test_rccShiny/apps/sv/Indikator1")

 # For Swedish/English version
 rccShinyData$outcome1_en <- rccShinyData$outcome1
 rccShiny(
   data = rccShinyData,
   folder = "Indikator2",
   path = "C:/Users/552l/test_rccShiny",
    outcome = "outcome1",
   outcomeTitle = c("Kontaktsjuksköterska", "Contact nurse"),
   titleTextBeforeSubtitle = c("Något på svenska","Something in English"),
   description = c("Superbra att ha!","Supergood to have!"),
   userInputList = list(
     list(
       var = "age",
       label = c("Ålder vid diagnos","Age at diagnosis"),
       choices = c(0,120)
     )
   ),
   targetValues = c(95,99),
   language = c("sv", "en")
 )

 runApp("C:/Users/552l/test_rccShiny/apps/sv/Indikator2")
 runApp("C:/Users/552l/test_rccShiny/apps/en/Indikator2")
