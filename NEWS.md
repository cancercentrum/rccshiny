# rccShiny (development version)

## Mindre förbättringar och felrättningar

* Nyheter om varje ny version av paketet (SNY-83)

# rccShiny 1.6.0

## Ny funktionalitet

* Redovisning av missing - entydigt och med antal (SNY-8)

* Ny parameter med alternativa sjukhusnamn som kan ersätta fullösningen `npcrGroupPrivateOthers` (SNY-61)

# rccShiny 1.5.1

En liten felrättningsversion. 

* Fixat bakåtkompabilitet (d74e1d4)

# rccShiny 1.5.0

## Ny funktionalitet

* Namnsättning av percentiler i figurer och tabeller (SNY-63)

* Vinjett om hur man får Google Analytics att logga besök på varje enskild app (parametern `gaPath`) (SNY-62)

* Justerat filnamn samt rubriker i exporterade filer (Excel, PDF) (SNY-50)

## Mindre förbättringar och felrättningar

* Lagt till `selectAll` och `deselectAll` i `rccShinyTXT()` (SNY-73)

* Rättat felstavningar i engelska versionen (SNY-69)

# rccShiny 1.4.2

En liten felrättningsversion med några mindre förbättringar.

* Skapat vinjetter för anslutningsinfo och tolkningsmanual (SNY-72)

* Justerat assign så att problemet med generisk parametriserad mall avhjälps (SNY-71)

* Rättat fel som gjorde att Javaskript-error från conditionalPanel ledde till att app ibland inte laddas på INCA (SNY-70)

# rccShiny 1.4.1

En liten felrättningsversion. 

* Fixat bakåtkompabilitet (6652b38)

# rccShiny 1.4.0

* Justerat hur data läses in på INCA (SNY-68)

* Lagt till `local = TRUE` till source av incaScript (?) + ta bort `shinycssloaders::withSpinner` (SNY-65)

# rccShiny 1.3

Nyheter i version 1.3 och tidigare versioner.

* Döp om "landsting" till "region" och "region" till "sjukvårdsregion" (SNY-56)

* Ange percentiler i anropet till funktionen (SNY-22)

* Webbplats för R-paketet (SNY-66)

* "Markera alla" och "Avmarkera alla"" i pickerinputs (SNY-59, 4cfc0e7)

* Lägga till möjlighet att exkludera flikar (SNY-39, 54dbd9f)

* Röjandekontroll: Möjlighet att dölja absoluta antal i figurer/tabeller där antalet i en cell är mindre än gränsvärde (SNY-55, cc51329)	

* Återinför möjlighet att få ut länktext (SNY-57, a8f9dc7)

* Manual för användning/tolkning av Shiny-rapporter och KPL (SNY-38, 81cb132)

* Möjlighet att i appen växla mellan urval på period utifrån kvartal eller år (SNY-53, f19f7db)

* Gör textstorlek i jämförelse-figuren dynamisk (SNY-54, ae7d801)

* Möjlighet att frångå kravet på minst 5 i nämnaren om `INCA = TRUE` (SNY-52)

* Möjlighet att använda Highcharts i samtliga figurer	(SNY-46)
	
* Namnsättning av totalen via parameter (möjliggör t.ex. regionala rapporter) (SNY-43)

* Möjlighet att ändra defaultval för "Period" i anropet till rccShiny (SNY-41)

* Möjlighet att ändra defaultval för "Begränsa till region" och "Jämförelsenivå" i anropet till `rccShiny (SNY-42)

* Lägg tillbaka urvalsdialogen i vänstermarginalen, samt lägg tillbaka "Kommentar" (SNY-48)

* Synka layout med IPÖ-rapporterna (Shiny Dashboard) (SNY-47)

* Färg vid enbart en målnivå (SNY-27)

* Ändra färgerna för regionerna i trend-figuren när outcome är dikotom/kontinuerlig (SNY-44)

* Sortering (SNY-10)

* Ytterligare flik Kommentar (SNY-26)
