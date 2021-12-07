# rccShiny 1.10.1

En liten felrättningsversion.

# rccShiny 1.10.0

## Ny funktionalitet

* Slutanvändarens urval kan sparas mellan interaktiva rapporter om de används tillsammans med en ny version av startsidan (index.html), se guiden https://statistik.incanet.se/startsida/ (SNY-103)

## Mindre förbättringar och felrättningar

* Data för `rccShiny2()` (argumentet `data`) kan vara av en underklass till klassen `data.frame`, t.ex. `tibble::tbl_df` (SNY-109)
* Färger i grafer är anpassade till den grafiska manualen för INCA-plattformen (SNY-94)
* Den gamla funktionen `rccShiny()` är nu helt utfasad (SNY-90, SNY-101)

# rccShiny 1.9.1

En liten felrättningsversion (främst).

* Rättat fel då presentationsform "antal fall" kraschar när sjukhus är NA (SNY-105)

* Interna funktioner i paketet är numera strikt interna. För bakgrund, se t.ex.
    * https://www.r-bloggers.com/2019/12/internal-functions-in-r-packages/ och
    * https://style.tidyverse.org/documentation.html#internal-functions

# rccShiny 1.9.0

## Ny funktionalitet

* Lagt till presentationsform: Antal fall (SNY-51)

* Lagt till möjlighet att ersätta "Sjukhus", "Region" och "Sjukvårdsregion" med egna etiketter (SNY-78)

## Mindre förbättringar och felrättningar

* Rättat fel när urvalstexten under figurrubriken inte syntes om "reglage"-urval fanns längst ner bland urvalen (SNY-95)

* Rättat fel så att labels i varOtherComparison kan vara de som normalt används för sjukvårdsregion, region, län, sjukhus givet den jämförelsenivån inte är inkluderad i figuren. (SNY-92)

# rccShiny 1.8.1

En liten felrättningsversion (främst).

* Fixat problem med numerisk extra urvalsvariabel varOther när variabeln inte bara innehåller heltal (SNY-102)

* Lagt till alternativ för att justera intervallet mellan varje valbart värde på skjutreglaget för varje numerisk variabel i varOther

* Färgen på siffror i kartor anpassas efter bakgrundsfärgens ljusstyrka (05c3519)

# rccShiny 1.8.0

* Den gamla funktionen `rccShiny()` börjar nu gradvis fasas ut (SNY-90), använd `rccShiny2()` istället

## Ny funktionalitet

* Justerat presentation när hideLessThanCell> 1 och utfallet är en kategorisk variabel. Det totala antalet visas nu alltid om N >= hideLessThan medan enskilda celler är dolda (i tabell) om någon n < hideLessThanCell (SNY-89)

## Mindre förbättringar och felrättningar

* Förbättrat färgskalor för kartor (SNY-85)

* Uppdaterat Google Analytics-guiden med vägledning om anonymizeIp

* Tagit bort "Begränsa till sjukvårdsregion" i trendfliken när utfallet är en kategorisk variabel (SNY-93)

# rccShiny 1.7.1

En liten felrättningsversion (främst).

* Ett fel meddelas om _labels_ i varOtherComparison är i konflikt med de som används för 
sjukvårdsregion/bostadslän/region/sjukhus (SNY-91)

* Innehållet i vinjetten "Tolkningsmanual för interaktiva Shinyrapporter" har flyttat till https://statistik.incanet.se/tolkningsmanual/

* Innehållet i vinjetten "Använda Google Analytics med rccShiny" har flyttat till https://statistik.incanet.se/ga/

# rccShiny 1.7.0

## Ny funktionalitet

* Lagt till alternativ för att inkludera andra variabler som presentationsnivå (SNY-9, SNY-45)

* Döpt om sjukvårdsregionen "Uppsala-Örebro" till "Mellansverige" i alla presentationer (SNY-88)

* "folder" används nu som webbsidans titel och kan ses i Google Analytics. Om INCA sätts ingen sidrubrik (SNY-86)

## Mindre förbättringar och felrättningar

* includeMissingColumn = TRUE + outcome som faktor med en nivå = "Uppgift saknas" orsakar krasch (SNY-81)

# rccShiny 1.6.1

En liten felrättningsversion (främst).

* Kartan funkar inte i R version 4+ (SNY-87)

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

## Mindre förbättringar och felrättningar

* Lagt till `selectAll` och `deselectAll` i `rccShinyTXT()` (SNY-73)

* Rättat felstavningar i engelska versionen (SNY-69)

* Justerat filnamn samt rubriker i exporterade filer (Excel, PDF) (SNY-50)

# rccShiny 1.4.2

En liten felrättningsversion (främst).

* Justerat assign så att problemet med generisk parametriserad mall avhjälps (SNY-71)

* Rättat fel som gjorde att Javaskript-error från conditionalPanel ledde till att app ibland inte laddas på INCA (SNY-70)

* Skapat vinjetter för anslutningsinfo och tolkningsmanual (SNY-72)

# rccShiny 1.4.1

En liten felrättningsversion. 

* Fixat bakåtkompabilitet (6652b38)

# rccShiny 1.4.0

* Justerat hur data läses in på INCA (SNY-68)

* Lagt till `local = TRUE` till source av incaScript (?) + ta bort `shinycssloaders::withSpinner` (SNY-65)

# rccShiny 1.3

Nyheter i version 1.3 och tidigare versioner.

* Döpt om "landsting" till "region" och "region" till "sjukvårdsregion" (SNY-56)

* Kan ange percentiler i anropet till funktionen (SNY-22)

* Skapat webbplats för R-paketet (SNY-66)

* Lagt till "Markera alla" och "Avmarkera alla"" i pickerinputs (SNY-59, 4cfc0e7)

* Lagt till möjlighet att exkludera flikar (SNY-39, 54dbd9f)

* Röjandekontroll: Möjlighet att dölja absoluta antal i figurer/tabeller där antalet i en cell är mindre än gränsvärde (SNY-55, cc51329)	

* Återinfört möjlighet att få ut länktext (SNY-57, a8f9dc7)

* Skapat manual för användning/tolkning av Shiny-rapporter (SNY-38, 81cb132)

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

