# rccShiny

[![R-CMD-check](https://github.com/cancercentrum/rccshiny/workflows/R-CMD-check/badge.svg)](https://github.com/cancercentrum/rccshiny/actions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/h81m5d2ie1p7tqt2/branch/develop?svg=true)](https://ci.appveyor.com/project/oc1lojo/rccshiny)

Shiny möjliggör skapandet av webbapplikationer med R. Paketet `rccShiny`
är en RCC-utvidgning vars syfte är att skapa interaktiva rapporter för
redovisning av svensk cancervård med möjlighet för slutanvändaren att
själv skräddarsy selektionskriterier och format på statistiken på ett
flexibelt sätt, se
<https://cancercentrum.se/samverkan/vara-uppdrag/statistik/kvalitetsregisterstatistik/interaktiva-rapporter/>

## Installation

``` {.r}
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

remotes::install_bitbucket("cancercentrum/rccshiny", ref = "master")
```

## Användning

Ladda paketet.

``` {.r}
library(rccShiny)
```

Läs hjälpfil.

``` {.r}
?rccShiny::rccShiny2
```
