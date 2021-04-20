# rccShiny

[![R-CMD-check](https://github.com/oc1lojo/rccshiny/workflows/R-CMD-check/badge.svg)](https://github.com/oc1lojo/rccshiny/actions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/h81m5d2ie1p7tqt2/branch/develop?svg=true)](https://ci.appveyor.com/project/oc1lojo/rccshiny)

Shiny möjliggör skapandet av webbapplikationer med R. Paketet `rccShiny`
är en RCC implementering vars syfte är att skapa ett komplement till
årsrapporterna där slutanvändaren själv kan välja selektionskriterier
och format på statistiken på ett flexibelt sätt.

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

För fler exempel, se t.ex. R-koden för för Bröstcancerregistrets
interaktiva rapporter (<http://statistik.incanet.se/brostcancer/>) på
<https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny> .
