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

Man kan också manuellt hämta en komprimerad arkiv-fil med senaste
släppta versionen från
<https://bitbucket.org/cancercentrum/rccshiny/get/master.tar.gz> och
installera med
`install.packages("cancercentrum-rccshiny-COMMIT.tar.gz", repos = NULL, type = "source")`
där COMMIT i Butbuckets namngivning av den komprimerade arkiv-filen
baseras på identifieraren för sista ändringen i kodförrådet för den
släppta versionen (se
<https://bitbucket.org/cancercentrum/rccshiny/commits/branch/master>).

Arkiv-filer för olika versioner av paketet finns på
<https://bitbucket.org/cancercentrum/rccshiny/downloads/?tab=tags>

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
