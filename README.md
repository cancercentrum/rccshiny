[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/oc1lojo/rccshiny?branch=master&svg=true)](https://ci.appveyor.com/project/oc1lojo/rccshiny)
[![Travis build
status](https://travis-ci.org/oc1lojo/rccshiny.svg?branch=master)](https://travis-ci.org/oc1lojo/rccshiny)

Syfte
=====

Shiny möjliggör skapandet av webbapplikationer med R. Paketet `rccShiny` är en RCC implementering vars syfte är att skapa ett komplement till årsrapporterna där slutanvändaren själv kan välja selektionskriterier och format på statistiken på ett flexibelt sätt.

Nedladdning
===========

Installera R paketet `devtools` från CRAN:

``` r
install.packages("devtools")
```

Installera R paketet `rccShiny`:

``` r
devtools::install_bitbucket("cancercentrum/rccshiny")
```

Ladda paketet:

``` r
library(rccShiny) 
```

Läs hjälpfil:

``` r
?rccShiny::rccShiny
```

Gogogo!

För fler exempel: Koden för Bröstcancerregistrets appar finns <https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny> och årsrapporten <http://statistik.incanet.se/brostcancer/>.

Frågor och förbättringsförslag
==============================

Lägg upp en issue <https://bitbucket.org/cancercentrum/rccshiny/issues> eller kontakta Fredrik Sandin, <fredrik.sandin@rccuppsalaorebro.se> eller Erik Lindberg, <erik.g.lindberg@regionvasterbotten.se>.
