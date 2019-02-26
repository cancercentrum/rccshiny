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

Lägg upp ett ärende <https://cancercentrum.atlassian.net/secure/RapidBoard.jspa?rapidView=77&projectKey=SNY> eller kontakta Marie Lindquist, <marie.lindquist@sll.se>!
