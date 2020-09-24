
rccShiny
========

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/h81m5d2ie1p7tqt2/branch/master?svg=true)](https://ci.appveyor.com/project/oc1lojo/rccshiny)
[![Travis build
status](https://travis-ci.com/oc1lojo/rccshiny.svg?branch=master)](https://travis-ci.com/oc1lojo/rccshiny)

Shiny möjliggör skapandet av webbapplikationer med R. Paketet `rccShiny`
är en RCC implementering vars syfte är att skapa ett komplement till
årsrapporterna där slutanvändaren själv kan välja selektionskriterier
och format på statistiken på ett flexibelt sätt.

Installation
------------

    if (!requireNamespace("remotes")) {
      install.packages("remotes")
    }

    remotes::install_bitbucket("cancercentrum/rccshiny")
    # remotes::install_bitbucket("cancercentrum/rccshiny", build_vignettes = TRUE) # även vinjetter

Man kan också manuellt hämta en komprimerad arkiv-fil med senaste
släppta versionen från
<a href="https://bitbucket.org/cancercentrum/rccshiny/get/master.tar.gz" class="uri">https://bitbucket.org/cancercentrum/rccshiny/get/master.tar.gz</a>
och installera med  
`install.packages("cancercentrum-rccshiny-COMMIT.tar.gz", repos = NULL, type = "source")`  
där COMMIT i Butbuckets namngivning av den komprimerade arkiv-filen
baseras på identifieraren för sista ändringen i kodförrådet för den
släppta versionen (se
<a href="https://bitbucket.org/cancercentrum/rccshiny/commits/branch/master" class="uri">https://bitbucket.org/cancercentrum/rccshiny/commits/branch/master</a>).

Arkiv-filer för olika versioner av paketet finns på
<a href="https://bitbucket.org/cancercentrum/rccshiny/downloads/?tab=tags" class="uri">https://bitbucket.org/cancercentrum/rccshiny/downloads/?tab=tags</a>

Användning
----------

Ladda paketet.

    library(rccShiny)

Läs hjälpfil.

    ?rccShiny::rccShiny2

För fler exempel, se t.ex. R-koden för för Bröstcancerregistrets
interaktiva rapporter
(<a href="http://statistik.incanet.se/brostcancer/" class="uri">http://statistik.incanet.se/brostcancer/</a>)
på
<a href="https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny" class="uri">https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny</a>
.
