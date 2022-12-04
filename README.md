
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Topografie

<!-- badges: start -->
<!-- badges: end -->

Dit R-pakket bevat een app die je de topografie van Europa laat oefenen.
De app is eveneens gepubliceerd op
<https://reidhin.shinyapps.io/topografie/>

De app maakt gebruik van de topografiën die op <https://nominatim.org/>
gepubliceerd zijn.

## Installatie

Indien je de app lokaal wilt draaien op je eigen computer, dan kun je
dit pakket installeren als:

``` r
require("devtools")
devtools::install_github(")
```

## Voorbeeld

Na de installatie van dit R-pakket kun je de app als volgt opstarten:

``` r
library(topografie)
run_app()
```

Indien je een korte versie van de app wil opstarten (bijvoorbeeld als je
de app verder wilt ontwikkelen), dan kun je een andere bestandsnaam
meegeven aan de app:

``` r
library(topografie)
run_app("europe2.rds")
```

In de toekomst zal deze functionaliteit gebruikt worden om ook andere
werelddelen te kunnen oefenen.

## Licentie

<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl"><img alt="Creative Commons-Licentie" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />Dit
werk valt onder een
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl">Creative
Commons Naamsvermelding-NietCommercieel-GelijkDelen 4.0
Internationaal-licentie</a>.
