
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Topografie

<!-- badges: start -->
<!-- badges: end -->

Dit R-pakket bevat een app die je de topografie laat oefenen. De app is
eveneens gepubliceerd op <https://reidhin.shinyapps.io/topografie/>

De app maakt gebruik van de topografiën die op
[nominatim](https://nominatim.org/) en
[naturalearthdata](https://www.naturalearthdata.com/) gepubliceerd zijn.

## Installatie

Indien je de app lokaal wilt draaien op je eigen computer, dan kun je
dit pakket installeren als:

``` r
require("devtools")
devtools::install_github("reidhin/topografie")
```

## Voorbeeld

Na de installatie van dit R-pakket kun je de app als volgt opstarten:

``` r
library(topografie)
run_app()
```

Indien je een speciale versie van de app wil opstarten (bijvoorbeeld als
je de app verder wilt ontwikkelen), dan kun je een andere bestandsnaam
meegeven aan de app:

``` r
library(topografie)
run_app("europe2.rds")
```

Deze functionaliteit wordt gebruikt om ook andere werelddelen te kunnen
oefenen.

## Datasets

De gebruikte datasets worden gemaakt vanuit input bestanden in \*.csv
formaat. Hierin staat een lijst met topografie-namen van steden, landen,
rivieren, etc. samen met het type topografie, zoals city, country, etc.
Deze lijst wordt gebruikt om de grenzen en posities op te halen vanuit
[nominatim](https://nominatim.org/) en
[naturalearthdata](https://www.naturalearthdata.com/). De gebruikte
scripts staan in de folder `data-raw`. De gemaakte datasets worden in de
folder `inst/dashboard/data` geplaatst. Hierin staat ook een
`contents.rds` bestand dat door de app wordt gebruikt om een overzicht
van de beschikbare bestanden te presenteren. Indien je een nieuwe
dataset wil toevoegen kun je, nadat je de code hebt gecloned, in deze
folders nieuwe scripts aanmaken.

Zoals hierboven vermeld worden externe bronnen gebruikt voor het bepalen
van lands- en gebiedsgrenzen. In sommige gevallen kunnen de grenzen
betwist zijn, bijvoorbeeld vanwege (inter)nationale conflicten. Daarom
kunnen er afwijkende grenzen voorkomen in de dataset.

In sommige gevallen kunnen er ook verschillende interpretaties zijn van
een geografisch gebied. Het gebied ‘Scandinavië’ kent bijvoorbeeld
verschillende
[definities](https://nl.wikipedia.org/wiki/Scandinavi%C3%AB): (1)
geografisch met enkel Noorwegen en Zweden, (2) politiek met Denemarken,
Noorwegen en Zweden, (3) als ‘Noordse’ landen waarbij ook IJsland en
Finland horen.

In deze gevallen wordt gekozen voor een gangbare definitie die aansluit
bij de schoolopdracht.

## Project organisatie

    ├── app.R                               <- Script om de app juist op te starten op shinyapps.io
    ├── data-raw                            <- Scripts om de datasets te maken
    ├── DESCRIPTION                         <- Project beschrijving
    ├── inst
    │   ├── dashboard
    │   │   ├── data                        <- Folder met de datasets voor het dashboard
    │   │   └── www                         <- Folder met style-files, javascript en favicon
    │   └── extdata                         <- Folder met input *.csv bestanden voor de datasets
    ├── LICENSE.md                          <- Licentie
    ├── man                                 <- Help-bestanden met uitleg over de functies
    │   ├── run_app.Rd
    │   └── topo_ui.Rd
    ├── NAMESPACE
    ├── NEWS.md
    ├── R                                
    │   ├── run_app.R                       <- Hoofdfunctie die de app opstart
    │   ├── topo_ui.R                       <- Functies voor de gebruikersinterface
    │   ├── util_modals.R                   <- Functies voor de gebruikte modals
    │   └── utils.R                         <- Overige hulpfuncties
    ├── README.md
    ├── README.Rmd                          <- README met uitleg en beschrijving van het project
    ├── rsconnect                           <- Folder met hulpbestanden om de app op shinyapps te publiceren
    └── topografie.Rproj

## Licentie

<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl"><img alt="Creative Commons-Licentie" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />Dit
werk valt onder een
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl">Creative
Commons Naamsvermelding-NietCommercieel-GelijkDelen 4.0
Internationaal-licentie</a>.
