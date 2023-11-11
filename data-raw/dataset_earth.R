# create "Europa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "earth.csv"
  )
)

# modify some input
df.input <- df.input %>%
  mutate(zoekterm=ifelse(zoekterm=="", naam, zoekterm))

df.database <- load_naturalearth()

## Filter only necessary items
df <- merge(
  df.database,
  df.input,
  by.x = c("type", "name_nl"),
  by.y = c("type", "zoekterm")
)

cat("Niet gevonden:")
cat(setdiff(df.input$naam, df$naam))

# Search in Nominatim
namen <- setdiff(df.input$naam, df$naam)
if (length(namen) > 0) {
  df.nominatim <- wrapper_nominatim(df.input %>% filter(naam %in% namen))

  # add to output
  df <- bind_rows(
    df %>% filter(!(naam %in% namen)),
    df.nominatim
  )
}

# TODO: Noordelijke IJszee centreert op een gekke plek
# TODO: Stille oceaan op twee plekken

# Europa heeft geen Engeland (en andere eilanden)
namen <- c("Europa", "Noorwegen", "Nova Zembla", "IJsland", "Ierland", "Verenigd Koninkrijk", "Spanje", "Italië", "Frankrijk")
df <- bind_rows(
  df %>% filter(!(naam %in% namen)),
  df %>%
    filter(naam %in% namen) %>%
    mutate(naam = "Europa", type="region")
)

# continent Australie bevat geen Nieuw Zeeland
df <- bind_rows(
  df %>% filter(!(naam %in% c("Australië", "Nieuw-Guinea", "Nieuw-Zeeland"))),
  df %>%
    filter(naam %in% c("Australië", "Nieuw-Guinea", "Nieuw-Zeeland")) %>%
    mutate(naam = "Oceanië", type="region")
)

# Indonesie mist in Azie
namen <- c("Azië", "Indonesië", "Filipijnen", "Maleisië")
df <- bind_rows(
  df %>% filter(!(naam %in% namen)),
  df %>%
    filter(naam %in% namen) %>%
    mutate(naam = "Azië", type="region")
)

# Bovenloop Amazone toevoegen -> Ucayali toegevoegd
# Bovenloop jangtze -> Jinsha toegevoegd
# mississippi langer - op kaart staat missouri erbij - Missouri toegevoegd
# Bovenloop nijl toevoegen - Witte Nijl toegevoegd
# Nijldelta toevoegen - is er niet
# vierkant mist in atlantische oceaan -> sargossa zee toegevoegd

# voeg geometriëen samen
df <- df %>%
  group_by(naam, type) %>%
  summarize(geometry=st_union(geometry))

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "earth.rds"
  )
)

