# create "Africa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "africa.csv"
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

# Bovenloop nijl toevoegen - Witte Nijl toegevoegd
# Nijldelta toevoegen - is er niet
# vierkant mist in atlantische oceaan -> sargossa zee toegevoegd

# voeg geometriëen samen
df <- df %>%
  st_make_valid() %>%
  group_by(naam, type) %>%
  summarize(geometry=st_union(geometry))

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "africa.rds"
  )
)

