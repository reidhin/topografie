# create "Europa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


devtools::load_all()

## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "small_europe.csv"
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
df.nominatim <- wrapper_nominatim(df.input %>% filter(naam %in% namen))

# add to output
df <- bind_rows(
  df %>% filter(!(naam %in% namen)),
  df.nominatim
)

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "small_europe.rds"
  )
)

