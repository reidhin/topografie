# create "Europa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "europe.csv"
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

print("Niet gevonden:")
print(setdiff(df.input$naam, df$naam))

# TODO: Ruhrgebied wordt niet gevonden!
# TODO: Noordelijke IJszee centreert op een gekke plek
# TODO: Straat van Gibraltar ziet er niet goed uit

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "europe.rds"
  )
)

