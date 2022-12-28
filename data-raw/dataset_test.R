# create "Test" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


## read input
df.input <- data.frame(
  naam = c("Engeland", "Londen"),
  type = c("region", "city"),
  zoekterm = c("Engeland", "Londen")
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

# TODO: Noordelijke IJszee centreert op een gekke plek

# Search in Nominatim
namen <- c(setdiff(df.input$naam, df$naam))
if (length(namen) > 0) {
  df.nominatim <- wrapper_nominatim(df.input %>% filter(naam %in% namen))

  # add to output
  df <- bind_rows(
    df %>% filter(!(naam %in% namen)),
    df.nominatim
  )
}


# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "test.rds"
  )
)

