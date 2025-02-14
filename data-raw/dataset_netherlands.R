# create "Netherlands" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "nederland.csv"
  )
)

# modify some input
df.input <- df.input %>%
  mutate(zoekterm=ifelse(zoekterm=="", naam, zoekterm))

## Add provinces
df.provinces <- readRDS(
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "all_provinces.rds"
  )
)

# load database
df.database <- bind_rows(load_naturalearth(), df.provinces)

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
# check https://nominatim.openstreetmap.org/ui/search.html
namen <- setdiff(df.input$naam, df$naam)
if (length(namen) > 0) {
  df.nominatim <- wrapper_nominatim(
    df.input %>% filter(naam %in% namen),
    resolution=0.001
  )

  # add to output
  df <- bind_rows(
    df %>% filter(!(naam %in% namen)),
    df.nominatim
  )
}

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
    "netherlands.rds"
  )
)

devtools::load_all()
run_app("netherlands.rds")

