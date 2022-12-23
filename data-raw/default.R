# create default dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings

## read input
df.input <- data.frame(
  naam = "Nederland",
  type = "country",
  zoekterm = "Nederland"
)

# modify some input
df.input <- df.input %>%
  mutate(zoekterm=ifelse(zoekterm=="", naam, zoekterm))


## read databases

# countries
countries <- ne_download(type="countries", category="cultural", returnclass="sf", scale="large") %>%
  select(NAME_NL, geometry) %>%
  rename(name_nl=NAME_NL) %>%
  mutate(type="country")

## Filter only necessary items
df <- merge(
  countries,
  df.input,
  by.x = c("type", "name_nl"),
  by.y = c("type", "zoekterm")
)


# group and aggregate
df <- df %>%
  group_by(type, naam) %>%
  summarise(geometry=st_union(geometry)) %>%
  ungroup()

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "default.rds"
  )
)

