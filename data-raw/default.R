# create default dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings

## read input
df.input <- data.frame(
  naam = c("Nederland"),
  type = "country",
  zoekterm = c("Nederland")
)

# modify some input
df.input <- df.input %>%
  mutate(zoekterm=ifelse(zoekterm=="", naam, zoekterm))


## read databases

# countries
countries <- ne_download(type="countries", category="cultural", returnclass="sf", scale="large")

europe <- countries %>%
  filter(CONTINENT=="Europe") %>%
  select(NAME_NL, geometry) %>%
  rename(name_nl=NAME_NL) %>%
  mutate(type="country")

countries <- countries %>%
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

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "default.rds"
  )
)

saveRDS(
  countries,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "all_countries.rds"
  )
)

saveRDS(
  europe,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "all_european_countries.rds"
  )
)

