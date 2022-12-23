# create "Noord-Europa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings

## read input
df.input <- read.csv(
  file.path(
    system.file("extdata", package="topografie"),
    "north_europe.csv"
  )
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

# part of countries such as Wales, Scotland
sub_countries <- ne_download(type="map_subunits", category="cultural", returnclass="sf", scale="large") %>%
  select(NAME_NL, geometry) %>%
  rename(name_nl=NAME_NL) %>%
  mutate(type="region")

# # provincies
# provinces <- ne_download(type="admin_1_states_provinces", category="cultural", returnclass="sf", scale="large") %>%
#   select(name_nl, geometry) %>%
#   mutate(type="region")


# cities (+ capitals?)
cities <- ne_download(type="populated_places", category="cultural", returnclass="sf", scale="large") %>%
  select(FEATURECLA, POP_MAX, NAME_NL, geometry) %>%
  rename(name_nl=NAME_NL) %>%
  mutate(type="city")

# rivers (+ lakes)
rivers <- ne_download(type="rivers_lake_centerlines", category="physical", returnclass="sf", scale="large") %>%
  select(name_nl, geometry) %>%
  mutate(type="river")

# regions (Not provinces!)
regions <- ne_download(type="geography_regions_polys", category="physical", returnclass="sf", scale="large") %>%
  filter(REGION=="Europe") %>%
  select(NAME_NL, geometry) %>%
  rename(name_nl=NAME_NL) %>%
  mutate(type="region")

# marine areas
seas <- ne_download(type="geography_marine_polys", category="physical", returnclass="sf", scale="large") %>%
  select(name_nl, geometry) %>%
  mutate(type="sea")

# in case of duplicates, keep largest city
cities <- cities %>%
  group_by(name_nl) %>%
  arrange( desc(as.numeric(POP_MAX) ) ) %>%
  slice(1) %>%
  ungroup()

# join all items together
df.database <- bind_rows(countries, sub_countries, cities, rivers, regions, seas)

## Filter only necessary items
df <- merge(
  df.database,
  df.input,
  by.x = c("type", "name_nl"),
  by.y = c("type", "zoekterm")
)


# group and aggregate
df <- df %>%
  group_by(type, naam) %>%
  summarise(geometry=st_union(geometry)) %>%
  ungroup()

# TODO: merge Denemarken into Scandinavië
# Puur geografisch gezien is Scandinavië het gebied dat overeenkomt met het Scandinavisch Schiereiland,
# dat voor het grootste deel bestaat uit het Scandinavisch Hoogland. Dit betreft de landen Noorwegen en Zweden.
# Politiek gezien bestaat Scandinavië uit Denemarken, Noorwegen en Zweden.
# Dit is de definitie die in Scandinavië zelf wordt gehanteerd en men kan aan de hand van deze definitie stellen
# dat Scandinavië een deel van "Norden" is (zie derde definitie)
# https://nl.wikipedia.org/wiki/Scandinavi%C3%AB



# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "north_europe.rds"
  )
)


# # calculate distance
# df <- df %>%
#   rowwise() %>%
#   mutate(distance = adist(name_nl, naam)) %>%
#   ungroup()
#
# ## read country boundaries
# df.country <- data.frame(naam=c("Nederland", "Duitsland"))
# df.country <- df %>% filter(type=="country") %>% select(naam)
#
# apply(adist(europe$NAME_NL, df.country$naam), 2, which.min)


