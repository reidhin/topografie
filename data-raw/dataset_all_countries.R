# create "All countries" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


# load sovereign countries
df <- ne_download(type="sovereignty", category="cultural", returnclass="sf", scale="large") %>%
  filter(grepl("Sovereign", TYPE)) %>%
  select(NAME_NL, geometry) %>%
  rename(naam = NAME_NL) %>%
  mutate(type="country")

# Rusland klopt niet omdat deze over de datumgrens heen gaat

# voeg geometriëen samen
df <- bind_rows(
  df %>% filter(naam!="Egypte"),
  df %>% filter(naam=="Egypte") %>% st_make_valid()
)

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "sovereignties.rds"
  )
)

