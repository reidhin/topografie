# dataset utils

load_naturalearth <- function() {
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

  return(df.database)
}
