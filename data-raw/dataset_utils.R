# libraries
library(dplyr)
library(httr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings

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


# function to query nominatim api
get_from_nominatim <- function(query, polygon, resolution, language) {

  # url to use
  osm.base.url <- "http://nominatim.openstreetmap.org/search"

  # Formulate query
  request <- GET(
    url=osm.base.url,
    query = list(
      q=query,
      format="geojson",
      polygon_geojson=polygon,
      polygon_threshold=resolution,
      `accept-language`=language,
      limit=1
    )
  )
  response <- content(request, as="text", encoding="UTF-8")
  print(sprintf("query: %s, status: %s", query, request$status_code))
  out <- geojsonsf::geojson_sf(response)

  return(out)
}


# function to process nominatim
wrapper_nominatim <- function(df.in) {
  # parameters
  resolution <- 0.01
  language <- "nl"

  # check if the input is a city
  df.in <- df.in %>%
    mutate(polygon=ifelse(type=="city", 0, 1))

  # run through data.frame
  out <- data.frame()
  for (i in 1:nrow(df.in)) {
    # get response from nominatim
    temp <- get_from_nominatim(
      query=df.in$zoekterm[i],
      polygon=df.in$polygon[i],
      resolution = resolution,
      language = language
    )
    temp$naam <- df.in$naam[i]
    temp$type <- df.in$type[i]
    out <- rbind(out, temp %>% select(-contains("icon")))

    # a maximum of one request per second is allowed
    Sys.sleep(1)
  }

  # select only necessary columns
  out <- out %>%
    select(type, naam, geometry)

  return(out)
}


