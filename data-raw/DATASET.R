## get geo-locations
# library(tidygeocoder)
library(dplyr)
library(httr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings


# parameters
resolution <- 0.1
language <- "nl"
data_folder <- file.path("inst", "dashboard", "data")


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


# read input
df <- read.csv(file.path(data_folder, "input.csv"))

# modify some output
df <- df %>%
  mutate(polygon=ifelse(type=="city", 0, 1)) %>%
  mutate(zoekterm=ifelse(zoekterm=="", naam, zoekterm))


# run through data.frame
out <- data.frame()
for (i in 1:nrow(df)) {
  # get response from nominatim
  temp <- get_from_nominatim(
    query=df$zoekterm[i],
    polygon=df$polygon[i],
    resolution = resolution,
    language = language
  )
  temp$naam <- df$naam[i]
  temp$type <- df$type[i]
  out <- rbind(out, temp %>% select(-contains("icon")))

  # a maximum of one request per second is allowed
  Sys.sleep(1)
}

# st_geometry_type(temp)
# save as rds
saveRDS(out, file.path(data_folder, "europe.rds"))



# ## read country boundaries
# df.country <- data.frame(naam=c("Nederland", "Duitsland"))
# df.country <- df %>% filter(type=="country") %>% select(naam)
#
# # run through countries
# out <- data.frame()
# for (i in 1:nrow(df.country)) {
#   request <- GET(
#     url=osm.base.url,
#     query = list(
#       country=df.country$naam[i],
#       format="geojson",
#       polygon_geojson=1,
#       polygon_threshold=0.1,
#       `accept-language`="NL",
#       limit=1
#     )
#   )
#   response <- content(request, as="text", encoding="UTF-8")
#   print(sprintf("Country: %s, status: %s", df.country$naam[i], request$status_code))
#   temp <- geojsonsf::geojson_sf(response)
#   temp$naam <- df.country$naam[i]
#   out <- rbind(out, temp)
#
#   # a maximum of one request per second is allowed
#   Sys.sleep(1)
# }
# df.country <- out
#
# # save as rds
# saveRDS(df.country, file.path(data_folder, "country.rds"))
#
# # read cities
# # df.city <- data.frame(city=c("Amsterdam", "Berlijn"))
# df.city <- df %>% filter(type=="city") %>% select(naam)
#
# # run through cities
# out <- data.frame()
# for (i in 1:nrow(df.city)) {
#   request <- GET(
#     url=osm.base.url,
#     query = list(
#       city=df.city$naam[i],
#       format="json",
#       `accept-language`="NL",
#       limit=1
#     )
#   )
#   response <- content(request, as="text", encoding="UTF-8")
#   print(sprintf("City: %s, status: %s", df.city$naam[i], request$status_code))
#   temp <- jsonlite::fromJSON(response)
#   temp$naam <- df.city$naam[i]
#   out <- rbind(out, temp)
#
#   # a maximum of one request per second is allowed
#   Sys.sleep(1)
# }
# df.city <- out
# df.city$lat <- as.numeric(df.city$lat)
# df.city$lon <- as.numeric(df.city$lon)
#
#
# # save as rds
# saveRDS(df.city, file.path(data_folder, "city.rds"))
#
#
#
# #
# # df <- data.frame(city=c("Amsterdam", "Nijmegen"))
# #
# # df2 <- data.frame(address=c("Amsterdam, Nederland", "Rijn"))
# #
# # df3 <- data.frame(country=c("Nederland", "Duitsland"))
# #
# # df3 %>% tidygeocoder::geocode(country, method="osm")
# #
# # out <- df3 %>% tidygeocoder::geocode(country, method="osm", full_results=TRUE, return_type="geographies")
# #
# # # get a geometry using https://nominatim.openstreetmap.org/search?q=rijn&format=geojson&polygon_geojson=1
# #
# # ql <- get_api_query(
# #   "osm",
# #   list(address = "praag"),
# #   list(format="geojson", polygon_geojson=1, limit=1, polygon_threshold=0.5, `accept-language`="nl")
# # )
# # raw1 <- query_api("http://nominatim.openstreetmap.org/search", ql)
# # extract_results("osm", jsonlite::fromJSON(raw1$content))
# # temp <- jsonlite::fromJSON(raw1$content)
# #
# # usethis::use_data(DATASET, overwrite = TRUE)
