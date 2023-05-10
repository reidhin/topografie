# create "Noord-Europa" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))

devtools::load_all()

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

# load the database
df.database <- load_naturalearth()

## Filter only necessary items
df <- merge(
  df.database,
  df.input,
  by.x = c("type", "name_nl"),
  by.y = c("type", "zoekterm")
)


# TODO: merge Denemarken into Scandinavië
# Puur geografisch gezien is Scandinavië het gebied dat overeenkomt met het Scandinavisch Schiereiland,
# dat voor het grootste deel bestaat uit het Scandinavisch Hoogland. Dit betreft de landen Noorwegen en Zweden.
# Politiek gezien bestaat Scandinavië uit Denemarken, Noorwegen en Zweden.
# Dit is de definitie die in Scandinavië zelf wordt gehanteerd en men kan aan de hand van deze definitie stellen
# dat Scandinavië een deel van "Norden" is (zie derde definitie)
# https://nl.wikipedia.org/wiki/Scandinavi%C3%AB
# add to output
df <- bind_rows(
  df %>% filter(!(naam %in% "Scandinavië")),
  df %>%
    filter(naam %in% c("Noorwegen", "Zweden", "Finland", "Denemarken")) %>%
    mutate(naam = "Scandinavië", type="region")
)


print("Niet gevonden:")
print(setdiff(df$naam, df.input$naam))

# save as rds
saveRDS(
  df,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "northern_europe.rds"
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


