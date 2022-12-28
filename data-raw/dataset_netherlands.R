# create "Netherlands" dataset
library(rnaturalearth)
library(dplyr)
library(sf)  # important to have such that list are returned as polygons or multilinestrings
source(file.path("data-raw", "dataset_utils.R"))


df.database <- load_naturalearth()

# Few enries are not valid
# Egypte, Zuidelijke Oceaan, Rosszee
valid.idx <- st_is_valid(df.database)

# vind Nederland
nl <- df.database %>% filter(name_nl=="Nederland" & type=="country")

# find the overlap
idx <- st_intersects(df.database[valid.idx, ], nl, sparse=FALSE)

df.nl <- df.database[valid.idx, ][idx, ]
df.nl <- df.nl %>%
  mutate(naam=name_nl)

# save as rds
saveRDS(
  list(df=df.nl, viewbox=c(5, 50, 10, 55), zoom=6),
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "netherlands.rds"
  )
)

