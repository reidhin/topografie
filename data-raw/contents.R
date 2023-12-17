# create contents dataset

contents <- list(
  school = c("NSV2 - groep 7", "Kandinsky - klas 2", "NSV2 - groep 7", "NSV2 - groep 8", "NSV2 - groep 8"),
  region = c("Europa", "Europa", "Noord-Europa", "Aarde", "Afrika"),
  filename = c("small_europe.rds", "europe.rds", "northern_europe.rds", "earth.rds", "africa.rds"),
  crs = list(
    leaflet::leafletCRS(),
    leaflet::leafletCRS(),
    leaflet::leafletCRS(),
    leaflet::leafletCRS(
      crsClass="L.Proj.CRS",
      code='ESRI:53009',
      proj4def= '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
      resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)
    ),
    leaflet::leafletCRS()
  ),
  background = c(
    "all_european_countries.rds",
    "all_european_countries.rds",
    "all_european_countries.rds",
    "all_countries.rds",
    "all_countries.rds"
  )
)

# load the package
devtools::load_all()


# save as rds
saveRDS(
  contents,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "contents.rds"
  )
)
