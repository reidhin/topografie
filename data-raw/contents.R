# create contents dataset

contents <- data.frame(
  school = c("NSV2 - groep 7", "Kandinsky - klas 2", "NSV2 - groep 7"),
  region = c("Europa", "Europa", "Noord-Europa"),
  filename = c("small_europe.rds", "europe.rds", "northern_europe.rds")
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
