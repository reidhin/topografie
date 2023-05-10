# create contents dataset

contents <- data.frame(
  school = c("NSV2", "Kandinsky", "NSV2"),
  region = c("Europa", "Europa", "Noord-Europa"),
  filename = c("small_europe.rds", "europe.rds", "northern_europe.rds")
)


# save as rds
saveRDS(
  contents,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "contents.rds"
  )
)
