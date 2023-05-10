# create contents dataset

contents <- data.frame(
  school = c("Kandinsky", "NSV2", "NSV2"),
  region = c("Europa", "Noord-Europa", "Europa"),
  filename = c("europe.rds", "northern_europe.rds", "small_europe.rds")
)


# save as rds
saveRDS(
  contents,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "contents.rds"
  )
)
