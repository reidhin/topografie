# create contents dataset

contents <- data.frame(
  school = c("Kandinsky", "NSV2"),
  region = c("Europa", "Noord-Europa"),
  filename = c("europe.rds", "north_europe.rds")
)


# save as rds
saveRDS(
  contents,
  file.path(
    system.file("dashboard", "data", package="topografie"),
    "contents.rds"
  )
)
