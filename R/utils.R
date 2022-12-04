# function that provides text with scores
statistics.text <- function(s) {
  # score
  total = s$correct + s$wrong

  # calculate percentage if needed
  if (total > 0) {
    perc <- sprintf("Score: %4.1f%%", 100*s$correct/total)
  } else {
    perc <- "Nog geen score"
  }

  # create output to be used as HTML
  paste(
    sprintf("Aantal goed: %d", s$correct),
    sprintf("Aantal fout: %d", s$wrong),
    perc,
    sep = "<br/>"
  )
}
