# This consists of a list of modals
modal_ready <- function(s) {
  modalDialog(
    title = "Klaar!",
    "Alle steden, landen, rivieren, streken en gebergten gehad.",
    br(),
    br(),
    div(
      id = "scores",
      style = "border-width: 1px; border-style: solid; border-color: grey; border-radius: 4px; padding: 8px;",
      HTML(statistics.text(s))
    ),
    easyClose = TRUE,
    footer = modalButton("Sluiten")
  )
}

modal_colofon <- function(filename) {
  modalDialog(
    title = "Colofon",
    includeMarkdown(filename),
    easyClose = TRUE,
    footer = modalButton("Sluiten")
  )
}
