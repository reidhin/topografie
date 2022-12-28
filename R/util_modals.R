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

modal_startup <- function(datasets) {
  modalDialog(
    title = "Kies je topografie opdracht",
    # drop-down with dataset options
    div(
      id = "div_datasets",
      style = "flex: 0 0 auto;",
      radioButtons(
        inputId="datasets",
        label="Kies een optie",
        choiceNames = paste(datasets$region, datasets$school, sep=", "),
        choiceValues = datasets$filename
      )
    ),
    footer = actionButton(inputId = "start", label = "Starten!")
  )
}
