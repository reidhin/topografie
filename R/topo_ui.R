#' Topo-ui
#'
#' Creates a shiny::sidebarLayout to be used in the topo-app
#'
#' @param datasets The datasets to choose from
#' @param ... Not used
#'
#' @return sidebarLayout
#'
#' @import shiny
#'
#' @export
#'
topo_ui <- function(datasets, ...) {
  sidebarLayout(

    # show the interaction in the sidebar
    div(
      class = "col-xs-3",
      style = "height: 100vh; height: calc(var(--vh, 1vh) * 100); display: flex; flex-flow: column; margin-bottom: 0;",

      # Application title
      h2(
        style = "flex: 0 0 auto; margin-bottom: 24px;",
        textOutput("title")
      ),

      # drop-down with topo-names
      # check options here: https://selectize.dev/docs/usage
      div(
        id = "div_topo_names",
        style = "flex: 0 0 auto; display: none;",
        selectInput(
          inputId="dropdown_topo_names",
          label="Selecteer je antwoord",
          choices=NULL,
          width = "100%"
        )
      ),

      div(
        style = "flex: 1 0 200px;",

        # return correct/wrong
        div(
          style = "text-align: center; margin-bottom: 32px; height: 96px;",
          h2(textOutput("text")),
          p(textOutput("correct_answer"))
        ),

        # Go to next item
        div(
          id="next_item",
          style="display: none;",
          actionButton(
            inputId="go_to_next",
            label="Volgende!",
            style="width: 100%;"
          )
        )
      ),

      div(
        id = "scores",
        style = "border-width: 1px; border-style: solid; border-color: grey; border-radius: 4px; padding: 8px; flex: 0 0 auto",
        htmlOutput("text_statistics")
      ),

      div(
        style = "font-size: xx-small; margin-top: 16px; flex: 0 0 auto",
        actionLink(
          inputId = "colofon",
          "Created by Hans Weda \u00A9"
        )
      )
    ),

    # show the map on the main panel
    div(
      class = "col-xs-9",
      style = "height: 100vh; height: calc(var(--vh, 1vh) * 100);",
      leaflet::leafletOutput("map_euro", width="100%", height="100%")
    )
  )
}
