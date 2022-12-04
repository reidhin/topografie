#' Run topografie shiny app
#'
#' Runs a shiny app that displays a map. The user has to provide the correct answer for the item shown on the map.
#'
#' @param filename The file with geo-info to load into the app
#' @param ... Other parameters to be passed on to shinyApp
#'
#' @return Nothing
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
run_app <- function(filename="europe.rds", ...) {

  # find the www map
  www <- system.file("dashboard", "www", package="topografie")
  addResourcePath("www", system.file(file.path("dashboard", "www"), package="topografie"))

  # load the map
  data_folder <- system.file("dashboard", "data", package="topografie")
  df <- readRDS(file.path(data_folder, filename))

  # add an index
  df$index = 1:nrow(df)

  # all names
  topo.names <- df$naam

  ui <- fluidPage(

    # generic metadata
    tags$head(tags$title("Topografie voor Kandinsky College")),
    tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),

    # include visitor tracker
    tags$head(includeHTML(file.path(www, "geitjes-analytics.html"))),

    # we are using shinyjs
    shinyjs::useShinyjs(),

    # include style file
    includeCSS(file.path(www, "reidhin_style.css")),
    includeScript(file.path(www, "height.js")),

    sidebarLayout(

      # show the interaction in the sidebar
      sidebarPanel(
        width=3,
        style = "height: 100vh; height: calc(var(--vh, 1vh) * 100); display: flex; flex-flow: column; margin-bottom: 0;",

        # Application title
        h2(
          style = "flex: 0 0 auto; margin-bottom: 24px;",
          "Topografie Kandinsky"
        ),

        # drop-down with topo-names
        div(
          style = "flex: 0 0 auto;",
          selectInput(
            inputId="dropdown_topo_names",
            label="Selecteer je antwoord",
            choices=c("Kies"= "", sort(topo.names)),
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
          "Created by Hans Weda \u00A9"
        )
      ),

      # show the map on the main panel
      mainPanel(
        style = "height: 100vh; height: calc(var(--vh, 1vh) * 100);",
        leaflet::leafletOutput("map_euro", width="100%", height="100%"),
        width=9
      )
    )
  )

  server <- function(input, output, session) {

    # set seed
    set.seed(as.integer(Sys.time()))

    # which topo-item is selected
    selected <- reactiveVal(sample(1:length(topo.names), 1))

    # whether a choice has been made
    topo_item_chosen <- reactiveVal(NULL)

    # number correct and wrong
    scores <- reactiveValues(correct=0, wrong=0)

    # all indices to go
    indices_to_go <- reactiveVal(1:nrow(df))


    observeEvent(input$go_to_next, {

      # reset selected
      updateSelectInput(session, "dropdown_topo_names", selected="")

      # hide button
      shinyjs::hide("next_item")

      # remove text
      output$text <- renderText("")
      output$correct_answer <- renderText("")

      # select new item
      if (length(indices_to_go()) == 0) {
        # klaar!
        showModal(modalDialog(
          title = "Klaar!",
          "Alle steden, landen, rivieren, streken en gebergten gehad.",
          br(),
          br(),
          div(
            id = "scores",
            style = "border-width: 1px; border-style: solid; border-color: grey; border-radius: 4px; padding: 8px;",
            HTML(statistics.text(scores))
          ),
          easyClose = TRUE,
          footer = modalButton("Sluiten")
        ))
        indices_to_go(1:nrow(df))
      }
      if (length(indices_to_go()) == 1) {
        # last item selected
        selected(indices_to_go())
      } else {
        # sample from left over indices
        selected(sample(indices_to_go(), 1))
      }

    })


    # adjust the topo_item_chosen depending on dropdown
    observeEvent(
      input$dropdown_topo_names,
      ignoreInit=TRUE,
      {
        if (input$dropdown_topo_names == "") {
          topo_item_chosen(NULL)
        } else {
          topo_item_chosen(which(topo.names==input$dropdown_topo_names))
        }
      }
    )


    # when topo item is chosen, act accordingly
    observeEvent(topo_item_chosen(), {
      if (topo_item_chosen() == isolate(selected())) {
        # goed!
        output$text <- renderText("goed")
        scores$correct <- scores$correct + 1
        temp <- indices_to_go()
        indices_to_go(temp[temp != selected()])
        shinyjs::show("next_item")
      } else {
        # fout!
        output$text <- renderText("fout")
        output$correct_answer <- renderText(
          sprintf("Het goede antwoord was: %s", topo.names[selected()])
        )
        scores$wrong <- scores$wrong + 1
        shinyjs::show("next_item")
      }

    })


    # update the score-texts when necessary
    observeEvent(scores$correct | scores$wrong, {
      output$text_statistics <- renderUI(HTML(statistics.text(scores)))
    })


    # The initial map with the items
    output$map_euro <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(df %>% dplyr::filter(.data$type=="country")) %>%
        leaflet::addProviderTiles("Esri.WorldTerrain") %>%
        leaflet::fitBounds(-5, 40, 15, 70) %>%
        leaflet::addPolygons(
          color = "grey",
          weight = 1,
          fillColor = "lightblue"
        ) %>%
        leaflet::addCircles(
          data = df %>% dplyr::filter(.data$type=="city"),
          color = "darkgrey",
          opacity = 1
        ) %>%
        leaflet::addPolylines(
          data = df %>% dplyr::filter(.data$type=="river"),
          color = "lightblue",
          opacity = 1,
          weight = 1
        )
    })



    # The proxy map to be modified
    observe({

      # obtain the selected item
      df.sel <- df %>%
        dplyr::filter(.data$index==selected()) %>%
        dplyr::mutate(sf_type = sf::st_geometry_type(.data$geometry))

      # remove the previous layer
      leaflet::leafletProxy("map_euro") %>%
        leaflet::removeShape(layerId = "selected")

      # city
      if ("city" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addCircles(
            color = "blue",
            opacity = 1,
            radius = 2*10^4,
            layerId = "selected"
          )
      }

      # country
      if ("country" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addPolygons(
            color = "grey",
            weight = 1,
            fillColor = "blue",
            layerId = "selected"
          )
      }

      # river
      if ("river" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addPolylines(
            color = "blue",
            opacity = 1,
            weight = 1,
            layerId = "selected"
          )
      }

      # area
      if ("area" %in% df.sel$type) {
        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(.data$sf_type=="POINT")
        ) %>%
          leaflet::addCircles(
            color = "blue",
            radius = 10^5,
            opacity = 1,
            weight = 0,
            layerId = "selected"
          )

        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(grepl("LINESTRING", .data$sf_type))
        ) %>%
          leaflet::addPolylines(
            color = "blue",
            weight = 50,
            opacity = 0.2,
            layerId = "selected"
          )

        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(grepl("POLYGON", .data$sf_type))
        ) %>%
          leaflet::addPolygons(
            color = "blue",
            opacity = 1,
            weight = 0,
            layerId = "selected"
          )
      }

      # fly to appropriate point
      center.coordinates <- sf::st_coordinates(sf::st_centroid(df.sel$geometry[1]))
      leaflet::leafletProxy("map_euro") %>%
        leaflet::flyTo(lng = center.coordinates[1], lat = center.coordinates[2], zoom=4)

    })


  }

  shinyApp(ui, server, ...)
}
