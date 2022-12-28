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
run_app <- function(filename="default.rds", ...) {

  # data folder with the topography datasets
  data_folder <- system.file("dashboard", "data", package="topografie")

  # load dataset options
  datasets <- readRDS(file.path(data_folder, "contents.rds"))

  # find the www map
  www <- system.file("dashboard", "www", package="topografie")
  addResourcePath("www", system.file(file.path("dashboard", "www"), package="topografie"))

  ui <- fluidPage(

    # generic metadata
    tags$head(tags$title("Topografie")),
    tags$head(tags$link(rel="shortcut icon", href="www/favicon.png")),

    # we are using shinyjs
    shinyjs::useShinyjs(),

    # include style file
    includeCSS(file.path(www, "reidhin_style.css")),
    includeScript(file.path(www, "height.js")),
    includeScript(file.path(www, "refocus.js")),

    # add the topographic layout
    topo_ui(datasets)
  )

  server <- function(input, output, session) {

    # set seed
    set.seed(as.integer(Sys.time()))

    # which topo-item is selected
    selected <- reactiveVal(NULL)

    # whether a choice has been made
    topo_item_chosen <- reactiveVal(NULL)

    # number correct and wrong
    scores <- reactiveValues(correct=0, wrong=0)

    # define all indices to go
    names_to_go <- reactiveVal(NULL)

    # topo-filename
    topo.filename <- reactive({
      # get the query
      query <- getQueryString()

      # check if "topo" is in the query
      if ("topo" %in% names(query)) {
        # compose the filename
        temp.filename <- paste0(tolower(query$topo), ".rds")
        # check if the filename exists in the datasets
        if (temp.filename %in% datasets$filename) {
          filename <- temp.filename
        }
      }

      filename
    })


    # reactive with data.frame with topo-items
    df.topo <- reactive({
      # load the map
      readRDS(file.path(data_folder, topo.filename()))
    })


    # update some reactiveVals if needed
    observe({
      # set all indices to go
      names_to_go(unique(df.topo()$naam))

      # define which one is selected
      selected(sample(unique(df.topo()$naam), 1))

      # update answer options
      updateSelectInput(
        session,
        inputId = "dropdown_topo_names",
        choices=c("Kies"= "", sort(unique(df.topo()$naam))),
        selected=""
      )

    })


    # set the title in the sidebarPanel
    output$title <- renderText({
      paste(
        "Topografie", datasets$school[datasets$filename == topo.filename()]
      )
    })


    # show and hide the startup-modal and correct div-s depending on the filename
    observeEvent(topo.filename(), {
      if (topo.filename() == "default.rds") {
        shinyjs::hide("div_topo_names")
        showModal(modal_startup(datasets))
      } else {
        shinyjs::show("div_topo_names")
      }
    })


    # when a dataset is chosen, update the query string
    observeEvent(input$start, ignoreInit = TRUE, {
      # close the modal
      removeModal()

      # start the correct topo-challenge
      updateQueryString(
        queryString = sprintf("?topo=%s", gsub(".rds", "", input$datasets)),
        mode = "push"
      )
    })


    observeEvent(input$go_to_next, {

      # reset selected
      updateSelectInput(session, "dropdown_topo_names", selected="")

      # hide button
      shinyjs::hide("next_item")

      # remove text
      output$text <- renderText("")
      output$correct_answer <- renderText("")

      # select new item
      if (length(names_to_go()) == 0) {
        # klaar!
        showModal(modal_ready(scores))
        names_to_go(unique(df.topo()$naam))
      }
      if (length(names_to_go()) == 1) {
        # last item selected
        selected(names_to_go())
      } else {
        # sample from left over indices
        selected(sample(names_to_go(), 1))
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
          topo_item_chosen(input$dropdown_topo_names)
        }
      }
    )


    # when topo item is chosen, act accordingly
    observeEvent(topo_item_chosen(), {
      if (topo_item_chosen() == isolate(selected())) {
        # goed!
        output$text <- renderText("goed")
        scores$correct <- scores$correct + 1
        temp <- names_to_go()
        names_to_go(temp[temp != selected()])
        shinyjs::show("next_item")
      } else {
        # fout!
        output$text <- renderText("fout")
        output$correct_answer <- renderText(
          sprintf("Het goede antwoord was: %s", selected())
        )
        scores$wrong <- scores$wrong + 1
        shinyjs::show("next_item")
      }
      # refocus
      session$sendCustomMessage("refocus", list("go_to_next"))

    })


    # update the score-texts when necessary
    observeEvent(scores$correct | scores$wrong, {
      output$text_statistics <- renderUI(HTML(statistics.text(scores)))
    })


    # launch colofon when clicked
    observeEvent(input$colofon, showModal(modal_colofon(file.path(www, "colofon.Rmd"))))

    # The initial map with the items
    output$map_euro <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(df.topo() %>% dplyr::filter(.data$type=="country")) %>%
        leaflet::addProviderTiles("Esri.WorldTerrain") %>%
        # leaflet::addProviderTiles("Esri.WorldPhysical") %>%
        leaflet::fitBounds(-5, 40, 15, 70) %>%
        leaflet::addPolygons(
          color = "grey",
          weight = 1,
          # opacity = 1,
          fillColor = "lightblue"
        ) %>%
        leaflet::addCircles(
          data = df.topo() %>% dplyr::filter(.data$type=="city"),
          color = "darkgrey",
          opacity = 1
        ) %>%
        leaflet::addPolylines(
          data = df.topo() %>% dplyr::filter(.data$type=="river"),
          color = "lightblue",
          opacity = 1,
          weight = 1
        )
    })



    # The proxy map to be modified
    observe({

      # obtain the selected item
      df.sel <- df.topo() %>%
        dplyr::filter(.data$naam==selected()) %>%
        dplyr::mutate(sf_type = sf::st_geometry_type(.data$geometry))

      # remove the previous layer
      leaflet::leafletProxy("map_euro") %>%
        leaflet::clearGroup(group = "selected")

      # city
      if ("city" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addCircles(
            color = "blue",
            opacity = 1,
            radius = 2*10^4,
            group = "selected"
          )
      }

      # country
      if ("country" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addPolygons(
            color = "grey",
            weight = 1,
            fillColor = "blue",
            group = "selected"
          )
      }

      # river
      if ("river" %in% df.sel$type) {
        leaflet::leafletProxy("map_euro", data = df.sel) %>%
          leaflet::addPolylines(
            color = "blue",
            opacity = 1,
            weight = 1,
            group = "selected"
          )
      }

      # area, region, sea
      if (any(df.sel$type %in% c("area", "region", "sea"))) {
        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(.data$sf_type=="POINT")
        ) %>%
          leaflet::addCircles(
            color = "blue",
            radius = 10^5,
            opacity = 1,
            weight = 0,
            group = "selected"
          )

        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(grepl("LINESTRING", .data$sf_type))
        ) %>%
          leaflet::addPolylines(
            color = "blue",
            weight = 50,
            opacity = 0.2,
            group = "selected"
          )

        leaflet::leafletProxy(
          "map_euro",
          data = df.sel %>% dplyr::filter(grepl("POLYGON", .data$sf_type))
        ) %>%
          leaflet::addPolygons(
            color = "blue",
            opacity = 1,
            weight = 0,
            group = "selected"
          )
      }

      # fly to appropriate point
      # some polygons are invalid, therefore the when an error is encountered
      # the mean of all the coordinates is taken as poor-mans centroid
      center.coordinates <- tryCatch(
        expr = sf::st_coordinates(sf::st_centroid(sf::st_union(df.sel$geometry))),
        error = function(e) as.vector(colMeans(sf::st_coordinates(sf::st_union(df.sel$geometry))))
      )
      # center.coordinates <- sf::st_coordinates(sf::st_centroid(df.sel$geometry[1]))
      # print(center.coordinates)
      leaflet::leafletProxy("map_euro") %>%
        leaflet::flyTo(lng = center.coordinates[1], lat = center.coordinates[2], zoom=4)

    })


  }

  shinyApp(ui, server, ...)
}
