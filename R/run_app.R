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
#' @importFrom stats rnorm
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

    # crs of the projection
    crs <- reactiveVal(
      if (filename %in% datasets$filename) datasets$crs[[which(datasets$filename == filename)]] else leaflet::leafletCRS()
    )

    # The filename of the background
    background.filename <- reactiveVal(
      if (filename %in% datasets$filename) datasets$background[datasets$filename == filename] else "all_european_countries.rds"
    )

    # zoom-factor after flying to certain locations
    zoom.factor <- reactiveVal(
      if (filename %in% datasets$filename) datasets$zoom.factor[datasets$filename == filename] else 3
    )

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
          crs(datasets$crs[[which(datasets$filename == filename)]])
          background.filename(datasets$background[datasets$filename == filename])
          zoom.factor(datasets$zoom.factor[datasets$filename == filename])
        }
      }

      filename
    })

    # reactive with data.frame with topo-items
    df.topo <- reactive({
      # load the map
      readRDS(file.path(data_folder, topo.filename()))
    })

    # reactive to load all countries
    all_countries <- reactive({
      readRDS(file.path(data_folder, background.filename()))
    })

    # update some reactiveVals if needed
    observe({
      # set all indices to go
      names_to_go(unique(df.topo()$naam))

      # define which one is selected
      selected(sample(unique(df.topo()$naam), 1))

      # For debugging purposes:
      #selected("Zeeland")
      #selected("Noordelijke IJszee")

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
      map <- leaflet::leaflet(
        df.topo() %>% dplyr::filter(.data$type=="country"),
        options = leaflet::leafletOptions(
          crs = crs()
        )
      )

      if (crs()$crsClass == "L.CRS.EPSG3857") {
        # the default CRS - in this case we can add Provider Tiles
        map <- map %>%
          leaflet::addProviderTiles("Esri.WorldTerrain", group="Terrain") %>%
          leaflet::addProviderTiles("Esri.WorldShadedRelief", group="Relief") %>%
          leaflet::addProviderTiles("Esri.WorldPhysical", group="Physical") %>%
          leaflet::addProviderTiles("CartoDB.VoyagerNoLabels", group="Voyager") %>%
          # leaflet::addProviderTiles("Stamen.TonerBackground", group="Stamen") %>%
          # leaflet::addProviderTiles("CartoDB.PositronNoLabels", group="Positron") %>%
          # leaflet::fitBounds(-5, 40, 15, 70) %>%
          leaflet::addLayersControl(
            baseGroups = c("Terrain", "Relief", "Physical", "Voyager"),
            options = leaflet::layersControlOptions(collapsed = TRUE)
          ) %>%
          leaflet::addPolygons(
            data=all_countries(),
            color = "grey",
            weight = 1,
            fill = FALSE
          )
      } else {
        map <- map %>%
          leaflet::addGraticule(
            sphere = TRUE,
            style= list(color= '#777', weight= 2, opacity= 1, fillColor= '#ccffff', fillOpacity= .5)
          ) %>%
          leaflet::addGraticule(
            style= list(color= '#999', weight= 0.5, opacity= 1)
          ) %>%
          leaflet::addPolygons(
            data=all_countries(),
            color = "grey",
            weight = 1,
            fillColor = "white",
            fillOpacity = 1
          )
      }

      map <- map %>%
        # leaflet::addPolygons(
        #   data=all_countries,
        #   color = "grey",
        #   weight = 1,
        #   fillColor = "antiquewhite",
        #   fillOpacity = 1,
        #   group = "default"
        # ) %>%
       leaflet::addCircles(
          data = df.topo() %>% dplyr::filter(.data$type=="city"),
          color = "darkgrey",
          opacity = 1
        ) %>%
        leaflet::addCircles(
          data = df.topo() %>% dplyr::filter(.data$type=="city"),
          color = "firebrick",
          opacity = 0.7,
          group = "Physical"
        ) %>%
        leaflet::addPolylines(
          data = df.topo() %>% dplyr::filter(.data$type=="river"),
          color = "lightblue",
          opacity = 1,
          weight = 1
        )


      map
    })

    # add tiles


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
          leaflet::addCircleMarkers(
            color = "blue",
            opacity = 1,
            radius = 10,
            weight = 2,
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

      # area, region, sea, province
      if (any(df.sel$type %in% c("area", "region", "sea", "province"))) {
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

      # simplify the geometry first, such that far-away islands are ignored in:
      # 1. Flying to the appropriate center
      # 2. Calculating an appropriate bounding box
      df.sel.simpl <- sf::st_simplify(df.sel, dTolerance = 10000)
      if (sf::st_is_empty(df.sel.simpl)) {
        df.sel.simpl <- sf::st_simplify(df.sel, dTolerance = 10)
      }

      # fly to appropriate point
      # some polygons are invalid, therefore the when an error is encountered
      # the mean of all the coordinates is taken as poor-mans centroid
      center.coordinates <- tryCatch(
        expr = sf::st_coordinates(sf::st_centroid(sf::st_union(df.sel.simpl$geometry))),
        error = function(e) as.vector(colMeans(sf::st_coordinates(sf::st_union(df.sel.simpl$geometry))))
      )

      # Find size of geometry to determine appropriate zoom factor.
      # The formula is purely heuristic
      bb <-sf::st_bbox(df.sel.simpl)
      diagonal <- sqrt((bb[3]-bb[1])**2 + (bb[4]-bb[2])**2)
      zoomfact <- zoom.factor() * exp(-10*(diagonal/180)) + zoom.factor()


      # Sometimes the tiles and polygons are not aligned after a leaflet::flyto,
      # in particular if the panning distance is small.
      # By adding a random zoom, I hope to avoid this mismatch.
      leaflet::leafletProxy("map_euro") %>%
        leaflet::flyTo(lng = center.coordinates[1], lat = center.coordinates[2], zoom=rnorm(1, mean=zoomfact, sd=0.1))

    })


  }

  shinyApp(ui, server, ...)
}
