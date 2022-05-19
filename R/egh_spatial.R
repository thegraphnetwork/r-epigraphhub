#' Function designed to help user explore and visualize their spatial epi analysis in an interactive way.
#'
#' \code{egh_spatial} was designed to help user explore and visualize their spatial epi analysis in an interactive way. It allows to visualize neighbourhood contiguity between polygons, explore their spatial data and visualize some common statistical tests for spatial data. This function relies heavily on other spatial analysis functions included in other packages. User can either load a spatial polygon file (.rds, .gpkg. or .shp files) or provide a sf or sp object to the function.
#'
#' It currently only allows analysis and visualization of static datasets (e.g., it does not allow datasets with multiple repetitions, like in a time series).
#'
#' The following will soon be implemented: plotting choropleth map of the dependent variable, plotting a choropleth map of Empirical Bayesian Estimates (global and local), returning Moran's I statistic, running a LISA showing scatterplot and map, running Getis-Ord analysis, running a SaTScan analysis.
#'
#' @param x a shapefile, geopackage file, a RDS file, a sf object or a sp object containing the spatial polygons for analysis.
#' @param is_file is x a spatial polygon file (.shp, .gpkg or .rds)? Mark FALSE for sf or sp objects.
#' @param area the variable in x that contains the vector of names of the polygons.
#' @param outcome the outcome variable that is included in x to be analyzed.
#'
#' Thanks to https://github.com/walkerke for the inspiration and published code on how to highlight neighbours.
#'
#' @section Warning:
#' Calculations might take a while in slower computers. When exploring the different spatial contiguity functions, the higher the number of neighbours and distance the slower the computation time can become.
#'
#' @examples \dontrun{
#' library(epigraphhub)
#' library(readr)
#' library(sf)
#'
#' x <- read_rds('shape_AGO_2.rds')
#'
#' egh_spatial(x = x, area = "NAME_2")
#' }
#' @import magrittr
#' @import readr
#' @import sf
#' @import dplyr
#' @import spdep
#' @import sp
#' @import shiny
#' @import leaflet
#' @export

egh_spatial <- function(x,
                        is_file = FALSE,
                        area,
                        outcome = NULL){

  if (missing(x)) stop("Please, provide a shapefile, geopackage file, RDS file, a sf object, or a sp object.")

  if (missing(is_file)) stop("Please, inform if x object is a file or a sf/sp object.")

  if (missing(area)) stop("Please, assign what is the variable that defines the name of each polygon.")

  if (is_file == TRUE & length(x) > 1) {
    stop("It seems that x is an object in the environment. Please check if it is a sf or a sp object and use 'is_file = FALSE' in the main function.")
  }

  if (is_file == FALSE & length(x) == 1) {
    stop("It seems that x is not a sp or sf object. Please check if it is a file and use 'is_file = TRUE' in the main function.")
  }

  if (is_file == TRUE) {
    if (stringr::str_detect(x, pattern = ".rds") == TRUE) {
      x <- readr::read_rds(x)

    } else if (stringr::str_detect(x, pattern = ".gpkg") == TRUE) {
      x <- sf::st_read(x)

    } else if (stringr::str_detect(x, pattern = ".shp") == TRUE) {
      x <- sf::st_read(x)

    }

  } else if (is_file == FALSE) {
    if (grepl("spatial", class(x)[1], ignore.case = TRUE)) {
      x <- sf::st_as_sf(x)

    }  else if (!grepl("sf", class(x)[1], ignore.case = TRUE)) {
      stop("Object is neither a sp or a sf object.")

    }
  }

  x <- dplyr::mutate(.data = x, id = dplyr::row_number())

  sf::st_crs(x) <- "EPSG:4326"

  x_centroid <- sf::st_centroid(x)

  x_rownames <- dplyr::pull(sf::st_drop_geometry(x[get("area")]))

  area_ids <- sf::st_drop_geometry(x) %>%
    dplyr::select(id, get("area"))

  # Queen's case spatial weights
  nb_queen <- spdep::poly2nb(x, queen = TRUE, row.names = x$id)

  df_queen <- as(spdep::nb2lines(nb_queen, coords = sp::coordinates(as(x, "Spatial"))), "sf") %>%
    sf::st_set_crs(sf::st_crs(x)) %>%
    dplyr::left_join(area_ids, by = c("i" = "id")) %>%
    dplyr::mutate(i_ID = get(area)) %>%
    dplyr::select(-get("area")) %>%
    dplyr::left_join(y = area_ids, by = c("j" = "id")) %>%
    dplyr::mutate(j_ID = get(area)) %>%
    dplyr::select(-get("area"))

  # Rook's case spatial weights
  nb_rook <- spdep::poly2nb(x, queen = FALSE, row.names = x$id)

  df_rook <- as(spdep::nb2lines(nb_rook, coords = sp::coordinates(as(x, "Spatial"))), "sf") %>%
    sf::st_set_crs(sf::st_crs(x)) %>%
    dplyr::left_join(area_ids, by = c("i" = "id")) %>%
    dplyr::mutate(i_ID = get(area)) %>%
    dplyr::select(-get("area")) %>%
    dplyr::left_join(area_ids, by = c("j" = "id")) %>%
    dplyr::mutate(j_ID = get(area)) %>%
    dplyr::select(-get("area"))


  # Shiny dashboard for visualization ----------------------------------------------------------------

  # ui
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          'radio',
          label = 'Select a neighbourhood contiguity type:',
          choices = list(
            "Queen's case contiguity" = 1,
            "Rook's case contiguity" = 2,
            'K-nearest neighbors' = 3,
            'Distance' = 4
          ),
          selected = 1
        ),
        shiny::conditionalPanel(
          condition = "input.radio == 3",
          shiny::sliderInput(
            "knn_slider",
            'Select number of neighbours',
            min = 1,
            max = nrow(x) - 1,
            value = 8)
        ),
        shiny::conditionalPanel(
          condition = "input.radio == 4",
          shiny::sliderInput(
            "dist_slider",
            "Select a distance threshold (in degrees)",
            min = 0.1,
            max = 30,
            step = 0.1,
            value = 1)
        )),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Neighbours exploration",
            leaflet::leafletOutput("map",
                                   width = '100%',
                                   height = 800)
          ),
          shiny::tabPanel(
            "Neighbourhood structure",
            leaflet::leafletOutput("nb_map",
                                   width = "100%",
                                   height = 800)
          )))))

  server <- function(input, output) {

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(x) %>%
        leaflet::addProviderTiles('CartoDB.Positron') %>%
        leaflet::addPolygons(layerId = ~id,
                             fillColor = 'transparent',
                             color = '#000000',
                             weight = 0.5,
                             smoothFactor = 0.1)

      map
    })

    click_tract <- shiny::eventReactive(input$map_shape_click, {
      return(input$map_shape_click$id)
    })

    focal_tract <- shiny::reactive({
      req(click_tract())
      return(x[x$id == click_tract(), ])
    })

    # Distance-based spatial weights

    x_centroid <- sf::st_centroid(x)
    # x2_centroid <- sf::st_centroid(x2)

    knn <- shiny::reactive({
      if (input$radio == 3) {
        nb_knn <- spdep::knearneigh(x_centroid, k = input$knn_slider, longlat = TRUE)
        return(nb_knn$nn)

      } else {
        return(NULL)
      }
    })

    dist <- shiny::reactive({
      if (input$radio == 4) {
        nb_dist <- spdep::dnearneigh(x_centroid, 0, input$dist_slider, longlat = TRUE)
        return(nb_dist)

      } else {
        return(NULL)
      }
    })

    neighbors <- shiny::reactive({
      req(click_tract())
      if (input$radio == 1) {
        return(x[x$id %in% nb_queen[[click_tract()]], ])

      } else if (input$radio == 2) {
        return(x[x$id %in% nb_rook[[click_tract()]], ])

      } else if (input$radio == 3) {
        v <- knn()[click_tract(), ]
        return(x[x$id %in% v, ])

      } else if (input$radio == 4) {
        v <- dist()[[click_tract()]]
        if (v == 0) {
          return(NULL)

        } else {
          return(x[x$id %in% v, ])
        }
      }
    })

    shiny::observe({
      req(click_tract())
      proxy <- leaflet::leafletProxy('map')
      if (!is.null(neighbors())) {
        proxy %>%
          leaflet::removeShape('focal') %>%
          leaflet::clearGroup('neighbors') %>%
          leaflet::addPolygons(data = neighbors(),
                               fill = FALSE,
                               color = '#0000FF',
                               group = 'neighbors',
                               opacity = 1) %>%
          leaflet::addPolygons(data = focal_tract(),
                               color = '#00FFFF',
                               opacity = 1,
                               layerId = 'focal',
                               fillColor = 'transparent')

      } else {
        proxy %>%
          leaflet::removeShape('focal') %>%
          leaflet::clearGroup('neighbors') %>%
          leaflet::addPolygons(data = focal_tract(),
                               color = '#00FFFF',
                               opacity = 1,
                               layerId = 'focal',
                               fillColor = 'transparent')
      }
    })

    # Neighbourhood structure map

    output$nb_map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(x) %>%
        leaflet::addProviderTiles('CartoDB.Positron') %>%
        leaflet::addPolygons(layerId = ~id,
                             fillColor = 'transparent',
                             color = '#000000',
                             weight = 0.5,
                             smoothFactor = 0.1)

      map

      if (input$radio == 1) {
        map %>%
          leaflet::addPolylines(data = df_queen,
                                group = "matrix",
                                color = '#000000',
                                weight = 2)

      } else if (input$radio == 2) {
        map %>%
          leaflet::addPolylines(data = df_rook,
                                group = "matrix",
                                color = '#000000',
                                weight = 2)

      } else if (input$radio == 3) {
        nb_knn <- spdep::knearneigh(x_centroid, k = input$knn_slider, longlat = TRUE)

        nb_knn2 <- spdep::knn2nb(nb_knn)

        df_knn <- as(spdep::nb2lines(nb_knn2, coords = sp::coordinates(as(x, "Spatial"))), "sf") %>%
          # st_set_crs(nb_k, st_crs(x)) %>%
          dplyr::left_join(area_ids, by = c("i" = "id")) %>%
          dplyr::mutate(i_ID = get(area)) %>%
          dplyr::select(-get("area")) %>%
          dplyr::left_join(area_ids, by = c("j" = "id")) %>%
          dplyr::mutate(j_ID = get(area)) %>%
          dplyr::select(-get("area"))

        map %>%
          leaflet::addPolylines(data = df_knn,
                                group = "matrix",
                                color = '#000000',
                                weight = 2)

      } else if (input$radio == 4) {
        nb_dist <- spdep::dnearneigh(x_centroid, 0, input$dist_slider, longlat = TRUE)

        df_dist <- as(spdep::nb2lines(nb_dist, coords = sp::coordinates(as(x, "Spatial"))), "sf") %>%
          # st_set_crs(nb_k, st_crs(x)) %>%
          dplyr::left_join(area_ids, by = c("i" = "id")) %>%
          dplyr::mutate(i_ID = get(area)) %>%
          dplyr::select(-get("area")) %>%
          dplyr::left_join(area_ids, by = c("j" = "id")) %>%
          dplyr::mutate(j_ID = get(area)) %>%
          dplyr::select(-get("area"))

        map %>%
          leaflet::addPolylines(data = df_dist,
                                group = "matrix",
                                color = '#000000',
                                weight = 2)
      }
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}

