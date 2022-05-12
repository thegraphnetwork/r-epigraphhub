#' Wrapper function to download daily temperature data from Google Earth to use on epi analysis
#'
#' \code{get_temperature_data} connects to the EpiGraphHub database to pull an spatial polygon from a country and an specific administrative level. Then, it connects to the Google Earth Engine database to pull a dataframe containing daily temperature data for the study area. This function pulls data from MODIS daily Land-Surface Temperature (LST) at 1km grids using the rgee package (MOD11A1 V6.1 product).
#'
#' To read more about MOD11A1 V6.1 product:
#' https://lpdaac.usgs.gov/products/mod11a1v061/
#'
#' To read more about R Google Earth Engine
#' https://cran.r-project.org/web/packages/rgee/
#'
#' @param x an \code{egh_connection} object or a sf object, needed to pull the specified spatial polygon
#' @param id_col if providing a sf object, please specify what is the id column that uniquely specifies each of the spatial objects, such as districts, regions, cities, etc.
#' @param country a three-letter string specifying the country iso code (ISO 3166-1 alfa-3) pull data from. Must be "1", "2" or "3". Please, check https://gadm.org/ for available countries and administrative levels.
#' @param admin_lvl a string specifying the administrative level to pull data from. Must be "1", "2" or "3". Please, check https://gadm.org/ for available countries and administrative levels.
#' @param startDate a string containing the starting date in "yyyy-mm-dd" format for the query.
#' @param endDate a string containing the ending date in "yyyy-mm-dd" format for the query.
#'
#' @section Warning:
#' A Internet connection is needed to use this function. Longer time periods and large datasets can take several minutes to fetch data, especially if using a free account. Adjust expectations accordingly.
#'
#' This functions calls the rgee package. If you never used rgee, please follow the instalation instructions. You will be prompted to create an user account on Google Earth Engine or to connect to an existing one. This is mandatory for rgee to run. Please, go to https://cran.r-project.org/web/packages/rgee/ for more instructions.
#'
#' @examples \dontrun{
#' library(epigraphhub)
#' country = "AGO"
#' admin_lvl = "2"
#' startDate = "2021-01-01"
#' endDate = "2022-01-01"
#' con <- egh_connection(auto_connect = TRUE, use_env = TRUE, path_env = ".env")
#' df <- get_temperature_data(x = con,
#'                            country = country,
#'                            admin_lvl = admin_lvl,
#'                            startDate = startDate,
#'                            endDate = endDate)
#' }
#' @import rgee
#' @import dplyr
#' @import sf
#' @import geojsonio
#' @importFrom raster extent
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect peek_vars
#' @export

get_temperature_data <- function(x,
                                 id_col = NULL,
                                 country = NULL,
                                 admin_lvl = NULL,
                                 startDate,
                                 endDate){

  if (missing(x)) stop("Please, provide an egh_connect() or a sf object.")
  if (missing(startDate)) stop("Dates not provided.")
  if (missing(endDate)) stop("Dates not provided.")
  if (endDate < startDate) stop("Ending date cannot be equal or lower than starting date.")

  # defining if user is using EGH connection or is providing a sf object
  if (class(x)[1] == "PqConnection"){
    if (missing(country)) stop("Country isocode not provided.")
    if (missing(admin_lvl)) stop("Administrative level not provided.")
    if (admin_lvl < 0 | admin_lvl > 3) stop("Administrative level should be equal to 0, 1, 2 or 3.")
    # discovering country and admin lvl
    my_country <- paste0("\"", country, "_", admin_lvl, "\"")
    # pulling geospatial file from the EpiGraphHub database
    df_spatial <- sf::st_read(x,
                              geometry_column = "geometry",
                              query = paste0("SELECT * FROM public.", my_country),
                              crs = 4326)
    # turning into sf object
    df_spatial <- sf::st_as_sf(df_spatial)
    # default id_col to code of the admin lvl
    id_col <- paste0("GID_", admin_lvl)
  } else if (class(x)[1] == "sf"){
    if (!is.null(country)) message("Since you provided a sf object, country isocode is not necessary. Ignoring 'country' argument.")
    if (!is.null(admin_lvl)) message("Since you provided a sf object, administrative level is not necessary. Ignoring 'admin_lvl' argument.")
    if (missing(id_col)) stop("Id column not provided.")

    # if it is already a sf object, just maintain the id_col
    df_spatial <- x
    id_col = id_col
  }

  # initializing rgee
  require(rgee)
  # ee_install()
  ee_Initialize()

  # defining starting and ending dates
  startDate = ee$Date(startDate)
  endDate = ee$Date(endDate)

  # extracting rectangle boundaries to pull data faster
  boundary_xmin <- raster::extent(df_spatial)[1]
  boundary_xmax <- raster::extent(df_spatial)[2]
  boundary_ymin <- raster::extent(df_spatial)[3]
  boundary_ymax <- raster::extent(df_spatial)[4]

  boundary_limits <- ee$Geometry$Rectangle(coords = c(boundary_xmin,
                                                      boundary_ymin,
                                                      boundary_xmax,
                                                      boundary_ymax))

  # calculating the total number of days in the period
  nDays = ee$Number(endDate$difference(startDate,"day"))$round()

  # filtering daily temperature data from MODIS LST day and night
  temp_data_day <- ee$ImageCollection("MODIS/006/MOD11A1") %>%
    ee$ImageCollection$filterBounds(boundary_limits) %>%
    ee$ImageCollection$map(function(x) x$select("LST_Day_1km"))

  temp_data_night <- ee$ImageCollection("MODIS/006/MOD11A1") %>%
    ee$ImageCollection$filterBounds(boundary_limits) %>%
    ee$ImageCollection$map(function(x) x$select("LST_Night_1km"))

  # mapping day temperature for the time period
  temp_data_day_collection = ee$ImageCollection(
    ee$List$sequence(1, nDays)$map(ee_utils_pyfunc(function(n){
      ini <- startDate$advance(n, "days")
      end <- ini$advance(1,"days")
      return(temp_data_day$filterDate(ini, end)$
               select(0)$mean()$
               multiply(0.02)$
               subtract(273.15)$
               set("system:time_start", ini))
    })))

  temp_data_night_collection = ee$ImageCollection(
    # funcao em Python para mapear todos os meses do periodo
    ee$List$sequence(1, nDays)$map(ee_utils_pyfunc(function(n){
      # calculando o offset da data de inicio
      ini <- startDate$advance(n, "days")
      # avancar um mes
      end <- ini$advance(1,"days")
      # filtrando para cada data no periodo, extraindo a media mensal, e convertendo de Kelvins para Celsius
      return(temp_data_night$filterDate(ini, end)$
               select(0)$mean()$
               multiply(0.02)$
               subtract(273.15)$
               set("system:time_start", ini))
    })))

  # extracting data for each spatial unit in df_spatial
  df_temp_day <- ee_extract(x = temp_data_day_collection,
                            y = df_spatial[id_col],
                            sf = FALSE)

  df_temp_night <- ee_extract(x = temp_data_night_collection,
                              y = df_spatial[id_col],
                              sf = FALSE)

  # extracting starting and ending dates of the dataset and constructing a sequence
  date_temp <- temp_data_day$filterDate(start = startDate$args$value, opt_end = endDate$args$value)
  date_temp <- ee_get_date_ic(date_temp)
  date_seq <- seq.Date(min(as.Date(date_temp$time_start)),
                       max(as.Date(date_temp$time_start)),
                       by = 1)

  # how many spatial polygons are in our spatial dataset?
  n_objects <- nrow(df_spatial[id_col])

  # creating a dataframe with dates to be joined into our final object
  df_dates <- data.frame(date = date_seq) %>%
    dplyr::mutate(date_temp = seq(length(date_seq)) - 1)

  # returning a dataframe with all day and night temperature data
  df_final_day <- df_temp_day %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("LST_Day_1km"),
      names_to = "date_temp",
      values_to = "LST_Day_1km"
    ) %>%
    dplyr::mutate(
      date_temp = stringr::str_remove_all(date_temp,
                                          "X|_LST_Day_1km"),
      date_temp = as.numeric(date_temp)
    )

  df_final_night <- df_temp_night %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("LST_Night_1km"),
      names_to = "date_temp",
      values_to = "LST_Night_1km"
    ) %>%
    dplyr::mutate(
      date_temp = stringr::str_remove_all(date_temp,
                                          "X|_LST_Night_1km"),
      date_temp = as.numeric(date_temp)
    )

  df_final <- df_final_day %>%
    dplyr::left_join(df_final_night) %>%
    dplyr::left_join(df_dates) %>%
    dplyr::relocate(date, .before = LST_Day_1km) %>%
    dplyr::select(-date_temp) %>%
    dplyr::arrange(id_col, date)

  return(df_final)

}
