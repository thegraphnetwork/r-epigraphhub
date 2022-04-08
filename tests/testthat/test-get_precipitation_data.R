testthat::test_that("errors", {

  country = "AGO"
  admin_lvl = "1"
  startDate = "2022-01-01"
  endDate = "2022-02-01"
  id_col = "NAME_2"

  system2("killall ssh")
  con <- egh_connection(auto_connect = TRUE, use_env = FALSE, user = "epigraph", password = "epigraph")
  x <- sf::st_read(con,
                    geometry_column = "geometry",
                    query = paste0("SELECT * FROM public.", "\"AFG_2\""),
                    crs = 4326) %>%
    sf::st_as_sf()

  testthat::expect_error(
    get_precipitation_data(x = con, country = country, admin_lvl = admin_lvl, endDate = endDate),
    "Dates not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = con, country = country, admin_lvl = admin_lvl, startDate = startDate),
    "Dates not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = con, country = country, admin_lvl = admin_lvl, startDate = "2022-02-01", endDate = "2022-01-01"),
    "Ending date cannot be equal or lower than starting date."
  )
  testthat::expect_error(
    get_precipitation_data(x = con, admin_lvl = admin_lvl, startDate = startDate, endDate = endDate),
    "Country isocode not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = con, country = country, startDate = startDate, endDate = endDate),
    "Administrative level not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = con, country = country, admin_lvl = "4", startDate = startDate, endDate = endDate),
    "Administrative level should be equal to 0, 1, 2 or 3."
  )
  testthat::expect_message(
    get_precipitation_data(x = x, country = country, id_col = "NAME_2", startDate = startDate, endDate = endDate),
    "Since you provided a sf object, country isocode is not necessary. Ignoring 'country' argument."
  )
  testthat::expect_message(
    get_precipitation_data(x = x, admin_lvl = admin_lvl, id_col = "NAME_2", startDate = startDate, endDate = endDate),
    "Since you provided a sf object, administrative level is not necessary. Ignoring 'admin_lvl' argument."
  )
  testthat::expect_error(
    get_precipitation_data(x = x, startDate = startDate, endDate = endDate),
    "Id column not provided."
  )

})
