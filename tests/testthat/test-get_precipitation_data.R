testthat::test_that("errors", {

  country = "AGO"
  admin_lvl = "1"
  startDate = "2022-01-01"
  endDate = "2022-02-01"
  id_col = "NAME_2"

  x <- readr::read_rds("data/shape_AGO_2.rds")

  testthat::expect_error(
    get_precipitation_data(x = x, country = country, admin_lvl = admin_lvl, endDate = endDate),
    "Dates not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = x, country = country, admin_lvl = admin_lvl, startDate = startDate),
    "Dates not provided."
  )
  testthat::expect_error(
    get_precipitation_data(x = x, country = country, admin_lvl = admin_lvl, startDate = "2022-02-01", endDate = "2022-01-01"),
    "Ending date cannot be equal or lower than starting date."
  )
  # only needed when using an EGH connection, but can't manage to make it happen during test
  # testthat::expect_error(
  #   get_temperature_data(x = x, admin_lvl = admin_lvl, startDate = startDate, endDate = endDate),
  #   "Country isocode not provided."
  # )
  # testthat::expect_error(
  #   get_temperature_data(x = x, country = country, startDate = startDate, endDate = endDate),
  #   "Administrative level not provided."
  # )
  # testthat::expect_error(
  #   get_temperature_data(x = x, country = country, admin_lvl = "4", startDate = startDate, endDate = endDate),
  #   "Administrative level should be equal to 0, 1, 2 or 3."
  # )
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
