testthat::test_that("errors", {

  testthat::expect_error(
    egh_spatial(is_file = TRUE, area = "NAME_2"),
    "Please, provide a shapefile, geopackage file, RDS file, a sf object, or a sp object."
    )

  testthat::expect_error(
    egh_spatial(x = "data/shape_AGO_2.rds", area = "NAME_2"),
    "Please, inform if x object is a file or a sf/sp object."
  )

  testthat::expect_error(
    egh_spatial(x = "data/shape_AGO_2.rds", is_file = TRUE),
    "Please, assign what is the variable that defines the name of each polygon."
  )

  testthat::expect_error(
    egh_spatial(x = readr::read_rds("data/shape_AGO_2.rds"), is_file = TRUE, area = "NAME_2"),
    "It seems that x is an object in the environment. Please check if it is a sf or a sp object and use 'is_file = FALSE' in the main function."
  )

  testthat::expect_error(
    egh_spatial(x = "data/shape_AGO_2.rds", is_file = FALSE, area = "NAME_2"),
    "It seems that x is not a sp or sf object. Please check if it is a file and use 'is_file = TRUE' in the main function."
  )

  testthat::expect_error(
    egh_spatial(x = rep(1, 10), is_file = FALSE, area = "NAME_2"),
    "Object is neither a sp or a sf object."
  )

})

