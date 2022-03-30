testthat::test_that("errors", {

    df <- data.frame(date = rep(seq(as.Date("2022-01-01"), by = "day", length.out = 60), 2),
                   cases = rep(sample.int(n = 100, size = 60, replace = TRUE), 2),
                   region = c(rep("Region A", 60), rep("Region B", 60)))

  testthat::expect_error(
    df_epinow(cases = cases, date = date, regional = FALSE),
    "Dataset not provided."
  )
  testthat::expect_error(
    df_epinow(x = df, date = date, regional = FALSE),
    "Vector of cases not provided."
  )
  testthat::expect_error(
    df_epinow(x = df, cases = cases, regional = FALSE),
    "Vector of dates not provided."
  )
  testthat::expect_error(
    df_epinow(x = df, cases = cases, date = date, regional = TRUE),
    "Vector of regions not provided."
  )

})
