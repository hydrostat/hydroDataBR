test_that("Stage 11D keeps station-aware product-all planning contracts", {
  inv <- tibble::tibble(
    station_code = "56460000",
    station_type = "Fluviometrica",
    discharge_start_date = as.Date("2005-01-01"),
    discharge_end_date = as.Date("2010-12-31"),
    stage_start_date = as.Date("2005-01-01"),
    stage_end_date = as.Date("2010-12-31"),
    rainfall_start_date = as.Date(NA),
    rainfall_end_date = as.Date(NA),
    has_discharge_measurements = TRUE,
    has_stage_data = TRUE,
    has_rainfall_data = FALSE,
    is_operating = FALSE
  )

  plan <- hydroDataBR:::ana_station_product_plan(
    station_code = "56460000",
    station_inventory = inv,
    include_cross_sections = FALSE
  )

  attempted <- plan$product[plan$attempt]
  expect_true(all(c("daily_discharge", "daily_stage", "discharge_measurements", "rating_curves") %in% attempted))
  expect_false("daily_rainfall" %in% attempted)
  expect_false("cross_sections" %in% attempted)

  dm <- plan[plan$product == "discharge_measurements", , drop = FALSE]
  rc <- plan[plan$product == "rating_curves", , drop = FALSE]
  expect_identical(dm$date_start[[1L]], as.Date("2005-01-01"))
  expect_identical(dm$date_start_field[[1L]], "discharge_start_date")
  expect_identical(rc$date_end_field[[1L]], "discharge_end_date")
})

test_that("Stage 11D request windows stay limited to one year", {
  products <- c(
    "daily_discharge", "daily_stage", "daily_rainfall",
    "discharge_measurements", "rating_curves", "cross_sections"
  )

  for (product in products) {
    windows <- hydroDataBR:::ana_api_request_windows_for_product(
      product = product,
      date_start = "2000-01-01",
      date_end = "2002-12-31"
    )
    expect_gt(nrow(windows), 1L)
    expect_true(all(as.numeric(windows$date_end - windows$date_start) <= 365))
  }
})

test_that("Stage 11D cross-section aggregate windows can be guided by measurements", {
  measurements <- tibble::tibble(
    station_code = "56460000",
    measurement_datetime = as.POSIXct(c("2005-03-10", "2007-08-12"), tz = "UTC")
  )

  windows <- hydroDataBR:::ana_api_cross_section_windows(
    measurements,
    date_start = as.Date("2005-01-01"),
    date_end = as.Date("2010-12-31"),
    strategy = "measurement_years"
  )

  expect_equal(format(windows$date_start, "%Y"), c("2005", "2007"))
  expect_equal(format(windows$date_end, "%Y"), c("2005", "2007"))
})
