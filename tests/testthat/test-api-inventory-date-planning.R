test_that("product all uses discharge dates for fluviometric specialized products", {
  inv <- tibble::tibble(
    station_code = "56460000",
    station_type = "Fluviometrica",
    discharge_start_date = as.Date("1965-10-01"),
    discharge_end_date = as.Date("2018-12-01"),
    stage_start_date = as.Date("1965-10-01"),
    stage_end_date = as.Date("2018-12-01"),
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
  dm <- plan[plan$product == "discharge_measurements", , drop = FALSE]
  rc <- plan[plan$product == "rating_curves", , drop = FALSE]
  expect_identical(dm$date_start[[1L]], as.Date("1965-10-01"))
  expect_identical(dm$date_end[[1L]], as.Date("2018-12-01"))
  expect_identical(dm$date_start_field[[1L]], "discharge_start_date")
  expect_identical(rc$date_start_field[[1L]], "discharge_start_date")
})

test_that("rainfall products are skipped for fluviometric stations without rainfall availability", {
  inv <- tibble::tibble(
    station_code = "56460000",
    station_type = "Fluviometrica",
    discharge_start_date = as.Date("1965-10-01"),
    discharge_end_date = as.Date("2018-12-01"),
    rainfall_start_date = as.Date(NA),
    rainfall_end_date = as.Date(NA),
    has_discharge_measurements = TRUE,
    has_rainfall_data = FALSE,
    is_operating = FALSE
  )
  plan <- hydroDataBR:::ana_station_product_plan("56460000", station_inventory = inv)
  rain <- plan[plan$product == "daily_rainfall", , drop = FALSE]
  expect_false(isTRUE(rain$attempt[[1L]]))
  expect_match(rain$skip_reason[[1L]], "rainfall product not available")
})

test_that("unknown availability no longer triggers blind full-period fallback", {
  inv <- tibble::tibble(station_code = "99999999", station_type = "Fluviometrica")
  plan <- hydroDataBR:::ana_station_product_plan("99999999", station_inventory = inv)
  expect_false(any(plan$attempt))
  expect_false(any(plan$date_source == "fallback_full_period", na.rm = TRUE))
})
