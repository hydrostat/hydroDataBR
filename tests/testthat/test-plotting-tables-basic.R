test_that("Stage 08 daily tables summarize standardized series", {
  daily <- data.frame(
    station_code = rep("10100000", 10),
    date = as.Date("2020-01-01") + 0:9,
    variable = rep("discharge", 10),
    value = c(1:5, NA, 7:10),
    unit = rep("m3/s", 10),
    consistency_level = 1L,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  availability <- table_daily_availability(daily, period = "monthly")
  expect_s3_class(availability, "data.frame")
  expect_equal(nrow(availability), 1L)
  expect_equal(availability$days_expected, 10)
  expect_equal(availability$days_with_value, 9)

  annual <- table_daily_statistics(daily, period = "annual")
  expect_s3_class(annual, "data.frame")
  expect_equal(nrow(annual), 1L)
  expect_equal(annual$days_with_value, 9)
  expect_equal(annual$total_value, 49)

  regime <- table_daily_statistics(daily, period = "monthly_regime")
  expect_s3_class(regime, "data.frame")
  expect_equal(regime$month, 1L)
})

test_that("Stage 08 plot functions return ggplot objects", {
  daily <- data.frame(
    station_code = rep("10100000", 40),
    date = as.Date("2020-01-01") + 0:39,
    variable = rep("discharge", 40),
    value = seq(10, 49),
    unit = rep("m3/s", 40),
    consistency_level = 1L,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  expect_s3_class(theme_hydrodatabr(), "theme")
  expect_s3_class(plot_daily_series(daily), "ggplot")
  expect_s3_class(plot_daily_availability(daily), "ggplot")
  expect_s3_class(plot_annual_summary(daily), "ggplot")
  expect_s3_class(plot_monthly_summary(daily), "ggplot")
  expect_s3_class(plot_flow_duration_curve(daily), "ggplot")
})

test_that("Stage 08 station and request tables normalize common contracts", {
  stations <- data.frame(
    station_code = c("10100000", "10200000"),
    station_name = c("A", "B"),
    discharge_start_date = as.Date(c("2000-01-01", NA)),
    discharge_end_date = as.Date(c("2020-12-31", NA)),
    stage_start_date = as.Date(c("2000-01-01", "2001-01-01")),
    stage_end_date = as.Date(c("2020-12-31", "2020-12-31")),
    rainfall_start_date = as.Date(c(NA, "2005-01-01")),
    rainfall_end_date = as.Date(c(NA, "2020-12-31")),
    telemetric_start_date = as.Date(c(NA, NA)),
    telemetric_end_date = as.Date(c(NA, NA)),
    has_discharge_measurements = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  products <- table_station_products(stations)
  expect_s3_class(products, "data.frame")
  expect_true(all(c("station_code", "product", "available") %in% names(products)))
  expect_true(any(products$product == "daily_discharge" & products$available))

  report <- list(request_report = data.frame(
    product = "daily_discharge",
    station_code = "10100000",
    status = "success",
    n_rows = 10,
    stringsAsFactors = FALSE
  ))
  normalized <- table_request_report(report)
  expect_s3_class(normalized, "data.frame")
  expect_true(normalized$success)
})

test_that("Stage 08 rating and diagnostic plots support simple inputs", {
  measurements <- data.frame(
    measurement_date = as.Date("2020-01-01") + 0:4,
    stage_cm = c(100, 120, 140, 160, 180),
    discharge_m3s = c(10, 15, 25, 40, 60),
    wetted_area_m2 = c(20, 25, 32, 40, 50),
    width_m = c(10, 10, 10, 10, 10),
    mean_depth_m = c(2, 2.5, 3.2, 4, 5),
    mean_velocity_ms = c(0.5, 0.6, 0.8, 1.0, 1.2),
    stringsAsFactors = FALSE
  )

  curves <- data.frame(
    rating_curve_id = "1",
    rating_curve_segment_id = "1",
    coefficient_a = 0.02,
    coefficient_h0_cm = 80,
    coefficient_b = 1.5,
    stage_min_cm = 90,
    stage_max_cm = 200,
    stringsAsFactors = FALSE
  )

  expect_s3_class(plot_discharge_measurements(measurements), "ggplot")
  expect_s3_class(plot_discharge_measurements(measurements, type = "year"), "ggplot")
  expect_s3_class(plot_rating_curves(curves, measurements), "ggplot")

  diagnostics <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    relative_error_pct = c(-5, 0, 10, 25, -30),
    outside_curve_flag = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    missing_stage_flag = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  diag_table <- table_daily_hydrometry_diagnostics(diagnostics)
  expect_s3_class(diag_table, "data.frame")
  expect_true(all(c("issue", "n_flagged") %in% names(diag_table)))
  expect_s3_class(plot_daily_hydrometry_diagnostics(diagnostics), "ggplot")
  expect_s3_class(plot_daily_hydrometry_diagnostics(diagnostics, type = "flags"), "ggplot")
  expect_s3_class(plot_daily_hydrometry_diagnostics(diagnostics, type = "flag_timeline"), "ggplot")
})
