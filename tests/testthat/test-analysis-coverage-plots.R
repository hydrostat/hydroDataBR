test_that("Stage 08E extremes include flags and plots", {
  dates <- seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day")
  v <- 20 + sin(seq_along(dates) / 20) * 5
  v[dates == as.Date("2020-02-10")] <- 120
  daily <- data.frame(
    station_code = "001",
    date = dates,
    variable = "discharge",
    value = v,
    unit = "m3/s",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    flag_manual = dates == as.Date("2020-02-10"),
    stringsAsFactors = FALSE
  )

  maxima <- analyze_hydro_data(daily, analysis = "annual_maxima")
  expect_true(all(c("n_flags", "flag_class", "flags_summary") %in% names(maxima)))
  expect_true(any(maxima$n_flags > 0))
  expect_s3_class(plot_hydro_data(maxima, plot = "annual_maxima"), "ggplot")

  lows <- analyze_hydro_data(daily, analysis = "low_flows", durations = c(1, 7))
  expect_true(all(c("duration_days", "n_flags", "flag_class") %in% names(lows)))
  expect_s3_class(plot_hydro_data(lows, plot = "low_flows"), "ggplot")
})

test_that("Stage 08E rainfall diagnostics and maxima work", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
  rain <- rep(0, length(dates))
  rain[as.integer(format(dates, "%j")) %% 9 == 0] <- 12
  rain[dates == as.Date("2021-03-15")] <- 450
  rain[dates == as.Date("2021-05-10")] <- -1
  daily <- data.frame(
    station_code = "001",
    date = dates,
    variable = "rainfall",
    value = rain,
    unit = "mm",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  rain_max <- analyze_hydro_data(daily, analysis = "rainfall_annual_maxima")
  expect_true(all(c("max_value", "n_flags", "flag_very_high_rainfall") %in% names(rain_max)) || "n_flags" %in% names(rain_max))
  expect_s3_class(plot_hydro_data(rain_max, plot = "rainfall_annual_maxima"), "ggplot")

  diag <- analyze_hydro_data(daily, analysis = "rainfall_diagnostics")
  expect_true(nrow(diag) >= 2)
  expect_true(all(c("flag_negative_rainfall", "flag_very_high_rainfall", "n_flags") %in% names(diag)))
  expect_s3_class(plot_hydro_data(diag, plot = "rainfall_diagnostics"), "ggplot")
  expect_s3_class(plot_hydro_data(daily, plot = "rainfall_monthly_boxplot"), "ggplot")
})

test_that("Stage 08E measurement and rating diagnostics work", {
  measurements <- data.frame(
    station_code = "001",
    measurement_date = as.Date("2020-01-01") + 0:5,
    stage_cm = c(100, 110, 110, 130, 140, 150),
    discharge_m3s = c(10, 12, 25, 18, -1, 31),
    stringsAsFactors = FALSE
  )
  curves <- data.frame(
    station_code = "001",
    rating_curve_id = "A",
    rating_curve_segment_id = "1",
    coefficient_a = 0.01,
    coefficient_h0_cm = 0,
    coefficient_b = 1.5,
    stage_min_cm = 50,
    stage_max_cm = 200,
    valid_from = as.Date("2019-01-01"),
    valid_to = as.Date("2022-12-31"),
    stringsAsFactors = FALSE
  )
  x <- list(discharge_measurements = measurements, rating_curves = curves)

  md <- analyze_hydro_data(measurements, analysis = "measurement_diagnostics")
  expect_true(all(c("flag_repeated_stage_variable_discharge", "n_flags") %in% names(md)))
  expect_s3_class(plot_hydro_data(md, plot = "measurement_diagnostics"), "ggplot")

  rd <- analyze_hydro_data(x, analysis = "rating_diagnostics")
  expect_true(all(c("predicted_discharge_m3s", "relative_error_pct", "n_flags") %in% names(rd)))
  expect_s3_class(plot_hydro_data(rd, plot = "rating_diagnostics"), "ggplot")
  expect_s3_class(plot_rating_curves(curves, measurements = measurements), "ggplot")
  expect_s3_class(plot_hydro_data(x, plot = "rating_curves"), "ggplot")
})

test_that("Stage 08E additional analysis tables are available", {
  dates <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "day")
  daily <- data.frame(
    station_code = "001",
    date = rep(dates, 2),
    variable = rep(c("discharge", "stage"), each = length(dates)),
    value = c(seq_along(dates), rep(c(NA, 1, 2, 3), length.out = length(dates))),
    unit = rep(c("m3/s", "cm"), each = length(dates)),
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )
  monthly <- table_hydro_data(daily, table = "monthly_flow_indices")
  expect_true(all(c("q90_month", "q95_month") %in% names(monthly)))
  gaps <- table_hydro_data(daily, table = "daily_gap_summary", period = "monthly")
  expect_true(all(c("missing_days", "missing_pct") %in% names(gaps)))
})
