test_that("diagnose_station_hydrometry returns the expected tables", {
  measurements <- data.frame(
    station_code = "001",
    measurement_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    stage_cm = c(100, 120, 140),
    discharge_m3s = c(10, 12, 14)
  )

  curves <- data.frame(
    station_code = "001",
    rating_curve_id = "rc1",
    rating_curve_segment_id = "seg1",
    segment_number = 1,
    valid_from = as.Date("2019-01-01"),
    valid_to = as.Date("2021-12-31"),
    stage_min_cm = 50,
    stage_max_cm = 200,
    coefficient_a = 10,
    coefficient_h0_cm = 0,
    coefficient_n = 1
  )

  out <- diagnose_station_hydrometry(measurements, curves, detailed = TRUE)

  expect_type(out, "list")
  expected_names <- c(
    "summary", "indices", "measurement_flags", "repeated_group_details",
    "curve_metadata", "curve_segments", "rating_curve_points", "rating_matches",
    "best_rating_match", "residual_envelopes", "residual_points",
    "temporal_regime", "power_curve_points"
  )
  expect_true(all(expected_names %in% names(out)))
  expect_s3_class(out$summary, "data.frame")
  expect_equal(out$summary$station_code, "001")
  expect_equal(out$summary$n_measurements, 3)
  expect_equal(out$summary$n_valid_measurements, 3)
  expect_equal(out$summary$n_rating_curves, 1)
  expect_equal(out$summary$n_rating_curve_segments, 1)
  expect_equal(out$summary$rating_match_fraction, 1)
  expect_equal(nrow(out$indices), 8)
})

test_that("diagnose_station_hydrometry flags zero and negative measurements", {
  measurements <- data.frame(
    station_code = "001",
    measurement_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    stage_cm = c(0, 100, -1),
    discharge_m3s = c(10, 0, -5)
  )

  out <- diagnose_station_hydrometry(measurements, data.frame(), detailed = FALSE)

  expect_equal(out$summary$n_stage_zero_or_negative, 2)
  expect_equal(out$summary$n_discharge_zero_or_negative, 2)
  expect_equal(out$summary$n_valid_measurements, 0)
  expect_equal(out$summary$diagnostic_detail_level, "light_station_summary")
})

test_that("diagnose_station_hydrometry detects repeated value groups", {
  measurements <- data.frame(
    station_code = "001",
    measurement_date = as.Date("2020-01-01") + 0:5,
    stage_cm = rep(100, 6),
    discharge_m3s = c(10, 11, 12, 13, 14, 15)
  )

  out <- diagnose_station_hydrometry(measurements, data.frame(), detailed = FALSE)

  expect_equal(out$summary$n_repeated_stage_variable_discharge_points, 6)
  expect_gt(nrow(out$repeated_group_details), 0)
  expect_true("same_stage_variable_discharge" %in% out$repeated_group_details$group_type)
})

test_that("diagnose_station_hydrometry accepts h0 in centimeters", {
  measurements <- data.frame(
    station_code = "001",
    measurement_date = as.Date("2020-01-01"),
    stage_cm = 100,
    discharge_m3s = 9
  )

  curves <- data.frame(
    station_code = "001",
    rating_curve_id = "rc1",
    rating_curve_segment_id = "seg1",
    segment_number = 1,
    valid_from = as.Date("2019-01-01"),
    valid_to = as.Date("2021-12-31"),
    stage_min_cm = 50,
    stage_max_cm = 200,
    coefficient_a = 10,
    coefficient_h0_cm = 10,
    coefficient_n = 1
  )

  out <- diagnose_station_hydrometry(measurements, curves, detailed = TRUE)

  expect_equal(out$best_rating_match$rating_predicted_discharge_m3s, 9)
  expect_equal(out$best_rating_match$rating_log_residual, 0)
})

test_that("diagnose_station_hydrometry can run temporal-regime screening with permissive test parameters", {
  dates <- seq(as.Date("2000-01-01"), by = "1 month", length.out = 72)
  stage_cm <- seq(100, 200, length.out = 72)
  discharge <- 5 * ((stage_cm / 100) ^ 1.4)
  discharge[37:72] <- discharge[37:72] * 1.8

  measurements <- data.frame(
    station_code = "001",
    measurement_date = dates,
    stage_cm = stage_cm,
    discharge_m3s = discharge
  )

  out <- diagnose_station_hydrometry(
    measurements,
    data.frame(),
    detailed = TRUE,
    params = list(
      min_power_model_points = 20,
      min_regime_measurements = 10,
      min_regime_span_years = 1,
      max_break_candidates = 12,
      min_break_gain = 0.01,
      min_log_residual_shift = log(1.05),
      residual_shift_mad_fraction = 0.1
    )
  )

  expect_s3_class(out$temporal_regime$model_scores, "data.frame")
  expect_gt(nrow(out$temporal_regime$model_scores), 0)
  expect_true("baseline_equation" %in% names(out$temporal_regime$model_scores))
})
