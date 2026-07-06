test_that("diagnose_daily_hydrometry identifies cross-variable and rating-curve issues", {
  daily_data <- data.frame(
    station_code = "001",
    date = as.Date(c(
      "2020-01-01", "2020-01-01",
      "2020-01-02", "2020-01-02",
      "2020-01-03",
      "2020-01-04",
      "2020-01-05", "2020-01-05",
      "2020-01-06", "2020-01-06"
    )),
    variable = c(
      "discharge", "stage",
      "discharge", "stage",
      "discharge",
      "stage",
      "discharge", "stage",
      "discharge", "stage"
    ),
    value = c(10, 100, 25, 200, 0, 150, 60, 600, 10, 100),
    stringsAsFactors = FALSE
  )

  rating_curves <- data.frame(
    station_code = "001",
    rating_curve_id = "rc1",
    rating_curve_segment_id = "seg1",
    segment_number = 1,
    valid_from = as.Date("2020-01-01"),
    valid_to = as.Date("2020-01-05"),
    stage_min_cm = 50,
    stage_max_cm = 300,
    coefficient_a = 10,
    coefficient_h0_cm = 0,
    coefficient_n = 1,
    stringsAsFactors = FALSE
  )

  out <- diagnose_daily_hydrometry(
    daily_data = daily_data,
    rating_curves = rating_curves,
    relative_error_threshold_pct = 20
  )

  expected_names <- c(
    "summary", "indices", "daily_flags", "rating_matches", "rating_curve_coverage"
  )
  expect_true(all(expected_names %in% names(out)))
  expect_s3_class(out$summary, "data.frame")
  expect_s3_class(out$daily_flags, "data.frame")
  expect_s3_class(out$rating_matches, "data.frame")

  s <- out$summary
  expect_equal(nrow(s), 1)
  expect_equal(s$station_code, "001")
  expect_equal(s$expected_days, 6)
  expect_equal(s$discharge_valid_days, 5)
  expect_equal(s$stage_valid_days, 5)
  expect_equal(s$discharge_without_stage_days, 1)
  expect_equal(s$stage_without_discharge_days, 1)
  expect_equal(s$non_positive_discharge_days, 1)
  expect_equal(s$days_without_rating_curve_date_coverage, 1)
  expect_equal(s$days_without_applicable_rating_segment, 1)
  expect_equal(s$days_with_stage_outside_curve_range, 1)
  expect_equal(s$days_with_generated_discharge, 3)
  expect_equal(s$days_exceeding_relative_error_threshold, 1)

  flags <- out$daily_flags
  jan2 <- flags[flags$date == as.Date("2020-01-02"), ]
  expect_true(jan2$relative_error_exceeds_threshold)
  expect_equal(jan2$generated_discharge_m3s, 20)
  expect_equal(jan2$relative_error_pct, 25)

  jan3 <- flags[flags$date == as.Date("2020-01-03"), ]
  expect_true(jan3$discharge_without_stage)
  expect_true(jan3$non_positive_discharge)

  jan4 <- flags[flags$date == as.Date("2020-01-04"), ]
  expect_true(jan4$stage_without_discharge)

  jan5 <- flags[flags$date == as.Date("2020-01-05"), ]
  expect_true(jan5$stage_outside_curve_range)
  expect_true(jan5$no_applicable_rating_segment)

  jan6 <- flags[flags$date == as.Date("2020-01-06"), ]
  expect_true(jan6$no_rating_curve_for_date)
  expect_true(jan6$after_last_rating_curve)
})

test_that("diagnose_daily_hydrometry handles multiple stations and empty curves", {
  daily_data <- data.frame(
    station_code = c("001", "001", "002"),
    date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
    variable = c("discharge", "stage", "stage"),
    value = c(10, 100, 50),
    stringsAsFactors = FALSE
  )

  out <- diagnose_daily_hydrometry(daily_data, rating_curves = data.frame())

  expect_equal(sort(out$summary$station_code), c("001", "002"))
  expect_equal(sum(out$summary$days_without_rating_curve_date_coverage), 2)
  expect_equal(nrow(out$rating_matches), 0)
})

test_that("diagnose_daily_hydrometry validates daily input", {
  bad <- data.frame(
    station_code = "001",
    date = as.Date("2020-01-01"),
    value = 10
  )

  expect_error(
    diagnose_daily_hydrometry(bad, data.frame()),
    "missing required columns"
  )
})
