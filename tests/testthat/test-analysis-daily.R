test_that("analyze_daily_series validates the daily contract", {
  bad <- data.frame(
    station_code = "001",
    date = as.Date("2020-01-01"),
    value = 10
  )

  expect_error(
    analyze_daily_series(bad),
    "missing required columns"
  )
})

test_that("analyze_daily_series summarizes availability", {
  daily <- data.frame(
    station_code = c("001", "001", "001", "001"),
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-04", "2020-01-04")),
    variable = "discharge",
    value = c(10, NA, 12, 13)
  )

  out <- analyze_daily_series(daily, analysis = "availability")

  expect_equal(nrow(out), 1L)
  expect_equal(out$station_code, "001")
  expect_equal(out$variable, "discharge")
  expect_equal(out$start_date, as.Date("2020-01-01"))
  expect_equal(out$end_date, as.Date("2020-01-04"))
  expect_equal(out$n_records, 4L)
  expect_equal(out$expected_days, 4L)
  expect_equal(out$observed_days, 3L)
  expect_equal(out$valid_days, 2L)
  expect_equal(out$missing_dates, 1L)
  expect_equal(out$missing_value_days, 1L)
  expect_equal(out$missing_days, 2L)
  expect_equal(out$duplicate_records, 1L)
  expect_equal(out$availability_pct, 50)
})

test_that("analyze_daily_series summarizes missingness runs", {
  daily <- data.frame(
    station_code = "001",
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-04")),
    variable = "discharge",
    value = c(10, NA, 12)
  )

  out <- analyze_daily_series(daily, analysis = "missingness")

  expect_equal(out$n_missing_value_records, 1L)
  expect_equal(out$n_missing_dates, 1L)
  expect_equal(out$missing_value_days, 1L)
  expect_equal(out$total_missing_days, 2L)
  expect_equal(out$longest_missing_run_days, 2L)
  expect_equal(out$pct_missing_days, 50)
})

test_that("analyze_daily_series computes descriptive statistics", {
  daily <- data.frame(
    station_code = c("001", "001", "001", "002"),
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-01")),
    variable = c("discharge", "discharge", "discharge", "rainfall"),
    value = c(10, 20, NA, 5)
  )

  out <- analyze_daily_series(daily, analysis = "summary")
  out_001 <- out[out$station_code == "001", , drop = FALSE]
  out_002 <- out[out$station_code == "002", , drop = FALSE]

  expect_equal(nrow(out), 2L)
  expect_equal(out_001$n_records, 3L)
  expect_equal(out_001$observed_days, 3L)
  expect_equal(out_001$valid_days, 2L)
  expect_equal(out_001$min, 10)
  expect_equal(out_001$mean, 15)
  expect_equal(out_001$median, 15)
  expect_equal(out_001$max, 20)
  expect_true(!is.na(out_001$sd))
  expect_equal(out_002$sd, NA_real_)
})

test_that("analyze_daily_series computes monthly summaries", {
  daily <- data.frame(
    station_code = "001",
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-04", "2020-02-01")),
    variable = "discharge",
    value = c(10, NA, 12, 20)
  )

  out <- analyze_daily_series(daily, analysis = "monthly")
  jan <- out[out$year == 2020 & out$month == 1, , drop = FALSE]
  feb <- out[out$year == 2020 & out$month == 2, , drop = FALSE]

  expect_equal(nrow(out), 2L)
  expect_equal(jan$period_start, as.Date("2020-01-01"))
  expect_equal(jan$period_end, as.Date("2020-01-31"))
  expect_equal(jan$expected_days, 31L)
  expect_equal(jan$observed_days, 3L)
  expect_equal(jan$valid_days, 2L)
  expect_equal(jan$missing_days, 29L)
  expect_equal(jan$mean, 11)
  expect_equal(jan$sum, 22)
  expect_equal(feb$expected_days, 1L)
  expect_equal(feb$valid_days, 1L)
})

test_that("analyze_daily_series computes hydrological annual summaries", {
  daily <- data.frame(
    station_code = "001",
    date = as.Date(c("2019-10-01", "2020-09-30", "2020-10-01")),
    variable = "rainfall",
    value = c(1, 2, 3)
  )

  out <- analyze_daily_series(
    daily,
    analysis = "annual",
    year_type = "hydrological",
    start_month = 10
  )

  year_2020 <- out[out$year == 2020, , drop = FALSE]
  year_2021 <- out[out$year == 2021, , drop = FALSE]

  expect_equal(nrow(out), 2L)
  expect_equal(year_2020$period_start, as.Date("2019-10-01"))
  expect_equal(year_2020$period_end, as.Date("2020-09-30"))
  expect_equal(year_2020$expected_days, 366L)
  expect_equal(year_2020$valid_days, 2L)
  expect_equal(year_2020$sum, 3)
  expect_equal(year_2021$period_start, as.Date("2020-10-01"))
  expect_equal(year_2021$period_end, as.Date("2020-10-01"))
  expect_equal(year_2021$expected_days, 1L)
  expect_equal(year_2021$sum, 3)
})

test_that("filter_ana_stations filters explicit station metadata", {
  stations <- data.frame(
    station_code = c("001", "002", "003"),
    station_name = c("Rio Azul", "Rio Verde", "Ribeirao Preto"),
    station_type = c("flu", "flu", "plu"),
    state_code = c("MG", "SP", "MG"),
    municipality = c("Belo Horizonte", "Campinas", "Ouro Preto"),
    basin_code = c("1", "2", "1"),
    is_operating = c(TRUE, FALSE, TRUE),
    discharge_start_date = as.Date(c("2000-01-01", NA, NA)),
    discharge_end_date = as.Date(c("2020-01-01", NA, NA)),
    has_stage_data = c(TRUE, FALSE, FALSE),
    has_rainfall_data = c(FALSE, FALSE, TRUE),
    has_telemetry = c(FALSE, TRUE, FALSE),
    has_discharge_measurements = c(TRUE, FALSE, FALSE)
  )

  out <- filter_ana_stations(
    station_data = stations,
    state_code = "MG",
    product = "rainfall",
    is_operating = TRUE
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$station_code, "003")

  out_name <- filter_ana_stations(
    station_data = stations,
    name_pattern = "azul",
    product = c("discharge", "stage")
  )

  expect_equal(nrow(out_name), 1L)
  expect_equal(out_name$station_code, "001")
})
