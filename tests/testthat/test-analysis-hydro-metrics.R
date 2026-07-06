test_that("Stage 08C rainfall indices are computed from daily data", {
  dates <- as.Date("2020-01-01") + 0:9
  rainfall <- data.frame(
    station_code = "001",
    date = dates,
    variable = "rainfall",
    value = c(0, 5, 11, 21, 51, NA, 0, 2, 0, 8),
    unit = "mm",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  out <- analyze_hydro_data(rainfall, analysis = "rainfall_indices")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$year, 2020L)
  expect_equal(out$total_rainfall, 98)
  expect_equal(out$wet_days, 6)
  expect_equal(out$rx1day, 51)
  expect_equal(out$rx5day, 88)
  expect_equal(out$r10, 3)
  expect_equal(out$r20, 2)
  expect_equal(out$r50, 1)
  expect_equal(out$cdd, 1)
  expect_equal(out$cwd, 4)
  expect_equal(round(out$sdii, 6), round(98 / 6, 6))
})

test_that("Stage 08C flow indices, interpolated duration curve and low flows are computed", {
  discharge <- data.frame(
    station_code = "001",
    date = as.Date("2020-01-01") + 0:9,
    variable = "discharge",
    value = 1:10,
    unit = "m3/s",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  indices <- analyze_hydro_data(discharge, analysis = "flow_indices")
  expect_s3_class(indices, "data.frame")
  expect_equal(indices$qmlt, 5.5)
  expect_true(is.finite(indices$q90))
  expect_true(is.finite(indices$q95))

  fdc <- analyze_hydro_data(discharge, analysis = "flow_duration")
  expect_s3_class(fdc, "data.frame")
  expect_equal(nrow(fdc), 99L)
  expect_equal(fdc$permanence_pct, 1:99)
  expect_equal(fdc$value[fdc$permanence_pct == 50], 5.5)
  expect_equal(fdc$value[fdc$permanence_pct == 1], 10)
  expect_equal(fdc$value[fdc$permanence_pct == 99], 1)

  low <- analyze_hydro_data(
    discharge,
    analysis = "low_flows",
    durations = c(1, 3),
    complete_years_only = FALSE
  )
  expect_s3_class(low, "data.frame")
  expect_equal(sort(unique(low$duration_days)), c(1L, 3L))
  expect_equal(low$min_value[low$duration_days == 1L], 1)
  expect_equal(low$min_value[low$duration_days == 3L], 2)
  expect_false(low$complete_year[[1]])
})

test_that("Stage 08C annual maxima and table dispatcher support analysis tables", {
  daily <- data.frame(
    station_code = rep(c("001", "002"), each = 5),
    date = rep(as.Date("2020-01-01") + 0:4, 2),
    variable = "discharge",
    value = c(1, 5, 3, 4, 2, 10, 8, 7, 6, 9),
    unit = "m3/s",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  out <- analyze_hydro_data(daily, analysis = "annual_maxima", complete_years_only = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_equal(out$max_value[out$station_code == "001"], 5)
  expect_equal(out$max_value[out$station_code == "002"], 10)
  expect_true("complete_year" %in% names(out))
  expect_false(any(out$complete_year))

  via_table <- table_hydro_data(daily, table = "flow_indices")
  expect_s3_class(via_table, "data.frame")
  expect_equal(nrow(via_table), 2L)
  expect_true("qmlt" %in% names(via_table))
})

test_that("Stage 08C complete-year filtering removes partial hydrological years", {
  dates <- seq(as.Date("1995-01-01"), as.Date("2024-12-31"), by = "day")
  discharge <- data.frame(
    station_code = "001",
    date = dates,
    variable = "discharge",
    value = seq_along(dates),
    unit = "m3/s",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  maxima <- analyze_hydro_data(
    discharge,
    analysis = "annual_maxima",
    year_start_month = 10
  )
  expect_equal(range(maxima$year), c(1996L, 2024L))
  expect_equal(nrow(maxima), 29L)
  expect_true(all(maxima$complete_year))

  maxima_all <- analyze_hydro_data(
    discharge,
    analysis = "annual_maxima",
    year_start_month = 10,
    complete_years_only = FALSE
  )
  expect_true(1995L %in% maxima_all$year)
  expect_true(2025L %in% maxima_all$year)
  expect_false(maxima_all$complete_year[maxima_all$year == 1995L])
  expect_false(maxima_all$complete_year[maxima_all$year == 2025L])

  low <- analyze_hydro_data(
    discharge,
    analysis = "low_flows",
    year_start_month = 10,
    durations = c(1, 3, 7)
  )
  expect_equal(nrow(low), 29L * 3L)
  expect_true(all(low$complete_year))
})

test_that("Stage 08C analyses accept product-all batch objects", {
  d1 <- data.frame(
    station_code = "001",
    date = as.Date("2020-01-01") + 0:2,
    variable = "rainfall",
    value = c(0, 10, 20),
    unit = "mm",
    consistency_level = NA_integer_,
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )
  d2 <- d1
  d2$station_code <- "002"
  d2$value <- c(5, 0, 50)

  batch <- list(
    results = list(
      "001" = list(daily_data = d1),
      "002" = list(daily_data = d2)
    ),
    request_report = data.frame()
  )

  out <- analyze_hydro_data(batch, analysis = "rainfall_indices")
  expect_s3_class(out, "data.frame")
  expect_equal(sort(out$station_code), c("001", "002"))
  expect_equal(out$rx1day[out$station_code == "002"], 50)
})
