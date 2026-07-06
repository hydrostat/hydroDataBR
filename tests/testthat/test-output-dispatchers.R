test_that("Stage 08B table dispatcher handles daily and batch objects", {
  dates <- as.Date("2020-01-01") + 0:9
  daily <- rbind(
    data.frame(
      station_code = "001", date = dates, variable = "discharge",
      value = 1:10, unit = "m3/s", consistency_level = NA_integer_,
      source_status = NA_character_, source = "fixture", stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = "002", date = dates, variable = "discharge",
      value = 11:20, unit = "m3/s", consistency_level = NA_integer_,
      source_status = NA_character_, source = "fixture", stringsAsFactors = FALSE
    )
  )

  batch <- list(
    results = list(
      `001` = list(daily_data = daily[daily$station_code == "001", , drop = FALSE]),
      `002` = list(daily_data = daily[daily$station_code == "002", , drop = FALSE])
    ),
    request_report = data.frame(
      station_code = c("001", "002"), status = c("success", "success"),
      success = c(TRUE, TRUE), n_records = c(10L, 10L), n_rows = c(10L, 10L),
      error_message = c(NA_character_, NA_character_), stringsAsFactors = FALSE
    )
  )

  stats <- table_hydro_data(batch, table = "daily_statistics", variable = "discharge")
  expect_s3_class(stats, "data.frame")
  expect_equal(sort(unique(stats$station_code)), c("001", "002"))

  report <- table_hydro_data(batch, table = "request_report")
  expect_s3_class(report, "data.frame")
  expect_equal(nrow(report), 2)
})

test_that("Stage 08B plot dispatcher handles daily plots and multi-station modes", {
  dates <- as.Date("2020-01-01") + 0:39
  daily <- rbind(
    data.frame(
      station_code = "001", date = dates, variable = "discharge",
      value = seq_along(dates), unit = "m3/s", consistency_level = NA_integer_,
      source_status = NA_character_, source = "fixture", stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = "002", date = dates, variable = "discharge",
      value = seq_along(dates) + 10, unit = "m3/s", consistency_level = NA_integer_,
      source_status = NA_character_, source = "fixture", stringsAsFactors = FALSE
    )
  )

  expect_s3_class(
    plot_hydro_data(daily, plot = "daily_series", variable = "discharge"),
    "ggplot"
  )
  expect_s3_class(
    plot_hydro_data(daily, plot = "availability", variable = "discharge"),
    "ggplot"
  )

  plot_list <- plot_hydro_data(
    daily,
    plot = "annual_summary",
    variable = "discharge",
    multi_station = "list"
  )
  expect_type(plot_list, "list")
  expect_equal(names(plot_list), c("001", "002"))
  expect_s3_class(plot_list[[1]], "ggplot")
})

test_that("Stage 08B plot dispatcher handles rating curves and measurements", {
  curves <- data.frame(
    station_code = c("001", "002"),
    rating_curve_id = c("A", "B"),
    rating_curve_segment_id = c("1", "1"),
    valid_from = as.Date(c("2020-01-01", "2020-01-01")),
    valid_to = as.Date(c("2021-12-31", "2021-12-31")),
    coefficient_a = c(0.02, 0.03),
    coefficient_h0_cm = c(20, 30),
    coefficient_b = c(1.5, 1.4),
    stage_min_cm = c(50, 60),
    stage_max_cm = c(180, 190),
    stringsAsFactors = FALSE
  )
  measurements <- data.frame(
    station_code = rep(c("001", "002"), each = 3),
    measurement_date = rep(as.Date("2020-01-01") + c(0, 30, 60), 2),
    stage_cm = c(70, 100, 130, 80, 110, 140),
    discharge_m3s = c(5, 9, 14, 6, 11, 16),
    stringsAsFactors = FALSE
  )

  aggregate <- list(
    rating_curves = curves,
    discharge_measurements = measurements
  )

  expect_error(plot_hydro_data(aggregate, plot = "rating_curves"), "multiple stations")
  expect_s3_class(
    plot_hydro_data(aggregate, plot = "rating_curves", station_code = "001"),
    "ggplot"
  )
  expect_s3_class(
    plot_hydro_data(aggregate, plot = "rating_validity", station_code = "001"),
    "ggplot"
  )

  measurement_plots <- plot_hydro_data(aggregate, plot = "measurements", multi_station = "list")
  expect_equal(names(measurement_plots), c("001", "002"))
  expect_s3_class(measurement_plots[["001"]], "ggplot")
})
