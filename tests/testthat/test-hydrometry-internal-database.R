test_that("daily hydrometry diagnostics use the internal database when references are absent", {
  common_stations <- intersect(
    unique(as.character(ana_discharge_measurements$station_code)),
    unique(as.character(ana_rating_curves$station_code))
  )
  expect_gt(length(common_stations), 0)

  curves <- ana_rating_curves[
    as.character(ana_rating_curves$station_code) %in% common_stations &
      !is.na(ana_rating_curves$valid_from) &
      !is.na(ana_rating_curves$stage_min_cm) &
      !is.na(ana_rating_curves$stage_max_cm) &
      !is.na(ana_rating_curves$coefficient_a) &
      !is.na(ana_rating_curves$coefficient_n),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(curves), 0)

  curve <- curves[1, , drop = FALSE]
  station <- as.character(curve$station_code[1])
  date_value <- as.Date(curve$valid_from[1])
  stage_value <- mean(c(curve$stage_min_cm[1], curve$stage_max_cm[1]), na.rm = TRUE)

  daily_data <- data.frame(
    station_code = c(station, station),
    date = as.Date(c(date_value, date_value)),
    variable = c("discharge", "stage"),
    value = c(10, stage_value),
    unit = c("m3/s", "cm"),
    consistency_level = c(2L, 2L),
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  result <- diagnose_daily_hydrometry(daily_data)

  expect_true(is.list(result))
  expect_identical(result$hydrometry_reference_source, "internal_database")
  expect_identical(result$hydrometry_reference_snapshot, "2026-06")
  expect_true("daily_flags" %in% names(result))
  expect_true("measurement_year_summary" %in% names(result))
  expect_true("hydrometry_reference_details" %in% names(result))
  expect_true(any(result$hydrometry_reference_details$source == "internal_database"))
})

test_that("downloaded and user supplied references have priority over internal data", {
  curves <- ana_rating_curves[
    !is.na(ana_rating_curves$valid_from) &
      !is.na(ana_rating_curves$stage_min_cm) &
      !is.na(ana_rating_curves$stage_max_cm),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(curves), 0)

  curve <- curves[1, , drop = FALSE]
  station <- as.character(curve$station_code[1])
  date_value <- as.Date(curve$valid_from[1])
  stage_value <- mean(c(curve$stage_min_cm[1], curve$stage_max_cm[1]), na.rm = TRUE)

  daily_data <- data.frame(
    station_code = c(station, station),
    date = as.Date(c(date_value, date_value)),
    variable = c("discharge", "stage"),
    value = c(10, stage_value),
    unit = c("m3/s", "cm"),
    consistency_level = c(2L, 2L),
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  downloaded <- list(
    daily_data = daily_data,
    rating_curves = curve
  )
  downloaded_result <- diagnose_daily_hydrometry(downloaded)
  expect_identical(downloaded_result$hydrometry_reference_source, "downloaded_object")

  user_result <- diagnose_daily_hydrometry(
    daily_data,
    rating_curves = curve
  )
  expect_identical(user_result$hydrometry_reference_source, "user_supplied")
  expect_true(any(user_result$hydrometry_reference_details$source == "user_supplied"))
})

test_that("daily hydrometry diagnostics can skip the internal database", {
  daily_data <- data.frame(
    station_code = c("00000000", "00000000"),
    date = as.Date(c("2000-01-01", "2000-01-01")),
    variable = c("discharge", "stage"),
    value = c(10, 100),
    unit = c("m3/s", "cm"),
    consistency_level = c(2L, 2L),
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  result <- diagnose_daily_hydrometry(daily_data, use_internal_database = FALSE)

  expect_true(is.list(result))
  expect_identical(result$hydrometry_reference_source, "not_available")
  expect_true(is.na(result$hydrometry_reference_snapshot))
  expect_true("daily_flags" %in% names(result))
})

test_that("plot and table dispatchers can diagnose daily hydrometry automatically", {
  curves <- ana_rating_curves[
    !is.na(ana_rating_curves$valid_from) &
      !is.na(ana_rating_curves$stage_min_cm) &
      !is.na(ana_rating_curves$stage_max_cm),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(curves), 0)

  curve <- curves[1, , drop = FALSE]
  station <- as.character(curve$station_code[1])
  date_value <- as.Date(curve$valid_from[1])
  stage_value <- mean(c(curve$stage_min_cm[1], curve$stage_max_cm[1]), na.rm = TRUE)

  daily_data <- data.frame(
    station_code = c(station, station, station),
    date = as.Date(c(date_value, date_value, date_value + 1)),
    variable = c("discharge", "stage", "discharge"),
    value = c(10, stage_value, 11),
    unit = c("m3/s", "cm", "m3/s"),
    consistency_level = c(2L, 2L, 2L),
    source_status = NA_character_,
    source = "test",
    stringsAsFactors = FALSE
  )

  table_result <- table_hydro_data(daily_data, table = "hydrometry_diagnostics")
  expect_s3_class(table_result, "data.frame")

  plot_result <- plot_hydro_data(daily_data, plot = "hydrometry_diagnostics", type = "flags")
  expect_s3_class(plot_result, "ggplot")
})
