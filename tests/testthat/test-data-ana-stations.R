test_that("built-in ana_stations dataset has expected contract", {
  env <- new.env(parent = emptyenv())
  data("ana_stations", package = "hydroDataBR", envir = env)

  expect_true(exists("ana_stations", envir = env, inherits = FALSE))

  stations <- env$ana_stations

  expected_columns <- c(
    "station_code",
    "station_name",
    "station_type",
    "state_code",
    "municipality",
    "basin_code",
    "basin_name",
    "latitude",
    "longitude",
    "altitude_m",
    "drainage_area_km2",
    "operator",
    "responsible_agency",
    "is_operating",
    "discharge_start_date",
    "discharge_end_date",
    "telemetric_start_date",
    "telemetric_end_date",
    "stage_start_date",
    "stage_end_date",
    "rainfall_start_date",
    "rainfall_end_date",
    "has_discharge_measurements",
    "has_telemetry",
    "has_stage_data",
    "has_rainfall_data",
    "last_update"
  )

  expect_s3_class(stations, "data.frame")
  expect_named(stations, expected_columns)
  expect_gt(nrow(stations), 30000)
  expect_false(anyNA(stations$station_code))
  expect_equal(anyDuplicated(stations$station_code), 0L)
  expect_type(stations$station_code, "character")
  expect_type(stations$latitude, "double")
  expect_type(stations$longitude, "double")
  expect_type(stations$is_operating, "logical")
  expect_s3_class(stations$last_update, "Date")

  data_file <- file.path("data", "ana_stations.rda")
  if (file.exists(data_file)) {
    expect_lt(file.info(data_file)$size, 2 * 1024^2)
  }
})
