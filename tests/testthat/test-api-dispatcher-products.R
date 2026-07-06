testthat::test_that("get_ana_data dispatches all implemented products", {
  testthat::local_mocked_bindings(
    get_ana_daily_discharge = function(...) tibble::tibble(product = "daily_discharge"),
    get_ana_daily_stage = function(...) tibble::tibble(product = "daily_stage"),
    get_ana_daily_rainfall = function(...) tibble::tibble(product = "daily_rainfall"),
    get_ana_stations = function(...) tibble::tibble(product = "stations"),
    get_ana_discharge_measurements = function(...) tibble::tibble(product = "discharge_measurements"),
    get_ana_rating_curves = function(...) tibble::tibble(product = "rating_curves"),
    get_ana_cross_sections = function(...) list(
      sections = tibble::tibble(product = "cross_sections"),
      vertices = tibble::tibble()
    )
  )

  testthat::expect_identical(get_ana_data("daily_discharge")$product, "daily_discharge")
  testthat::expect_identical(get_ana_data("daily_stage")$product, "daily_stage")
  testthat::expect_identical(get_ana_data("daily_rainfall")$product, "daily_rainfall")
  testthat::expect_identical(get_ana_data("stations")$product, "stations")
  testthat::expect_identical(get_ana_data("discharge_measurements")$product, "discharge_measurements")
  testthat::expect_identical(get_ana_data("rating_curves")$product, "rating_curves")
  testthat::expect_identical(get_ana_data("cross_sections")$sections$product, "cross_sections")
})

testthat::test_that("get_ana_data accepts simple product aliases", {
  testthat::local_mocked_bindings(
    get_ana_stations = function(...) tibble::tibble(product = "stations"),
    get_ana_rating_curves = function(...) tibble::tibble(product = "rating_curves")
  )

  testthat::expect_identical(get_ana_data("station_inventory")$product, "stations")
  testthat::expect_identical(get_ana_data("rating_curve")$product, "rating_curves")
})

testthat::test_that("get_ana_data rejects unsupported products", {
  testthat::expect_error(
    get_ana_data("unknown_product"),
    "Unsupported product"
  )
})

testthat::test_that("get_ana_data_batch supports station inventory", {
  testthat::local_mocked_bindings(
    get_ana_stations = function(token = NULL, station_code, ...) {
      tibble::tibble(
        station_code = station_code,
        station_name = paste0("Station ", station_code),
        state_code = "MG",
        state_name = "MINAS GERAIS",
        municipality_code = NA_character_,
        municipality = NA_character_,
        basin_code = NA_character_,
        basin_name = NA_character_,
        station_type = NA_character_,
        operating = TRUE,
        latitude = NA_real_,
        longitude = NA_real_,
        altitude_m = NA_real_,
        drainage_area_km2 = NA_real_,
        operator_code = NA_character_,
        operator_acronym = NA_character_,
        responsible_acronym = NA_character_,
        last_update = as.Date(NA),
        source = "ana_hidrowebservice_inventory"
      )
    }
  )

  out <- get_ana_data_batch(
    station_codes = c("01540000", "01550000"),
    product = "stations",
    token = structure(list(), class = "ana_token")
  )

  testthat::expect_s3_class(out$data, "tbl_df")
  testthat::expect_s3_class(out$request_report, "tbl_df")
  testthat::expect_equal(out$data$station_code, c("01540000", "01550000"))
  testthat::expect_true(all(out$request_report$success))
})

testthat::test_that("get_ana_data_batch supports old product-first positional calls", {
  testthat::local_mocked_bindings(
    get_ana_daily_discharge = function(token = NULL, station_code, ...) {
      tibble::tibble(
        station_code = station_code,
        date = as.Date("2020-01-01"),
        variable = "discharge",
        value = 1,
        unit = "m3/s",
        consistency_level = "2",
        source_status = "mock",
        source = "ana_hidrowebservice_daily_discharge"
      )
    }
  )

  out <- get_ana_data_batch(
    "daily_discharge",
    c("01540000", "01550000"),
    token = structure(list(), class = "ana_token")
  )

  testthat::expect_equal(nrow(out$data), 2)
  testthat::expect_equal(out$request_report$station_code, c("01540000", "01550000"))
  testthat::expect_true(all(out$request_report$success))
})

testthat::test_that("get_ana_data_batch routes specialized products", {
  testthat::local_mocked_bindings(
    get_ana_rating_curves = function(token = NULL, station_code, ...) {
      tibble::tibble(
        rating_curve_id = paste0(station_code, "_curve"),
        rating_curve_segment_id = paste0(station_code, "_segment_1"),
        station_code = station_code,
        valid_from = as.Date("2020-01-01"),
        valid_to = as.Date("2020-12-31"),
        consistency_level = "2",
        segment_number = 1L,
        n_segments_reported = 1L,
        curve_type = "mock",
        equation_type = "mock",
        stage_min_cm = 100,
        stage_max_cm = 200,
        table_stage_step_cm = 10,
        coefficient_a = 1,
        coefficient_h0_cm = 95,
        coefficient_n = 2,
        source = "ana_hidrowebservice_rating_curves"
      )
    }
  )

  out <- get_ana_data_batch(
    station_codes = c("01540000", "01550000"),
    product = "rating_curves",
    token = structure(list(), class = "ana_token")
  )

  testthat::expect_equal(nrow(out$data), 2)
  testthat::expect_equal(out$data$coefficient_h0_cm, c(95, 95))
})
