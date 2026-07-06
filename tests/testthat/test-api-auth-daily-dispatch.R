test_that("ana_authenticate returns an in-memory token and print hides it", {
  testthat::local_mocked_bindings(
    ana_perform_get = function(url, headers = list(), timeout = 60) {
      expect_true("Identificador" %in% names(headers))
      expect_true("Senha" %in% names(headers))
      expect_match(url, "/OAUth/v1$", fixed = FALSE)
      list(
        status = 200L,
        body = '{"items":{"tokenautenticacao":"SECRET-TOKEN"}}',
        content_type = "application/json"
      )
    },
    .package = "hydroDataBR"
  )

  token <- ana_authenticate(identifier = "identifier", password = "password")

  expect_s3_class(token, "ana_token")
  expect_equal(hydroDataBR:::ana_token_value(token), "SECRET-TOKEN")
  expect_false(is_ana_token_expired(token))

  printed <- capture.output(print(token))
  expect_true(any(grepl("<hidden>", printed, fixed = TRUE)))
  expect_false(any(grepl("SECRET-TOKEN", printed, fixed = TRUE)))
})

test_that("ana_request_json uses bearer token without returning headers", {
  token <- list(
    token = "SECRET-TOKEN",
    created_at = Sys.time(),
    expires_at = Sys.time() + 3600,
    base_url = "https://example.test/base"
  )
  class(token) <- c("ana_token", "list")

  testthat::local_mocked_bindings(
    ana_perform_get = function(url, headers = list(), timeout = 60) {
      expect_match(url, "https://example.test/base/example/v1")
      expect_equal(headers$Authorization, "Bearer SECRET-TOKEN")
      list(
        status = 200L,
        body = '{"status":"OK","items":[{"x":1}]}',
        content_type = "application/json"
      )
    },
    .package = "hydroDataBR"
  )

  parsed <- ana_request_json(
    endpoint = "/example/v1",
    query = list(a = 1),
    token = token
  )

  expect_equal(parsed$status, "OK")
  expect_false("Authorization" %in% names(parsed))
})

test_that("daily discharge acquisition returns the finalized daily contract", {
  fake_daily <- tibble::tibble(
    station_code = c("01234567", "01234567"),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    variable = c("discharge", "stage"),
    value = c(10.5, 120),
    unit = c("m3/s", "cm"),
    consistency_level = c("2", "2"),
    source_status = c(NA_character_, NA_character_),
    source = c("ANA WebService XML", "ANA WebService XML")
  )

  testthat::local_mocked_bindings(
    ana_perform_get = function(url, headers = list(), timeout = 60) {
      expect_match(url, "HidroSerieHistorica", fixed = TRUE)
      expect_match(url, "tipoDados=3", fixed = TRUE)
      expect_match(url, "nivelConsistencia=2", fixed = TRUE)
      list(
        status = 200L,
        body = "<?xml version='1.0'?><DataTable></DataTable>",
        content_type = "text/xml"
      )
    },
    read_ana_xml = function(path) {
      expect_true(file.exists(path))
      fake_daily
    },
    .package = "hydroDataBR"
  )

  out <- get_ana_daily_discharge(
    station_code = "01234567",
    date_start = "2020-01-01",
    date_end = "2020-01-31"
  )

  expect_named(out, hydroDataBR:::ana_daily_contract_columns())
  expect_equal(unique(out$variable), "discharge")
  expect_equal(out$unit, "m3/s")
})

test_that("daily wrappers use the expected ANA data type codes", {
  requested_urls <- character()

  testthat::local_mocked_bindings(
    ana_perform_get = function(url, headers = list(), timeout = 60) {
      requested_urls <<- c(requested_urls, url)
      list(
        status = 200L,
        body = "<?xml version='1.0'?><DataTable></DataTable>",
        content_type = "text/xml"
      )
    },
    read_ana_xml = function(path) {
      tibble::tibble(
        station_code = "12345678",
        date = as.Date("2020-01-01"),
        variable = c("stage", "rainfall"),
        value = c(100, 2),
        unit = c("cm", "mm"),
        consistency_level = "2",
        source_status = NA_character_,
        source = "ANA WebService XML"
      )
    },
    .package = "hydroDataBR"
  )

  invisible(get_ana_daily_stage("12345678"))
  invisible(get_ana_daily_rainfall("12345678"))

  expect_true(any(grepl("tipoDados=1", requested_urls, fixed = TRUE)))
  expect_true(any(grepl("tipoDados=2", requested_urls, fixed = TRUE)))
})

test_that("get_ana_data dispatches to specific daily functions", {
  fake_daily <- tibble::tibble(
    station_code = "12345678",
    date = as.Date("2020-01-01"),
    variable = "rainfall",
    value = 1,
    unit = "mm",
    consistency_level = "2",
    source_status = NA_character_,
    source = "mock"
  )

  testthat::local_mocked_bindings(
    get_ana_daily_rainfall = function(...) fake_daily,
    .package = "hydroDataBR"
  )

  out <- get_ana_data("daily_rainfall", station_code = "12345678")
  expect_equal(out$variable, "rainfall")
})

test_that("batch acquisition continues after station-level errors", {
  testthat::local_mocked_bindings(
    get_ana_daily_discharge = function(station_code, ...) {
      station_code <- as.character(station_code)

      if (identical(station_code, "87654321")) {
        stop("mock station-level error", call. = FALSE)
      }

      tibble::tibble(
        station_code = station_code,
        date = as.Date("2020-01-01"),
        variable = "discharge",
        value = 1,
        unit = "m3/s",
        consistency_level = NA_integer_,
        source_status = "mock",
        source = "mock"
      )
    },
    .package = "hydroDataBR"
  )

  out <- get_ana_data_batch(
    product = "daily_discharge",
    station_code = c("12345678", "87654321")
  )

  expect_named(out, c("data", "request_report"))
  expect_equal(nrow(out$request_report), 2L)
  expect_equal(out$request_report$status, c("success", "error"))
  expect_equal(out$request_report$success, c(TRUE, FALSE))
  expect_equal(nrow(out$data), 1L)
  expect_equal(out$data$station_code, "12345678")
})

