
test_that("HydroStat daily standardization expands discharge, stage and rainfall", {
  raw <- data.frame(
    EstacaoCodigo = "10100000",
    Data = as.Date("2020-02-01"),
    NivelConsistencia = c("1"),
    MediaDiaria = "1",
    Vazao01 = "10,5",
    Vazao29 = "20,5",
    Cota01 = "100",
    Cota29 = "110",
    Chuva01 = "5,5",
    Chuva29 = "0",
    stringsAsFactors = FALSE
  )
  out <- hydroDataBR:::hydrostat_standardize_ana_daily_table(raw, "fixture")
  expect_true(all(c("discharge", "stage", "rainfall") %in% out$variable))
  expect_equal(nrow(out), 6L)
  expect_equal(out$value[out$variable == "discharge" & format(out$date, "%d") == "01"], 10.5)
  expect_equal(out$value[out$variable == "stage" & format(out$date, "%d") == "29"], 110)
  expect_equal(out$value[out$variable == "rainfall" & format(out$date, "%d") == "01"], 5.5)
})

test_that("HydroStat consistency preference keeps consisted daily values", {
  x <- data.frame(
    station_code = c("10100000", "10100000", "10100000"),
    date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02")),
    variable = "discharge",
    value = c(1, 2, 3),
    unit = "m3/s",
    consistency_level = c("1", "2", "1"),
    stringsAsFactors = FALSE
  )
  out <- hydroDataBR:::hydrostat_prefer_daily_rows(x)
  expect_equal(nrow(out), 2L)
  expect_equal(out$value[out$date == as.Date("2020-01-01")], 2)
})

test_that("write_hydro_data exports only final compact discharge tables", {
  x <- data.frame(
    station_code = "10100000",
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day"),
    variable = "discharge",
    value = seq_len(91),
    unit = "m3/s",
    consistency_level = "2",
    stringsAsFactors = FALSE
  )
  dir <- tempfile("hydro_export_")
  manifest <- write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  expect_true(all(c(
    "discharge_monthly_mean_wide",
    "discharge_monthly_mean_long",
    "discharge_missing_counts",
    "discharge_daily_wide",
    "discharge_daily_long",
    "discharge_annual_maxima",
    "discharge_annual_low_flows",
    "discharge_flow_duration",
    "discharge_diagnostics"
  ) %in% manifest$component))
  expect_false(any(grepl("daily_statistics|daily_availability|flow_indices|monthly_flow_indices", manifest$component)))
})

test_that("write_hydro_data uses rainfall totals not rainfall means", {
  x <- data.frame(
    station_code = "10100000",
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"),
    variable = "rainfall",
    value = rep(1, 31),
    unit = "mm",
    consistency_level = "2",
    stringsAsFactors = FALSE
  )
  dir <- tempfile("hydro_rain_")
  write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  wide <- utils::read.csv(file.path(dir, "rainfall_monthly_total_wide.csv"), check.names = FALSE)
  long <- utils::read.csv(file.path(dir, "rainfall_monthly_total_long.csv"), check.names = FALSE)
  expect_equal(wide$Jan[1], 31)
  expect_equal(wide$`Total Anual`[1], 31)
  expect_equal(long$`Precipitacao total`[1], 31)
  expect_false(file.exists(file.path(dir, "rainfall_monthly_mean_wide.csv")))
})

test_that("rating curves are standardized before export", {
  x <- list(
    rating_curves = data.frame(
      CodigoEstacao = "10100000",
      Periodo_Validade_Inicio = "01/01/2020",
      Periodo_Validade_Fim = "31/12/2020",
      Cota_Minima = "10,0",
      Cota_Maxima = "200,0",
      Coeficiente_Ajuste_A = "280,7005",
      Coeficiente_Ajuste_N = "1,7",
      Coeficiente_Ajuste_H0 = "-6,16",
      Numero_Curva = "01/01",
      stringsAsFactors = FALSE
    )
  )
  dir <- tempfile("hydro_rating_")
  write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  out <- utils::read.csv(file.path(dir, "rating_curves.csv"), check.names = FALSE)
  expect_equal(out$a[1], 280.7005)
  expect_equal(out$b[1], 1.7)
  expect_equal(out$h0[1], -6.16)
  expect_match(out$`Validade de data`[1], "01/01/2020")
})


# ---- STAGE09L_EXPORT_CALENDAR_DIAGNOSTICS_TESTS_BEGIN ----

test_that("write_hydro_data completes daily export calendars", {
  x <- data.frame(
    station_code = "01845002",
    date = as.Date(c("2025-03-01", "2025-03-03")),
    variable = "rainfall",
    value = c(1, 3),
    unit = "mm",
    consistency_level = "2",
    stringsAsFactors = FALSE
  )
  dir <- tempfile("hydro_calendar_")
  write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  daily <- utils::read.csv(file.path(dir, "rainfall_daily_long.csv"), check.names = FALSE)
  wide <- utils::read.csv(file.path(dir, "rainfall_daily_wide.csv"), check.names = FALSE)
  miss <- utils::read.csv(file.path(dir, "rainfall_missing_counts.csv"), check.names = FALSE)
  expect_equal(nrow(daily), 3L)
  expect_true(is.na(daily$`Precipitacao`[daily$Data == "02/03/2025"]))
  expect_true(is.na(wide$`02`[1]))
  expect_equal(miss$Mar[1], 1L)
})

test_that("rainfall indices flag hydrological years without data", {
  x <- data.frame(
    station_code = "01845002",
    date = seq.Date(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day"),
    variable = "rainfall",
    value = NA_real_,
    unit = "mm",
    consistency_level = "2",
    stringsAsFactors = FALSE
  )
  dir <- tempfile("hydro_rain_missing_")
  write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  idx <- utils::read.csv(file.path(dir, "rainfall_indices.csv"), check.names = FALSE)
  expect_true(is.na(idx$total_mm[1]))
  expect_true(is.na(idx$R10[1]))
  expect_equal(idx$dias_falhos[1], 365L)
  expect_match(idx$Flag[1], "ano sem dados")
})

test_that("write_hydro_data reports empty API daily products", {
  x <- list(
    request_report = data.frame(
      station_code = "56460000",
      product = c("daily_discharge", "daily_discharge", "rating_curves"),
      status = c("empty", "empty", "empty"),
      n_rows = c(0, 0, 0),
      stringsAsFactors = FALSE
    )
  )
  dir <- tempfile("hydro_api_empty_")
  write_hydro_data(x, dir, format = "csv", overwrite = TRUE)
  expect_true(file.exists(file.path(dir, "discharge_diagnostics.csv")))
  expect_true(file.exists(file.path(dir, "rating_diagnostics.csv")))
  diag <- utils::read.csv(file.path(dir, "discharge_diagnostics.csv"), check.names = FALSE)
  expect_equal(diag$status[1], "warning")
  expect_match(diag$message[1], "empty daily_discharge")
})

# ---- STAGE09L_EXPORT_CALENDAR_DIAGNOSTICS_TESTS_END ----
