test_that("Stage 11E daily table standardizer keeps the public daily contract", {
  raw <- data.frame(
    EstacaoCodigo = c("01234567", "01234567"),
    Data = c("2020-01-01", "2020-01-01"),
    NivelConsistencia = c("1", "2"),
    Vazao01 = c("1,0", "2,0"),
    Vazao01Status = c("1", "2"),
    stringsAsFactors = FALSE
  )

  out <- hydroDataBR:::.standardize_ana_daily_table(raw, "fixture")

  expect_s3_class(out, "tbl_df")
  expect_named(out, hydroDataBR:::ana_daily_contract_columns())
  expect_equal(nrow(out), 1L)
  expect_equal(out$station_code, "01234567")
  expect_identical(out$date, as.Date("2020-01-01"))
  expect_equal(out$variable, "discharge")
  expect_equal(out$value, 2)
  expect_equal(out$consistency_level, "2")
})

test_that("Stage 11E ANA API daily item parser accepts direct daily rows", {
  raw <- data.frame(
    CodigoEstacao = "01234567",
    Data = "2020-01-02",
    Valor = "3,5",
    NivelConsistencia = "2",
    stringsAsFactors = FALSE
  )

  out <- hydroDataBR:::ana_standardize_api_daily_items(
    raw,
    product = "daily_discharge",
    source = "fixture"
  )

  expect_named(out, hydroDataBR:::ana_daily_contract_columns())
  expect_equal(out$station_code, "01234567")
  expect_identical(out$date, as.Date("2020-01-02"))
  expect_equal(out$variable, "discharge")
  expect_equal(out$value, 3.5)
  expect_equal(out$unit, "m3/s")
})


test_that("Stage 11E daily contract preserves short station codes supplied by users", {
  raw <- data.frame(
    station_code = c("001", "001"),
    date = c("2020-01-01", "2020-01-01"),
    variable = "discharge",
    value = c(1, 2),
    unit = "m3/s",
    consistency_level = c("1", "2"),
    source_status = NA_character_,
    source = "fixture",
    flag_manual = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  out <- hydroDataBR:::hydro_prefer_daily_observations(raw)
  expect_equal(out$station_code, "001")
  expect_equal(out$value, 2)
  expect_true(out$flag_manual)
})
