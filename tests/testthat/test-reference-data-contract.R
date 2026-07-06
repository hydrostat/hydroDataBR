testthat::test_that("get_ana_stations validates required filters before requesting", {
  testthat::expect_error(
    get_ana_stations(token = structure(list(), class = "ana_token")),
    "At least one"
  )
})

testthat::test_that("get_ana_stations standardizes mocked inventory responses", {
  response <- list(
    status = "OK",
    code = 200,
    message = "Sucesso",
    items = list(
      list(
        codigoestacao = "01540000",
        Estacao_Nome = "PORTO VELHO",
        UF_Estacao = "RO",
        UF_Nome_Estacao = "RONDONIA",
        Municipio_Codigo = "1010000",
        Municipio_Nome = "PORTO VELHO",
        codigobacia = "1",
        Bacia_Nome = "RIO AMAZONAS",
        Tipo_Estacao = "Fluviometrica",
        Operando = "1",
        Latitude = "-8.7483",
        Longitude = "-63.9169",
        Altitude = "42.88",
        Area_Drenagem = "976000.0",
        Operadora_Codigo = "82",
        Operadora_Sigla = "CPRM",
        Responsavel_Sigla = "ANA",
        Data_Ultima_Atualizacao = "2023-12-19 00:00:00.0"
      )
    )
  )

  captured <- NULL

  testthat::local_mocked_bindings(
    ana_request_json = function(...) {
      captured <<- list(...)
      response
    }
  )

  stations <- get_ana_stations(
    token = structure(list(), class = "ana_token"),
    station_code = "01540000"
  )

  testthat::expect_s3_class(stations, "tbl_df")
  testthat::expect_named(
    stations,
    c(
      "station_code",
      "station_name",
      "state_code",
      "state_name",
      "municipality_code",
      "municipality",
      "basin_code",
      "basin_name",
      "station_type",
      "operating",
      "latitude",
      "longitude",
      "altitude_m",
      "drainage_area_km2",
      "operator_code",
      "operator_acronym",
      "responsible_acronym",
      "last_update",
      "source"
    )
  )
  testthat::expect_identical(stations$station_code, "01540000")
  testthat::expect_identical(stations$station_name, "PORTO VELHO")
  testthat::expect_identical(stations$state_code, "RO")
  testthat::expect_equal(stations$latitude, -8.7483)
  testthat::expect_equal(stations$longitude, -63.9169)
  testthat::expect_true(stations$operating)
  testthat::expect_identical(stations$last_update, as.Date("2023-12-19"))
  testthat::expect_identical(stations$source, "ana_hidrowebservice_inventory")
  testthat::expect_identical(captured$endpoint, "/HidroInventarioEstacoes/v1")
  testthat::expect_identical(captured$query[["C\u00f3digo da Esta\u00e7\u00e3o"]], "01540000")
})

testthat::test_that("get_ana_stations supports state-code inventory queries", {
  response <- list(items = list())
  captured <- NULL

  testthat::local_mocked_bindings(
    ana_request_json = function(...) {
      captured <<- list(...)
      response
    }
  )

  stations <- get_ana_stations(
    token = structure(list(), class = "ana_token"),
    state_code = "mg"
  )

  testthat::expect_s3_class(stations, "tbl_df")
  testthat::expect_equal(nrow(stations), 0)
  testthat::expect_identical(captured$query[["UF"]], "MG")
})
