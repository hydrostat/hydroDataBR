test_that("Stage 05A reference tables are standardized from mocked responses", {
  token <- list(access_token = "hidden")

  mock_states <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "Estados")
    expect_equal(query, list())

    list(Dados = list(
      list(SiglaUF = "MG", Nome = "Minas Gerais"),
      list(SiglaUF = "SP", Nome = "Sao Paulo")
    ))
  }

  states <- get_ana_states(token, request_function = mock_states)

  expect_s3_class(states, "tbl_df")
  expect_named(states, c("state_code", "state_name", "source"))
  expect_equal(states$state_code, c("MG", "SP"))
  expect_equal(states$source, rep("ana_hidrowebservice", 2))
})

test_that("Stage 05A municipalities keep stable contract", {
  token <- list(access_token = "hidden")

  mock_municipalities <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "Municipios")
    expect_equal(query, list(state_code = "MG"))

    list(data = list(
      list(CodigoMunicipio = 3106200, NomeMunicipio = "Belo Horizonte", SiglaUF = "MG", NomeEstado = "Minas Gerais"),
      list(CodigoMunicipio = 3170206, NomeMunicipio = "Uberlandia", SiglaUF = "MG", NomeEstado = "Minas Gerais")
    ))
  }

  municipalities <- get_ana_municipalities(
    token,
    state_code = "MG",
    request_function = mock_municipalities
  )

  expect_named(
    municipalities,
    c("municipality_code", "municipality", "state_code", "state_name", "source")
  )
  expect_equal(municipalities$municipality_code, c("3106200", "3170206"))
  expect_equal(municipalities$state_code, c("MG", "MG"))
})

test_that("Stage 05A basins and subbasins keep stable contracts", {
  token <- list(access_token = "hidden")

  mock_basins <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "Bacias")

    list(data = list(
      list(CodigoBacia = "4", NomeBacia = "Sao Francisco"),
      list(CodigoBacia = "6", NomeBacia = "Parana")
    ))
  }

  basins <- get_ana_basins(token, request_function = mock_basins)

  expect_named(basins, c("basin_code", "basin_name", "source"))
  expect_equal(basins$basin_code, c("4", "6"))

  mock_subbasins <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "SubBacias")
    expect_equal(query, list(basin_code = "4"))

    list(data = list(
      list(CodigoSubBacia = "40", NomeSubBacia = "Alto Sao Francisco", CodigoBacia = "4", NomeBacia = "Sao Francisco")
    ))
  }

  subbasins <- get_ana_subbasins(
    token,
    basin_code = "4",
    request_function = mock_subbasins
  )

  expect_named(
    subbasins,
    c("subbasin_code", "subbasin_name", "basin_code", "basin_name", "source")
  )
  expect_equal(subbasins$subbasin_code, "40")
  expect_equal(subbasins$basin_code, "4")
})

test_that("Stage 05A rivers and entities keep stable contracts", {
  token <- list(access_token = "hidden")

  mock_rivers <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "Rios")

    list(data = list(
      list(CodigoRio = "761", NomeRio = "Rio das Velhas")
    ))
  }

  rivers <- get_ana_rivers(token, request_function = mock_rivers)

  expect_named(rivers, c("river_code", "river_name", "source"))
  expect_equal(rivers$river_code, "761")

  mock_entities <- function(token, endpoint, query = list()) {
    expect_equal(endpoint, "Entidades")

    list(data = list(
      list(CodigoEntidade = "1", NomeEntidade = "Agencia Nacional de Aguas", SiglaEntidade = "ANA")
    ))
  }

  entities <- get_ana_entities(token, request_function = mock_entities)

  expect_named(entities, c("entity_code", "entity_name", "entity_acronym", "source"))
  expect_equal(entities$entity_acronym, "ANA")
})

test_that("Stage 05A reference parsing accepts data frames directly", {
  states <- ana_standardize_states(data.frame(
    UF = c("RJ", "ES"),
    NomeEstado = c("Rio de Janeiro", "Espirito Santo")
  ))

  expect_equal(states$state_code, c("RJ", "ES"))
  expect_equal(states$state_name, c("Rio de Janeiro", "Espirito Santo"))
})
