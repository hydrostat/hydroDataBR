test_that("cross-section query uses ANA Swagger parameter names", {
  query <- hydroDataBR:::ana_build_cross_sections_query(
    station_code = "10100000",
    start_date = as.Date("2019-01-01"),
    end_date = as.Date("2019-12-31"),
    consistency_level = NULL
  )

  code_name <- "C\u00f3digo da Esta\u00e7\u00e3o"

  expect_true(code_name %in% names(query))
  expect_equal(query[[code_name]], "10100000")
  expect_equal(query[["Tipo Filtro Data"]], "DATA_LEITURA")
  expect_equal(query[["Data Inicial (yyyy-MM-dd)"]], "2019-01-01")
  expect_equal(query[["Data Final (yyyy-MM-dd)"]], "2019-12-31")
})

test_that("cross-section query requires dates and enforces period length", {
  expect_error(
    hydroDataBR:::ana_build_cross_sections_query(
      station_code = "10100000",
      start_date = NULL,
      end_date = as.Date("2019-12-31"),
      consistency_level = NULL
    ),
    "start_date"
  )

  expect_error(
    hydroDataBR:::ana_build_cross_sections_query(
      station_code = "10100000",
      start_date = as.Date("2019-01-01"),
      end_date = as.Date("2020-12-31"),
      consistency_level = NULL
    ),
    "366"
  )
})

test_that("cross-section WebService items are standardized into sections and vertices", {
  response <- list(
    status = "OK",
    code = 200,
    message = "Sucesso",
    items = list(
      list(
        Cota = "-920.0",
        Data_Hora_Medicao = "2019-10-22 11:00:00.0",
        Data_Ultima_Alteracao = "2020-05-19 00:00:00.0",
        Distancia = "316.74",
        Distancia_pipf = "813.5",
        Eixo_X_Dist_Maxima = "813.5",
        Eixo_X_Dist_Minima = "0.0",
        Eixo_Y_Cota_Maxima = "1450.0",
        Eixo_Y_Cota_Minima = "-1218.0",
        Elm_Geom_Passo_Cota = "10.0",
        Nivel_Consistencia = "1",
        Num_Levantamento = "1",
        Num_Verticais = "618",
        Observacoes = NULL,
        Registro_ID = "6.7386491E7",
        Tipo_Secao = "1",
        codigoestacao = "10100000",
        verticais = "[]"
      ),
      list(
        Cota = "172.0",
        Data_Hora_Medicao = "2019-10-22 11:00:00.0",
        Data_Ultima_Alteracao = "2020-05-19 00:00:00.0",
        Distancia = "740.97",
        Distancia_pipf = "813.5",
        Eixo_X_Dist_Maxima = "813.5",
        Eixo_X_Dist_Minima = "0.0",
        Eixo_Y_Cota_Maxima = "1450.0",
        Eixo_Y_Cota_Minima = "-1218.0",
        Elm_Geom_Passo_Cota = "10.0",
        Nivel_Consistencia = "1",
        Num_Levantamento = "1",
        Num_Verticais = "618",
        Observacoes = NULL,
        Registro_ID = "6.7386491E7",
        Tipo_Secao = "1",
        codigoestacao = "10100000",
        verticais = "[]"
      )
    )
  )

  out <- hydroDataBR:::ana_standardize_cross_sections(response)

  expect_s3_class(out$sections, "data.frame")
  expect_s3_class(out$vertices, "data.frame")
  expect_equal(nrow(out$sections), 1)
  expect_equal(nrow(out$vertices), 2)
  expect_equal(out$sections$station_code, "10100000")
  expect_equal(out$vertices$distance_m, c(316.74, 740.97))
  expect_equal(out$vertices$stage_cm, c(-920, 172))
})

test_that("HidroWeb ZIP PerfilTransversal rows are standardized", {
  row <- data.frame(
    EstacaoCodigo = "56460000",
    NivelConsistencia = "2",
    Data = "17/05/2013",
    Hora = "01/01/1900 10:20",
    NumLevantamento = "27",
    TipoSecao = "2",
    NumVerticais = "2",
    DistanciaPIPF = "49,18",
    EixoXDistMaxima = "49,4827",
    EixoXDistMinima = "0,3027",
    EixoYCotaMaxima = "697,0",
    EixoYCotaMinima = "-52,0",
    ElmGeomPassoCota = "10,0",
    Observacoes = "Teste",
    Vertical = "9,4827,436|46,5127,420",
    stringsAsFactors = FALSE
  )

  out <- hydroDataBR:::ana_standardize_cross_sections(row)

  expect_equal(nrow(out$sections), 1)
  expect_equal(nrow(out$vertices), 2)
  expect_equal(out$vertices$distance_m, c(9.4827, 46.5127))
  expect_equal(out$vertices$stage_cm, c(436, 420))
})