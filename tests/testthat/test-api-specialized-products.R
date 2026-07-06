testthat::test_that("get_ana_discharge_measurements standardizes mocked responses", {
  response <- list(
    items = list(
      list(
        CodigoEstacao = "01540000",
        DataMedicao = "2020-01-15 10:30:00",
        NivelConsistencia = "2",
        Cota = "123",
        Vazao = "45.67",
        AreaMolhada = "20.5",
        Largura = "50",
        ProfundidadeMedia = "1.4",
        VelocidadeMedia = "2.2"
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

  measurements <- get_ana_discharge_measurements(
    token = structure(list(), class = "ana_token"),
    station_code = "01540000",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )

  testthat::expect_s3_class(measurements, "tbl_df")
  testthat::expect_named(
    measurements,
    c(
      "station_code",
      "measurement_datetime",
      "consistency_level",
      "stage_cm",
      "discharge_m3s",
      "wetted_area_m2",
      "width_m",
      "mean_depth_m",
      "mean_velocity_ms",
      "source"
    )
  )
  testthat::expect_identical(measurements$station_code, "01540000")
  testthat::expect_equal(measurements$stage_cm, 123)
  testthat::expect_equal(measurements$discharge_m3s, 45.67)
  testthat::expect_equal(measurements$mean_velocity_ms, 2.2)
  testthat::expect_identical(captured$endpoint, "/HidroSerieResumoDescarga/v1")
  station_param <- hydroDataBR:::ana_stage04_station_param()
  start_param <- hydroDataBR:::ana_stage04_start_param()
  end_param <- hydroDataBR:::ana_stage04_end_param()
  date_filter_param <- hydroDataBR:::ana_stage04_type_filter_param()
  testthat::expect_identical(captured$query[[station_param]], "01540000")
  testthat::expect_identical(captured$query[[start_param]], "2020-01-01")
  testthat::expect_identical(captured$query[[end_param]], "2020-12-31")
  testthat::expect_identical(captured$query[[date_filter_param]], "DATA_LEITURA")
  testthat::expect_false("CodigoDaEstacao" %in% names(captured$query))
  testthat::expect_false("DataInicio" %in% names(captured$query))
  testthat::expect_false("DataFim" %in% names(captured$query))
})

testthat::test_that("get_ana_rating_curves standardizes mocked rating-curve segments", {
  response <- list(
    items = list(
      list(
        CodigoCurva = "curve-1",
        CodigoTrecho = "curve-1-1",
        CodigoEstacao = "01540000",
        DataInicio = "2020-01-01",
        DataFim = "2020-12-31",
        NivelConsistencia = "2",
        NumeroTrecho = "1",
        NumeroTotalTrechos = "2",
        TipoCurva = "Potencia",
        TipoEquacao = "Q=a*(H-H0)^n",
        CotaMinima = "100",
        CotaMaxima = "300",
        IntervaloCota = "10",
        CoeficienteA = "0.4135",
        CoeficienteH0 = "96",
        CoeficienteN = "1.358"
      ),
      list(
        CodigoCurva = "curve-1",
        CodigoTrecho = "curve-1-2",
        CodigoEstacao = "01540000",
        DataInicio = "2020-01-01",
        DataFim = "2020-12-31",
        NivelConsistencia = "2",
        NumeroTrecho = "2",
        NumeroTotalTrechos = "2",
        TipoCurva = "Potencia",
        TipoEquacao = "Q=a*(H-H0)^n",
        CotaMinima = "301",
        CotaMaxima = "600",
        IntervaloCota = "10",
        CoeficienteA = "0.5000",
        CoeficienteH0 = "96",
        CoeficienteN = "1.400"
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

  curves <- get_ana_rating_curves(
    token = structure(list(), class = "ana_token"),
    station_code = "01540000"
  )

  testthat::expect_s3_class(curves, "tbl_df")
  testthat::expect_named(
    curves,
    c(
      "rating_curve_id",
      "rating_curve_segment_id",
      "station_code",
      "valid_from",
      "valid_to",
      "consistency_level",
      "segment_number",
      "n_segments_reported",
      "curve_type",
      "equation_type",
      "stage_min_cm",
      "stage_max_cm",
      "table_stage_step_cm",
      "coefficient_a",
      "coefficient_h0_cm",
      "coefficient_n",
      "source"
    )
  )
  testthat::expect_equal(nrow(curves), 2)
  testthat::expect_equal(curves$segment_number, c(1L, 2L))
  testthat::expect_equal(curves$coefficient_h0_cm, c(96, 96))
  testthat::expect_identical(captured$endpoint, "/HidroSerieCurvaDescarga/v1")
  rating_station_param <- hydroDataBR:::ana_stage04_station_param()
  rating_date_filter_param <- hydroDataBR:::ana_stage04_type_filter_param()
  testthat::expect_identical(captured$query[[rating_station_param]], "01540000")
  testthat::expect_false(rating_date_filter_param %in% names(captured$query))
  testthat::expect_false("CodigoDaEstacao" %in% names(captured$query))
})

test_that("get_ana_cross_sections returns sections and vertices", {
  response <- list(
    items = list(
      list(
        codigoestacao = "10100000",
        Data_Hora_Medicao = "2019-10-22 11:00:00.0",
        Data_Ultima_Alteracao = "2020-05-19 00:00:00.0",
        Nivel_Consistencia = "1",
        Num_Levantamento = "1",
        Tipo_Secao = "1",
        Registro_ID = "A",
        Num_Verticais = "2",
        Distancia_pipf = "5.5",
        Eixo_X_Dist_Minima = "0.0",
        Eixo_X_Dist_Maxima = "5.5",
        Eixo_Y_Cota_Minima = "100.0",
        Eixo_Y_Cota_Maxima = "101.0",
        Distancia = "0.0",
        Cota = "100.0",
        verticais = "[]"
      ),
      list(
        codigoestacao = "10100000",
        Data_Hora_Medicao = "2019-10-22 11:00:00.0",
        Data_Ultima_Alteracao = "2020-05-19 00:00:00.0",
        Nivel_Consistencia = "1",
        Num_Levantamento = "1",
        Tipo_Secao = "1",
        Registro_ID = "A",
        Num_Verticais = "2",
        Distancia_pipf = "5.5",
        Eixo_X_Dist_Minima = "0.0",
        Eixo_X_Dist_Maxima = "5.5",
        Eixo_Y_Cota_Minima = "100.0",
        Eixo_Y_Cota_Maxima = "101.0",
        Distancia = "5.5",
        Cota = "101.0",
        verticais = "[]"
      )
    )
  )

  cross_sections <- ana_standardize_cross_sections(response)

  expect_named(cross_sections, c("sections", "vertices"))
  expect_s3_class(cross_sections$sections, "data.frame")
  expect_s3_class(cross_sections$vertices, "data.frame")

  expect_true(all(c(
    "section_id",
    "station_code",
    "measurement_datetime",
    "consistency_level",
    "survey_number",
    "section_type",
    "n_vertices"
  ) %in% names(cross_sections$sections)))

  expect_true(all(c(
    "section_id",
    "station_code",
    "measurement_datetime",
    "consistency_level",
    "survey_number",
    "section_type",
    "vertex_index",
    "distance_m",
    "stage_cm"
  ) %in% names(cross_sections$vertices)))

  expect_equal(nrow(cross_sections$sections), 1L)
  expect_equal(nrow(cross_sections$vertices), 2L)
  expect_equal(cross_sections$sections$n_vertices, 2L)
  expect_equal(cross_sections$vertices$vertex_index, c(1L, 2L))
  expect_equal(cross_sections$vertices$distance_m, c(0, 5.5))
  expect_equal(cross_sections$vertices$stage_cm, c(100, 101))
})
