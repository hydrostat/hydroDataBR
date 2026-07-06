test_that("Stage 10D consolidated helpers are load-order safe", {
  expect_true(is.function(hydroDataBR:::ana_api_result_rows))
  expect_true(is.function(hydroDataBR:::ana_api_n_rows))
  expect_equal(hydroDataBR:::ana_api_result_rows(data.frame(a = 1:3)), 3L)
  expect_equal(hydroDataBR:::ana_api_n_rows(list(data = data.frame(a = 1:2))), 2L)
})

test_that("Stage 10D specialized parameter helpers use pipeline names", {
  expect_identical(hydroDataBR:::ana_stage04_station_param(), "Código da Estação")
  expect_identical(hydroDataBR:::ana_stage04_start_param(), "Data Inicial (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_end_param(), "Data Final (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_type_filter_param(), "Tipo Filtro Data")
  expect_identical(hydroDataBR:::ana_stage04_filter_date_param(), "Tipo Filtro Data")
})

test_that("Stage 10D specialized queries use pipeline parameter names", {
  q <- hydroDataBR:::ana_stage04_product_query(
    product = "discharge_measurements",
    station_code = "01540000",
    start_date = "2020-01-01",
    end_date = "2020-12-31"
  )
  expect_identical(q[[hydroDataBR:::ana_stage04_station_param()]], "01540000")
  expect_identical(q[[hydroDataBR:::ana_stage04_start_param()]], "2020-01-01")
  expect_identical(q[[hydroDataBR:::ana_stage04_end_param()]], "2020-12-31")
  expect_identical(q[[hydroDataBR:::ana_stage04_type_filter_param()]], "DATA_LEITURA")
})
