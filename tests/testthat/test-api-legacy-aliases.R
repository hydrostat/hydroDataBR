test_that("Stage 10D Stage 04 parameter helpers and aliases are stable", {
  expect_identical(hydroDataBR:::ana_stage04_station_param(), "Código da Estação")
  expect_identical(hydroDataBR:::ana_stage04_start_param(), "Data Inicial (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_end_param(), "Data Final (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_type_filter_param(), "Tipo Filtro Data")
  expect_identical(hydroDataBR:::ana_stage04_filter_date_param(), "Tipo Filtro Data")

  expect_identical(hydroDataBR:::ana_stage04_param_station, "Código da Estação")
  expect_identical(hydroDataBR:::ana_stage04_param_start, "Data Inicial (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_param_end, "Data Final (yyyy-MM-dd)")
  expect_identical(hydroDataBR:::ana_stage04_param_filter_date, "Tipo Filtro Data")
  expect_identical(hydroDataBR:::ana_stage04_param_date_filter, "Tipo Filtro Data")
})
