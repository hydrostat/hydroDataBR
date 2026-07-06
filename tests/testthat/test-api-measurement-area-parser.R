test_that("resumo_descarga wetted area is read from downloaded field names", {
  discharge_name <- paste0("Vaz", intToUtf8(0x00E3), "o")
  area_name <- paste0(intToUtf8(0x00C1), "rea Molhada (m", intToUtf8(0x00B2), ")")
  record <- list(
    CodigoEstacao = "56460000",
    Data_Hora_Dado = "2015-01-15T10:30:00",
    Nivel_Consistencia = "2",
    Cota = "123",
    Largura = "20",
    Profundidade = "0.5",
    Vel_Media = "2.3"
  )
  record[[discharge_name]] <- "45.6"
  record[[area_name]] <- "10,5"

  out <- hydroDataBR:::ana_standardize_discharge_measurements(list(items = list(record)))

  expect_equal(nrow(out), 1L)
  expect_equal(out$wetted_area_m2, 10.5)
  expect_equal(out$discharge_m3s, 45.6)
})

test_that("resumo_descarga area is not derived from discharge and velocity", {
  discharge_name <- paste0("Vaz", intToUtf8(0x00E3), "o")
  record <- list(
    CodigoEstacao = "56460000",
    Data_Hora_Dado = "2015-01-15T10:30:00",
    Nivel_Consistencia = "2",
    Cota = "123",
    Largura = "20",
    Vel_Media = "2.0"
  )
  record[[discharge_name]] <- "40"

  out <- hydroDataBR:::ana_standardize_discharge_measurements(list(items = list(record)))

  expect_true(is.na(out$wetted_area_m2))
})
