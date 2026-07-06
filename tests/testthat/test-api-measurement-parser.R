test_that("resumo_descarga pipeline field names are standardized", {
  discharge_name <- paste0("Vaz", intToUtf8(0x00E3), "o")
  record <- list(
    CodigoEstacao = "56460000",
    Data_Hora_Dado = "2015-01-15T10:30:00",
    Nivel_Consistencia = "2",
    Cota = "123",
    Area_Molhada = "10.5",
    Largura = "20",
    Profundidade = "0.5",
    Vel_Media = "2.3"
  )
  record[[discharge_name]] <- "45.6"

  out <- hydroDataBR:::ana_standardize_discharge_measurements(list(items = list(record)))

  expect_equal(nrow(out), 1L)
  expect_identical(out$station_code, "56460000")
  expect_equal(as.Date(out$measurement_datetime), as.Date("2015-01-15"))
  expect_identical(out$consistency_level, "2")
  expect_equal(out$stage_cm, 123)
  expect_equal(out$discharge_m3s, 45.6)
  expect_equal(out$wetted_area_m2, 10.5)
  expect_equal(out$width_m, 20)
  expect_equal(out$mean_depth_m, 0.5)
  expect_equal(out$mean_velocity_ms, 2.3)
})
