test_that("specialized endpoint paths are relative to the authenticated base URL", {
  expect_equal(hydroDataBR:::ana_discharge_measurements_endpoint(), "/HidroSerieResumoDescarga/v1")
  expect_equal(hydroDataBR:::ana_rating_curves_endpoint(), "/HidroSerieCurvaDescarga/v1")
  expect_equal(hydroDataBR:::ana_cross_sections_endpoint(), "/HidroSeriePerfilTransversal/v1")

  url <- hydroDataBR:::ana_build_url(
    base_url = hydroDataBR:::ana_auth_base_url(),
    path = hydroDataBR:::ana_cross_sections_endpoint(),
    query = list()
  )

  expect_false(grepl("EstacoesTelemetricas/EstacoesTelemetricas", url, fixed = TRUE))
  expect_match(url, "/EstacoesTelemetricas/HidroSeriePerfilTransversal/v1$", fixed = FALSE)
})

test_that("HidroWeb ZIP cross-section files are read as sections and vertices", {
  tmp_dir <- tempfile("hidroweb_zip_cross_sections_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  csv_file <- file.path(tmp_dir, "56460000_PerfilTransversal.csv")
  writeLines(
    c(
      "Agencia Nacional de Aguas - ANA",
      "",
      "EstacaoCodigo;NivelConsistencia;Data;Hora;NumLevantamento;TipoSecao;NumVerticais;DistanciaPIPF;EixoXDistMaxima;EixoXDistMinima;EixoYCotaMaxima;EixoYCotaMinima;ElmGeomPassoCota;Observacoes;Vertical",
      "56460000;2;17/05/2013;01/01/1900 10:20;27;2;2;\"49,18\";\"49,4827\";\"0,3027\";\"697,0\";\"-52,0\";\"10,0\";\"Teste\";\"9,4827,436|46,5127,420|\""
    ),
    csv_file,
    useBytes = TRUE
  )

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip("station.zip", basename(csv_file), flags = "-q")
  zip_file <- file.path(tmp_dir, "station.zip")

  out <- read_hidroweb_cross_sections(zip_file)

  expect_named(out, c("sections", "vertices"))
  expect_s3_class(out$sections, "tbl_df")
  expect_s3_class(out$vertices, "tbl_df")
  expect_equal(nrow(out$sections), 1L)
  expect_equal(nrow(out$vertices), 2L)
  expect_equal(out$sections$station_code, "56460000")
  expect_equal(out$sections$n_vertices, 2L)
  expect_equal(out$vertices$vertex_order, c(1L, 2L))
  expect_equal(out$vertices$vertex_distance_m, c(9.4827, 46.5127))
  expect_equal(out$vertices$vertex_stage_cm, c(436, 420))
})
