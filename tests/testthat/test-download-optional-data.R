test_that("download_ana_cross_section_vertices reads local optional vertices file", {
  tmp_dir <- tempfile("hydrodatabr-cache-")
  dir.create(tmp_dir)
  
  source_file <- tempfile(fileext = ".rds")
  metadata_file <- tempfile(fileext = ".csv")
  
  x <- data.frame(
    station_code = "00000000",
    cross_section_id = "s1",
    vertex_order = 1:2,
    distance_m = c(0, 10),
    elevation_m = c(100, 99),
    stringsAsFactors = FALSE
  )
  
  saveRDS(x, source_file, version = 3)
  sha <- unname(tools::sha256sum(source_file))
  
  metadata <- data.frame(
    object = "ana_cross_section_vertices",
    snapshot = "2026-06",
    file = basename(source_file),
    sha256 = sha,
    stringsAsFactors = FALSE
  )
  utils::write.csv(metadata, metadata_file, row.names = FALSE)
  
  out <- download_ana_cross_section_vertices(
    path = tmp_dir,
    url = source_file,
    metadata_url = metadata_file,
    quiet = TRUE
  )
  
  expect_s3_class(out, "data.frame")
  expect_equal(out, x)
  
  local_path <- download_ana_cross_section_vertices(
    path = tmp_dir,
    url = source_file,
    metadata_url = metadata_file,
    read = FALSE,
    quiet = TRUE
  )
  
  expect_true(file.exists(local_path))
  expect_true(grepl("ana_cross_section_vertices_2026-06[.]rds$", local_path))
})

test_that("download_ana_cross_section_vertices validates supported version", {
  expect_error(
    download_ana_cross_section_vertices(version = "2025-01", quiet = TRUE),
    "Only version '2026-06'"
  )
})