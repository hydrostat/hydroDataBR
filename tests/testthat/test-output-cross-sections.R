test_that("Stage 08D cross-section plots and tables use simplified dispatchers", {
  sections <- data.frame(
    station_code = c("001", "001"),
    cross_section_id = c("s1", "s2"),
    measurement_datetime = as.Date(c("2020-01-10", "2021-01-10")),
    survey_number = c("1", "2"),
    consistency_level = c(1L, 2L),
    n_vertices = c(5L, 5L),
    stringsAsFactors = FALSE
  )

  vertices <- data.frame(
    station_code = rep("001", 10),
    cross_section_id = rep(c("s1", "s2"), each = 5),
    measurement_datetime = rep(as.Date(c("2020-01-10", "2021-01-10")), each = 5),
    vertex_order = rep(1:5, 2),
    vertex_distance_m = rep(c(0, 4, 8, 12, 16), 2),
    vertex_stage_cm = c(220, 180, 150, 175, 215, 230, 185, 145, 170, 210),
    stringsAsFactors = FALSE
  )

  x <- list(cross_sections = list(sections = sections, vertices = vertices))

  profile <- plot_hydro_data(x, plot = "cross_section_profile", section_id = "s1")
  overlay <- plot_hydro_data(x, plot = "cross_section_overlay", section_id = "s1")
  timeline <- plot_hydro_data(x, plot = "cross_section_timeline", section_id = "s1")
  expect_s3_class(profile, "ggplot")
  expect_s3_class(overlay, "ggplot")
  expect_s3_class(timeline, "ggplot")
  expect_match(profile$labels$caption, "2020-01-10")
  expect_match(overlay$labels$caption, "s1")

  date_selected <- plot_hydro_data(x, plot = "cross_section_profile", section_date = as.Date("2021-01-10"))
  expect_s3_class(date_selected, "ggplot")
  expect_match(date_selected$labels$caption, "2021-01-10")

  table_sections <- table_hydro_data(x, table = "cross_sections")
  expect_s3_class(table_sections, "data.frame")
  expect_equal(nrow(table_sections), 2)
  expect_true(all(c("cross_section_id", "n_vertices", "cross_section_distance_span_m") %in% names(table_sections)))

  table_vertices <- table_hydro_data(x, table = "cross_sections", level = "vertices")
  expect_equal(nrow(table_vertices), 10)
  expect_true(all(c("vertex_distance_m", "vertex_stage_cm") %in% names(table_vertices)))

  table_summary <- table_hydro_data(x, table = "cross_sections", level = "summary")
  expect_equal(nrow(table_summary), 1)
  expect_equal(table_summary$n_cross_sections, 2)
})

test_that("Stage 08D cross-section dispatch handles batch objects", {
  make_cross <- function(station_code) {
    sections <- data.frame(
      cross_section_id = c("a", "b"),
      measurement_datetime = as.Date(c("2020-01-01", "2021-01-01")),
      stringsAsFactors = FALSE
    )
    vertices <- data.frame(
      cross_section_id = rep(c("a", "b"), each = 4),
      vertex_order = rep(1:4, 2),
      vertex_distance_m = rep(c(0, 5, 10, 15), 2),
      vertex_stage_cm = c(200, 160, 170, 210, 205, 155, 165, 215),
      stringsAsFactors = FALSE
    )
    list(cross_sections = list(sections = sections, vertices = vertices))
  }

  batch <- list(results = list("001" = make_cross("001"), "002" = make_cross("002")))

  expect_error(plot_hydro_data(batch, plot = "cross_section_profile"), "multiple stations")
  plots <- plot_hydro_data(batch, plot = "cross_section_profile", multi_station = "list")
  expect_type(plots, "list")
  expect_equal(names(plots), c("001", "002"))
  expect_s3_class(plots[[1]], "ggplot")

  table_summary <- table_hydro_data(batch, table = "cross_sections", level = "summary")
  expect_equal(nrow(table_summary), 2)
  expect_true(all(c("001", "002") %in% table_summary$station_code))
})
