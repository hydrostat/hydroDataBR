test_that("Stage 08A fix5 uses rainfall bars and plots curve validity", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day")
  rainfall <- data.frame(
    station_code = "10100000",
    date = dates,
    variable = "rainfall",
    value = rep(c(0, 2, 5), length.out = length(dates)),
    unit = "mm",
    consistency_level = 1L,
    source_status = "fixture",
    source = "test",
    stringsAsFactors = FALSE
  )

  monthly_plot <- plot_monthly_summary(
    rainfall,
    variable = "rainfall",
    statistic = "total",
    value_col = "mean_total_value"
  )
  layer_geoms <- unlist(lapply(monthly_plot$layers, function(layer) class(layer$geom)))
  expect_true("GeomBar" %in% layer_geoms)

  curves <- data.frame(
    rating_curve_id = c("1", "1", "2"),
    rating_curve_segment_id = c("A", "B", "A"),
    coefficient_a = c(0.02, 0.025, 0.021),
    coefficient_h0_cm = c(80, 90, 82),
    coefficient_b = c(1.5, 1.4, 1.55),
    stage_min_cm = c(90, 150, 95),
    stage_max_cm = c(150, 220, 210),
    valid_from = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01")),
    valid_to = as.Date(c("2020-12-31", "2020-12-31", "2021-12-31")),
    stringsAsFactors = FALSE
  )

  validity_plot <- plot_rating_curve_validity(curves)
  expect_s3_class(validity_plot, "ggplot")
  validity_data <- attr(validity_plot, "hydrodatabr_data")
  expect_s3_class(validity_data, "data.frame")
  expect_true(all(c("valid_from", "valid_to_plot", "stage_min_cm", "stage_max_cm") %in% names(validity_data)))
  expect_equal(nrow(validity_data), 3)
})
