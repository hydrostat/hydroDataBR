test_that("Stage 10D product plan reads built-in ana_stations inventory", {
  inv <- tryCatch(getExportedValue("hydroDataBR", "ana_stations"), error = function(e) NULL)
  if (is.null(inv) || !is.data.frame(inv) || !"station_code" %in% names(inv)) {
    testthat::skip("ana_stations inventory is not available in this test context")
  }
  if (!"56460000" %in% as.character(inv$station_code)) {
    testthat::skip("station 56460000 is not available in ana_stations")
  }

  plan <- hydroDataBR:::ana_station_product_plan(
    station_code = "56460000",
    include_cross_sections = FALSE
  )

  expect_true(all(plan$station_kind == "discharge"))

  discharge <- plan[plan$product == "daily_discharge", , drop = FALSE]
  stage <- plan[plan$product == "daily_stage", , drop = FALSE]
  rainfall <- plan[plan$product == "daily_rainfall", , drop = FALSE]
  measurements <- plan[plan$product == "discharge_measurements", , drop = FALSE]
  rating <- plan[plan$product == "rating_curves", , drop = FALSE]
  cross <- plan[plan$product == "cross_sections", , drop = FALSE]

  expect_true(discharge$attempt[[1L]])
  expect_true(stage$attempt[[1L]])
  expect_false(rainfall$attempt[[1L]])
  expect_true(measurements$attempt[[1L]])
  expect_true(rating$attempt[[1L]])
  expect_false(cross$attempt[[1L]])

  expect_identical(discharge$date_start[[1L]], as.Date("1965-10-01"))
  expect_identical(discharge$date_end[[1L]], as.Date("2018-12-01"))
  expect_identical(measurements$date_start[[1L]], as.Date("1965-10-01"))
  expect_identical(rating$date_start[[1L]], as.Date("1965-10-01"))
})
