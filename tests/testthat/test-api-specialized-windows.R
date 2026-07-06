test_that("Stage 10D all products use one-year request windows", {
  products <- c(
    "daily_discharge", "daily_stage", "daily_rainfall",
    "discharge_measurements", "rating_curves", "cross_sections"
  )
  for (product in products) {
    w <- hydroDataBR:::ana_api_request_windows_for_product(
      product = product,
      date_start = "2000-01-01",
      date_end = "2016-12-31"
    )
    expect_gt(nrow(w), 1L)
    expect_identical(w$date_start[[1L]], as.Date("2000-01-01"))
    expect_identical(w$date_end[[nrow(w)]], as.Date("2016-12-31"))
    expect_true(all(as.numeric(w$date_end - w$date_start) <= 365))
  }
})

test_that("Stage 10D one-year windows handle short periods", {
  w <- hydroDataBR:::ana_api_request_windows_for_product(
    product = "daily_discharge",
    date_start = "2015-01-01",
    date_end = "2015-06-30"
  )
  expect_equal(nrow(w), 1L)
  expect_identical(w$date_start[[1L]], as.Date("2015-01-01"))
  expect_identical(w$date_end[[1L]], as.Date("2015-06-30"))
})
