test_that("ANA API windows never exceed 366 inclusive days", {
  windows <- hydroDataBR:::ana_split_api_windows("2005-01-01", "2006-12-31")
  n_days <- as.integer(windows$end_date - windows$start_date) + 1L
  expect_true(all(n_days <= 366L))
  expect_equal(min(windows$start_date), as.Date("2005-01-01"))
  expect_equal(max(windows$end_date), as.Date("2006-12-31"))
})

test_that("single ANA API request windows reject periods longer than 366 days", {
  expect_error(
    hydroDataBR:::ana_assert_api_window("2005-01-01", "2006-01-02"),
    "at most 366 days"
  )
  expect_silent(hydroDataBR:::ana_assert_api_window("2005-01-01", "2005-12-31"))
})

