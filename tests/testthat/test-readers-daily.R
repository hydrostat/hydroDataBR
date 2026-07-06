required_daily_columns <- c(
  "station_code",
  "date",
  "variable",
  "value",
  "unit",
  "consistency_level",
  "source_status",
  "source"
)

expect_daily_contract <- function(x) {
  testthat::expect_s3_class(x, "tbl_df")
  testthat::expect_named(x, required_daily_columns)
  testthat::expect_type(x$station_code, "character")
  testthat::expect_s3_class(x$date, "Date")
  testthat::expect_type(x$variable, "character")
  testthat::expect_type(x$value, "double")
  testthat::expect_type(x$unit, "character")
  testthat::expect_type(x$consistency_level, "character")
  testthat::expect_type(x$source_status, "character")
  testthat::expect_type(x$source, "character")
}

test_that("read_hidroweb() reads HidroWeb CSV and applies the daily contract", {
  x <- hydroDataBR::read_hidroweb(
    testthat::test_path("fixtures", "hidroweb_discharge.csv"),
    variables = "discharge"
  )

  expect_daily_contract(x)
  testthat::expect_true(all(x$variable == "discharge"))
  testthat::expect_true(all(x$unit == "m3/s"))
  testthat::expect_equal(unique(x$station_code), "01234567")
})

test_that("read_hidroweb() preserves leading zeros and prefers consistency level 2", {
  x <- hydroDataBR::read_hidroweb(
    testthat::test_path("fixtures", "hidroweb_discharge.csv"),
    variables = "discharge"
  )

  jan_01 <- x[x$date == as.Date("2020-01-01"), ]
  testthat::expect_equal(nrow(jan_01), 1L)
  testthat::expect_equal(jan_01$station_code, "01234567")
  testthat::expect_equal(jan_01$consistency_level, "2")
  testthat::expect_equal(jan_01$value, 10.2)
  testthat::expect_equal(jan_01$source_status, "2")
})

test_that("read_hidroweb() discards invalid generated month-day dates", {
  x <- hydroDataBR::read_hidroweb(
    testthat::test_path("fixtures", "hidroweb_discharge.csv"),
    variables = "discharge"
  )

  testthat::expect_false(any(x$date == as.Date("2020-03-01"), na.rm = TRUE))
  testthat::expect_false(any(format(x$date, "%Y-%m-%d") %in% c("2020-02-30", "2020-02-31")))
})

test_that("read_hidroweb() reads a HidroWeb ZIP fixture", {
  x <- hydroDataBR::read_hidroweb(
    testthat::test_path("fixtures", "hidroweb_daily.zip"),
    variables = "all"
  )

  expect_daily_contract(x)
  testthat::expect_setequal(unique(x$variable), c("discharge", "rainfall"))
})

test_that("read_ana_xml() reads legacy XML daily series", {
  x <- hydroDataBR::read_ana_xml(
    testthat::test_path("fixtures", "ana_daily.xml"),
    variables = "stage"
  )

  expect_daily_contract(x)
  testthat::expect_true(all(x$variable == "stage"))
  testthat::expect_true(all(x$unit == "cm"))

  jan_01 <- x[x$date == as.Date("2020-01-01"), ]
  testthat::expect_equal(jan_01$value, 110)
  testthat::expect_equal(jan_01$consistency_level, "2")
})

test_that("read_ana_json() reads ANA API JSON daily series", {
  x <- hydroDataBR::read_ana_json(
    testthat::test_path("fixtures", "ana_daily.json"),
    variables = c("rainfall", "discharge")
  )

  expect_daily_contract(x)
  testthat::expect_setequal(unique(x$variable), c("discharge", "rainfall"))

  rainfall <- x[x$variable == "rainfall", ]
  testthat::expect_equal(rainfall$value, 7)
  testthat::expect_equal(rainfall$unit, "mm")
})

test_that("offline readers reject URLs at this stage", {
  testthat::expect_error(
    hydroDataBR::read_ana_xml("https://example.org/file.xml"),
    "arquivos locais|arquivo local"
  )
})
