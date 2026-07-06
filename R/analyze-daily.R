# Daily-series analysis and summary contracts.
# Source-neutral analysis code; does not call live services.
ana_required_daily_columns <- c("station_code", "date", "variable", "value")
ana_validate_daily_series <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing_columns <- setdiff(ana_required_daily_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  out <- data
  out$station_code <- as.character(out$station_code)
  out$variable <- as.character(out$variable)
  if (!inherits(out$date, "Date")) {
    date_raw <- if (is.factor(out$date)) as.character(out$date) else out$date
    converted_date <- tryCatch(
      as.Date(date_raw),
      error = function(e) rep(as.Date(NA), length(date_raw))
    )
    invalid_date <- is.na(converted_date) & !is.na(date_raw)
    if (any(invalid_date)) {
      stop("`date` must be coercible to Date.", call. = FALSE)
    }
    out$date <- converted_date
  }
  if (!is.numeric(out$value)) {
    value_raw <- if (is.factor(out$value)) as.character(out$value) else out$value
    converted_value <- suppressWarnings(as.numeric(value_raw))
    invalid_value <- is.na(converted_value) & !is.na(value_raw)
    if (any(invalid_value)) {
      stop("`value` must be numeric or coercible to numeric.", call. = FALSE)
    }
    out$value <- converted_value
  }
  invalid_key <- is.na(out$station_code) | out$station_code == "" |
    is.na(out$variable) | out$variable == "" |
    is.na(out$date)
  if (any(invalid_key)) {
    stop(
      "`station_code`, `variable`, and `date` must not contain missing or empty values.",
      call. = FALSE
    )
  }
  out <- out[order(out$station_code, out$variable, out$date), , drop = FALSE]
  row.names(out) <- NULL
  out
}
ana_validate_start_month <- function(start_month) {
  if (length(start_month) != 1L || is.na(start_month)) {
    stop("`start_month` must be a single integer from 1 to 12.", call. = FALSE)
  }
  if (start_month != as.integer(start_month) || start_month < 1L || start_month > 12L) {
    stop("`start_month` must be a single integer from 1 to 12.", call. = FALSE)
  }
  as.integer(start_month)
}
ana_split_daily_groups <- function(data) {
  split(data, paste(data$station_code, data$variable, sep = "\r"), drop = TRUE)
}
ana_empty_daily_result <- function(analysis) {
  switch(
    analysis,
    availability = data.frame(
      station_code = character(),
      variable = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      n_records = integer(),
      expected_days = integer(),
      observed_days = integer(),
      valid_days = integer(),
      missing_dates = integer(),
      missing_value_days = integer(),
      missing_days = integer(),
      duplicate_records = integer(),
      availability_pct = numeric(),
      stringsAsFactors = FALSE
    ),
    missingness = data.frame(
      station_code = character(),
      variable = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      n_records = integer(),
      expected_days = integer(),
      observed_days = integer(),
      valid_days = integer(),
      n_missing_value_records = integer(),
      n_missing_dates = integer(),
      missing_value_days = integer(),
      total_missing_days = integer(),
      pct_missing_value_records = numeric(),
      pct_missing_days = numeric(),
      longest_missing_run_days = integer(),
      stringsAsFactors = FALSE
    ),
    summary = data.frame(
      station_code = character(),
      variable = character(),
      n_records = integer(),
      observed_days = integer(),
      valid_days = integer(),
      min = numeric(),
      q25 = numeric(),
      mean = numeric(),
      median = numeric(),
      q75 = numeric(),
      max = numeric(),
      sd = numeric(),
      stringsAsFactors = FALSE
    ),
    monthly = data.frame(
      station_code = character(),
      variable = character(),
      year = integer(),
      month = integer(),
      period_start = as.Date(character()),
      period_end = as.Date(character()),
      n_records = integer(),
      expected_days = integer(),
      observed_days = integer(),
      valid_days = integer(),
      missing_days = integer(),
      min = numeric(),
      mean = numeric(),
      median = numeric(),
      max = numeric(),
      sd = numeric(),
      sum = numeric(),
      stringsAsFactors = FALSE
    ),
    annual = data.frame(
      station_code = character(),
      variable = character(),
      year = integer(),
      year_type = character(),
      start_month = integer(),
      period_start = as.Date(character()),
      period_end = as.Date(character()),
      n_records = integer(),
      expected_days = integer(),
      observed_days = integer(),
      valid_days = integer(),
      missing_days = integer(),
      min = numeric(),
      mean = numeric(),
      median = numeric(),
      max = numeric(),
      sd = numeric(),
      sum = numeric(),
      stringsAsFactors = FALSE
    )
  )
}
hydro_daily_bind_rows <- function(rows, analysis) {
  if (length(rows) == 0L) {
    return(ana_empty_daily_result(analysis))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
ana_date_year <- function(date) {
  as.integer(format(date, "%Y"))
}
ana_date_month <- function(date) {
  as.integer(format(date, "%m"))
}
ana_add_months <- function(date, n) {
  year <- ana_date_year(date)
  month <- ana_date_month(date) + n
  year <- year + (month - 1L) %/% 12L
  month <- ((month - 1L) %% 12L) + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
}
ana_safe_min <- function(x) {
  if (length(x) == 0L) NA_real_ else min(x)
}
ana_safe_mean <- function(x) {
  if (length(x) == 0L) NA_real_ else mean(x)
}
ana_safe_median <- function(x) {
  if (length(x) == 0L) NA_real_ else stats::median(x)
}
ana_safe_max <- function(x) {
  if (length(x) == 0L) NA_real_ else max(x)
}
ana_safe_sd <- function(x) {
  if (length(x) <= 1L) NA_real_ else stats::sd(x)
}
ana_safe_sum <- function(x) {
  if (length(x) == 0L) NA_real_ else sum(x)
}
ana_safe_quantile <- function(x, probs) {
  if (length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, type = 7))
}
ana_daily_group_keys <- function(x) {
  list(
    station_code = as.character(x$station_code[1L]),
    variable = as.character(x$variable[1L])
  )
}
ana_valid_dates <- function(x) {
  unique(x$date[!is.na(x$value)])
}
ana_observed_dates <- function(x) {
  unique(x$date)
}
ana_missing_value_days <- function(x) {
  has_valid_by_date <- tapply(!is.na(x$value), x$date, any)
  sum(!has_valid_by_date)
}
ana_longest_true_run <- function(x) {
  if (length(x) == 0L || !any(x)) {
    return(0L)
  }
  runs <- rle(x)
  as.integer(max(runs$lengths[runs$values]))
}
ana_period_stats <- function(x, period_start, period_end) {
  if (period_end < period_start) {
    expected_days <- 0L
    y <- x[FALSE, , drop = FALSE]
  } else {
    expected_days <- as.integer(period_end - period_start) + 1L
    y <- x[x$date >= period_start & x$date <= period_end, , drop = FALSE]
  }
  observed_days <- length(ana_observed_dates(y))
  valid_days <- length(ana_valid_dates(y))
  values <- y$value[!is.na(y$value)]
  data.frame(
    n_records = nrow(y),
    expected_days = expected_days,
    observed_days = observed_days,
    valid_days = valid_days,
    missing_days = expected_days - valid_days,
    min = ana_safe_min(values),
    mean = ana_safe_mean(values),
    median = ana_safe_median(values),
    max = ana_safe_max(values),
    sd = ana_safe_sd(values),
    sum = ana_safe_sum(values),
    stringsAsFactors = FALSE
  )
}
ana_daily_availability <- function(data) {
  groups <- ana_split_daily_groups(data)
  rows <- lapply(groups, function(x) {
    keys <- ana_daily_group_keys(x)
    start_date <- min(x$date)
    end_date <- max(x$date)
    expected_days <- as.integer(end_date - start_date) + 1L
    observed_days <- length(ana_observed_dates(x))
    valid_days <- length(ana_valid_dates(x))
    missing_dates <- expected_days - observed_days
    missing_value_days <- ana_missing_value_days(x)
    missing_days <- expected_days - valid_days
    data.frame(
      station_code = keys$station_code,
      variable = keys$variable,
      start_date = start_date,
      end_date = end_date,
      n_records = nrow(x),
      expected_days = expected_days,
      observed_days = observed_days,
      valid_days = valid_days,
      missing_dates = missing_dates,
      missing_value_days = missing_value_days,
      missing_days = missing_days,
      duplicate_records = nrow(x) - observed_days,
      availability_pct = 100 * valid_days / expected_days,
      stringsAsFactors = FALSE
    )
  })
  hydro_daily_bind_rows(rows, "availability")
}
ana_daily_missingness <- function(data) {
  groups <- ana_split_daily_groups(data)
  rows <- lapply(groups, function(x) {
    keys <- ana_daily_group_keys(x)
    start_date <- min(x$date)
    end_date <- max(x$date)
    expected_days <- as.integer(end_date - start_date) + 1L
    observed_days <- length(ana_observed_dates(x))
    valid_dates <- ana_valid_dates(x)
    valid_days <- length(valid_dates)
    missing_value_days <- ana_missing_value_days(x)
    missing_dates <- expected_days - observed_days
    total_missing_days <- expected_days - valid_days
    full_dates <- seq(start_date, end_date, by = "day")
    missing_flags <- !(full_dates %in% valid_dates)
    data.frame(
      station_code = keys$station_code,
      variable = keys$variable,
      start_date = start_date,
      end_date = end_date,
      n_records = nrow(x),
      expected_days = expected_days,
      observed_days = observed_days,
      valid_days = valid_days,
      n_missing_value_records = sum(is.na(x$value)),
      n_missing_dates = missing_dates,
      missing_value_days = missing_value_days,
      total_missing_days = total_missing_days,
      pct_missing_value_records = if (nrow(x) == 0L) NA_real_ else 100 * sum(is.na(x$value)) / nrow(x),
      pct_missing_days = 100 * total_missing_days / expected_days,
      longest_missing_run_days = ana_longest_true_run(missing_flags),
      stringsAsFactors = FALSE
    )
  })
  hydro_daily_bind_rows(rows, "missingness")
}
ana_daily_summary <- function(data) {
  groups <- ana_split_daily_groups(data)
  rows <- lapply(groups, function(x) {
    keys <- ana_daily_group_keys(x)
    values <- x$value[!is.na(x$value)]
    data.frame(
      station_code = keys$station_code,
      variable = keys$variable,
      n_records = nrow(x),
      observed_days = length(ana_observed_dates(x)),
      valid_days = length(ana_valid_dates(x)),
      min = ana_safe_min(values),
      q25 = ana_safe_quantile(values, 0.25),
      mean = ana_safe_mean(values),
      median = ana_safe_median(values),
      q75 = ana_safe_quantile(values, 0.75),
      max = ana_safe_max(values),
      sd = ana_safe_sd(values),
      stringsAsFactors = FALSE
    )
  })
  hydro_daily_bind_rows(rows, "summary")
}
ana_monthly_summary <- function(data) {
  groups <- ana_split_daily_groups(data)
  rows <- lapply(groups, function(x) {
    keys <- ana_daily_group_keys(x)
    group_start <- min(x$date)
    group_end <- max(x$date)
    first_month <- as.Date(format(group_start, "%Y-%m-01"))
    last_month <- as.Date(format(group_end, "%Y-%m-01"))
    month_starts <- seq(first_month, last_month, by = "month")
    group_rows <- lapply(month_starts, function(period_start_full) {
      period_end_full <- ana_add_months(period_start_full, 1L) - 1L
      period_start <- max(period_start_full, group_start)
      period_end <- min(period_end_full, group_end)
      stats <- ana_period_stats(x, period_start, period_end)
      data.frame(
        station_code = keys$station_code,
        variable = keys$variable,
        year = ana_date_year(period_start_full),
        month = ana_date_month(period_start_full),
        period_start = period_start,
        period_end = period_end,
        stats,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, group_rows)
  })
  hydro_daily_bind_rows(rows, "monthly")
}
ana_floor_annual_start <- function(date, start_month) {
  year <- ana_date_year(date)
  month <- ana_date_month(date)
  start_year <- if (month < start_month) year - 1L else year
  as.Date(sprintf("%04d-%02d-01", start_year, start_month))
}
ana_annual_summary <- function(data, year_type, start_month) {
  if (year_type == "calendar") {
    start_month <- 1L
  }
  groups <- ana_split_daily_groups(data)
  rows <- lapply(groups, function(x) {
    keys <- ana_daily_group_keys(x)
    group_start <- min(x$date)
    group_end <- max(x$date)
    first_year_start <- ana_floor_annual_start(group_start, start_month)
    last_year_start <- ana_floor_annual_start(group_end, start_month)
    year_starts <- seq(first_year_start, last_year_start, by = "year")
    group_rows <- lapply(year_starts, function(period_start_full) {
      period_end_full <- ana_add_months(period_start_full, 12L) - 1L
      period_start <- max(period_start_full, group_start)
      period_end <- min(period_end_full, group_end)
      stats <- ana_period_stats(x, period_start, period_end)
      label_year <- ana_date_year(period_start_full)
      if (year_type == "hydrological" && start_month != 1L) {
        label_year <- label_year + 1L
      }
      data.frame(
        station_code = keys$station_code,
        variable = keys$variable,
        year = label_year,
        year_type = year_type,
        start_month = start_month,
        period_start = period_start,
        period_end = period_end,
        stats,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, group_rows)
  })
  hydro_daily_bind_rows(rows, "annual")
}
#' Analisar series diarias padronizadas
#'
#' Calcula resumos basicos para series diarias no contrato padronizado do
#' pacote. A funcao trabalha por combinacao de estacao e variavel, usando as
#' colunas `station_code`, `date`, `variable` e `value`.
#'
#' @details
#' Esta funcao oferece resumos diarios gerais. Para o fluxo recomendado do
#' usuario, use [analyze_hydro_data()], que tambem aceita objetos agregados do
#' pacote e despacha analises hidrologicas especificas.
#'
#' @param data Data frame com serie diaria padronizada.
#' @param analysis Tipo de analise. Use `"summary"`, `"availability"`,
#'   `"missingness"`, `"monthly"` ou `"annual"`.
#' @param year_type Tipo de ano para `analysis = "annual"`. Use `"calendar"`
#'   para ano civil ou `"hydrological"` para ano hidrologico.
#' @param start_month Mes inicial do ano hidrologico. O padrao e 10, para anos
#'   de outubro a setembro.
#'
#' @return Um `data.frame` com o resumo solicitado.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = c("001", "001", "001"),
#'   date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-04")),
#'   variable = "discharge",
#'   value = c(10, NA, 12),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' analyze_daily_series(daily, analysis = "availability")
#' @noRd
analyze_daily_series <- function(data,
                                 analysis = c(
                                   "summary",
                                   "availability",
                                   "missingness",
                                   "monthly",
                                   "annual"
                                 ),
                                 year_type = c("calendar", "hydrological"),
                                 start_month = 10) {
  analysis <- match.arg(analysis)
  year_type <- match.arg(year_type)
  start_month <- ana_validate_start_month(start_month)
  daily <- ana_validate_daily_series(data)
  if (nrow(daily) == 0L) {
    return(ana_empty_daily_result(analysis))
  }
  switch(
    analysis,
    availability = ana_daily_availability(daily),
    missingness = ana_daily_missingness(daily),
    summary = ana_daily_summary(daily),
    monthly = ana_monthly_summary(daily),
    annual = ana_annual_summary(daily, year_type = year_type, start_month = start_month)
  )
}
