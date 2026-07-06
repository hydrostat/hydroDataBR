# Table helpers for hydroDataBR.
# Functions in this file return data frames and do not create widgets.
hydrodatabr_period_frame <- function(dates, period) {
  dates <- as.Date(dates)
  out <- data.frame(
    date = dates,
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    stringsAsFactors = FALSE
  )
  out$month_label <- hydrodatabr_month_labels[out$month]
  if (identical(period, "annual")) {
    out$period <- sprintf("%04d", out$year)
  } else {
    out$period <- sprintf("%04d-%02d", out$year, out$month)
  }
  out
}
hydrodatabr_count_dates_by_period <- function(dates, period, count_name) {
  dates <- unique(as.Date(dates))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(data.frame())
  }
  frame <- hydrodatabr_period_frame(dates, period)
  frame$count <- 1L
  group_cols <- if (identical(period, "annual")) {
    c("year", "period")
  } else {
    c("year", "month", "month_label", "period")
  }
  formula <- stats::as.formula(paste("count ~", paste(group_cols, collapse = " + ")))
  out <- stats::aggregate(formula, data = frame, FUN = sum)
  names(out)[names(out) == "count"] <- count_name
  out[order(out$period), , drop = FALSE]
}
hydrodatabr_merge_period_counts <- function(expected, observed, valued, period) {
  by_cols <- if (identical(period, "annual")) {
    c("year", "period")
  } else {
    c("year", "month", "month_label", "period")
  }
  out <- expected
  out <- merge(out, observed, by = by_cols, all.x = TRUE, sort = FALSE)
  out <- merge(out, valued, by = by_cols, all.x = TRUE, sort = FALSE)
  if (!"days_observed" %in% names(out)) out$days_observed <- NA_integer_
  if (!"days_with_value" %in% names(out)) out$days_with_value <- NA_integer_
  out$days_observed[is.na(out$days_observed)] <- 0L
  out$days_with_value[is.na(out$days_with_value)] <- 0L
  out$days_missing <- out$days_expected - out$days_with_value
  out$availability_pct <- ifelse(out$days_expected > 0, 100 * out$days_with_value / out$days_expected, NA_real_)
  out$missing_pct <- ifelse(out$days_expected > 0, 100 * out$days_missing / out$days_expected, NA_real_)
  out[order(out$period), , drop = FALSE]
}
hydrodatabr_daily_split_key <- function(data) {
  paste(data$station_code, data$variable, sep = "\r")
}
hydrodatabr_restore_split_key <- function(key) {
  parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
  list(station_code = parts[[1]], variable = parts[[2]])
}
#' Tabela de disponibilidade diaria
#'
#' Resume a disponibilidade de uma serie diaria padronizada por ano ou por mes, contando dias esperados, dias observados e dias com valor numerico.
#'
#' @param data Tabela no contrato diario padronizado.
#' @param period Periodo de agregacao: `"annual"` ou `"monthly"`.
#' @param variable Variavel a filtrar. Use `NULL` para manter todas.
#' @param station_code Codigo da estacao a filtrar. Use `NULL` para manter todas.
#' @param start_date Data inicial opcional.
#' @param end_date Data final opcional.
#'
#' @return Um `data.frame`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:40,
#'   variable = "discharge",
#'   value = c(1:20, rep(NA, 21)),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' table_daily_availability(daily, period = "monthly")
#' @noRd
table_daily_availability <- function(data, period = c("annual", "monthly"),
                                     variable = NULL, station_code = NULL,
                                     start_date = NULL, end_date = NULL) {
  period <- match.arg(period)
  daily <- hydrodatabr_prepare_daily(
    data = data,
    variable = variable,
    station_code = station_code,
    start_date = start_date,
    end_date = end_date
  )
  if (nrow(daily) == 0) {
    return(data.frame())
  }
  groups <- split(daily, hydrodatabr_daily_split_key(daily), drop = TRUE)
  pieces <- vector("list", length(groups))
  names_groups <- names(groups)
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    key <- hydrodatabr_restore_split_key(names_groups[[i]])
    date_seq <- seq(min(g$date, na.rm = TRUE), max(g$date, na.rm = TRUE), by = "day")
    expected <- hydrodatabr_count_dates_by_period(date_seq, period, "days_expected")
    observed <- hydrodatabr_count_dates_by_period(g$date, period, "days_observed")
    valued <- hydrodatabr_count_dates_by_period(g$date[is.finite(g$value)], period, "days_with_value")
    out <- hydrodatabr_merge_period_counts(expected, observed, valued, period)
    out$station_code <- key$station_code
    out$variable <- key$variable
    out <- out[, c("station_code", "variable", setdiff(names(out), c("station_code", "variable"))), drop = FALSE]
    pieces[[i]] <- out
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$station_code, out$variable, out$period), , drop = FALSE]
}
hydrodatabr_summarise_values <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  valid <- x[is.finite(x)]
  data.frame(
    days_with_value = length(valid),
    mean_value = if (length(valid) > 0) mean(valid) else NA_real_,
    min_value = if (length(valid) > 0) min(valid) else NA_real_,
    max_value = if (length(valid) > 0) max(valid) else NA_real_,
    total_value = if (length(valid) > 0) sum(valid) else NA_real_,
    stringsAsFactors = FALSE
  )
}
hydrodatabr_stats_by_period <- function(g, period) {
  period_frame <- hydrodatabr_period_frame(g$date, period)
  g$year <- period_frame$year
  g$month <- period_frame$month
  g$month_label <- period_frame$month_label
  g$period <- period_frame$period
  group_cols <- if (identical(period, "annual")) {
    c("year", "period")
  } else {
    c("year", "month", "month_label", "period")
  }
  split_key <- do.call(paste, c(g[group_cols], sep = "\r"))
  groups <- split(g, split_key, drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    s <- groups[[i]]
    stats <- hydrodatabr_summarise_values(s$value)
    first_row <- s[1, group_cols, drop = FALSE]
    pieces[[i]] <- cbind(first_row, stats)
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$period), , drop = FALSE]
}
hydrodatabr_monthly_regime <- function(monthly) {
  if (nrow(monthly) == 0) {
    return(data.frame())
  }
  monthly <- monthly[is.finite(monthly$mean_value), , drop = FALSE]
  if (nrow(monthly) == 0) {
    return(data.frame())
  }
  split_key <- paste(monthly$station_code, monthly$variable, monthly$month, sep = "\r")
  groups <- split(monthly, split_key, drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    mean_values <- g$mean_value[is.finite(g$mean_value)]
    total_values <- g$total_value[is.finite(g$total_value)]
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = if ("unit" %in% names(g)) g$unit[[1]] else NA_character_,
      month = g$month[[1]],
      month_label = g$month_label[[1]],
      years_with_data = length(unique(g$year[is.finite(g$mean_value)])),
      mean_value = if (length(mean_values) > 0) mean(mean_values) else NA_real_,
      min_value = if (length(mean_values) > 0) min(mean_values) else NA_real_,
      max_value = if (length(mean_values) > 0) max(mean_values) else NA_real_,
      mean_total_value = if (length(total_values) > 0) mean(total_values) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$station_code, out$variable, out$month), , drop = FALSE]
}
#' Tabela de estatisticas diarias agregadas
#'
#' Calcula estatisticas anuais, mensais ou de regime mensal a partir do contrato diario padronizado do `hydroDataBR`.
#'
#' @param data Tabela no contrato diario padronizado.
#' @param period Periodo: `"annual"`, `"monthly"` ou `"monthly_regime"`.
#' @param variable Variavel a filtrar. Use `NULL` para manter todas.
#' @param station_code Codigo da estacao a filtrar. Use `NULL` para manter todas.
#' @param start_date Data inicial opcional.
#' @param end_date Data final opcional.
#'
#' @return Um `data.frame`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:40,
#'   variable = "discharge",
#'   value = c(1:20, rep(NA, 21)),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' table_daily_statistics(daily, period = "monthly_regime")
#' @noRd
table_daily_statistics <- function(data, period = c("annual", "monthly", "monthly_regime"),
                                   variable = NULL, station_code = NULL,
                                   start_date = NULL, end_date = NULL) {
  period <- match.arg(period)
  if (identical(period, "monthly_regime")) {
    monthly <- table_daily_statistics(
      data = data,
      period = "monthly",
      variable = variable,
      station_code = station_code,
      start_date = start_date,
      end_date = end_date
    )
    return(hydrodatabr_monthly_regime(monthly))
  }
  daily <- hydrodatabr_prepare_daily(
    data = data,
    variable = variable,
    station_code = station_code,
    start_date = start_date,
    end_date = end_date
  )
  if (nrow(daily) == 0) {
    return(data.frame())
  }
  availability <- table_daily_availability(
    daily,
    period = period,
    variable = NULL,
    station_code = NULL
  )
  groups <- split(daily, hydrodatabr_daily_split_key(daily), drop = TRUE)
  pieces <- vector("list", length(groups))
  names_groups <- names(groups)
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    key <- hydrodatabr_restore_split_key(names_groups[[i]])
    stats <- hydrodatabr_stats_by_period(g, period)
    stats$station_code <- key$station_code
    stats$variable <- key$variable
    stats$unit <- if (any(!is.na(g$unit) & nzchar(g$unit))) unique(g$unit[!is.na(g$unit) & nzchar(g$unit)])[[1]] else NA_character_
    stats <- stats[, c("station_code", "variable", "unit", setdiff(names(stats), c("station_code", "variable", "unit"))), drop = FALSE]
    pieces[[i]] <- stats
  }
  stats <- do.call(rbind, pieces)
  rownames(stats) <- NULL
  by_cols <- if (identical(period, "annual")) {
    c("station_code", "variable", "year", "period")
  } else {
    c("station_code", "variable", "year", "month", "month_label", "period")
  }
  keep_availability <- c(by_cols, "days_expected", "days_observed", "days_missing", "availability_pct", "missing_pct")
  out <- merge(stats, availability[, keep_availability, drop = FALSE], by = by_cols, all.x = TRUE, sort = FALSE)
  out <- out[order(out$station_code, out$variable, out$period), , drop = FALSE]
  rownames(out) <- NULL
  preferred <- c(
    "station_code", "variable", "unit", "year", "month", "month_label", "period",
    "days_expected", "days_observed", "days_with_value", "days_missing",
    "availability_pct", "missing_pct", "mean_value", "min_value", "max_value", "total_value"
  )
  out[, c(intersect(preferred, names(out)), setdiff(names(out), preferred)), drop = FALSE]
}
hydrodatabr_station_product_available <- function(stations, flag_candidates, date_candidates = character()) {
  flag_col <- hydrodatabr_first_existing_name(stations, flag_candidates)
  if (!is.na(flag_col)) {
    return(as.logical(stations[[flag_col]]))
  }
  if (length(date_candidates) > 0) {
    date_col <- hydrodatabr_first_existing_name(stations, date_candidates)
    if (!is.na(date_col)) {
      return(!is.na(as.Date(stations[[date_col]])))
    }
  }
  rep(FALSE, nrow(stations))
}
hydrodatabr_get_ana_stations_data <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("ana_stations", package = "hydroDataBR", envir = env)
  if (!exists("ana_stations", envir = env, inherits = FALSE)) {
    stop("Built-in dataset `ana_stations` was not found.", call. = FALSE)
  }
  get("ana_stations", envir = env, inherits = FALSE)
}
#' Tabela de produtos por estacao
#'
#' Monta uma tabela longa de disponibilidade de produtos por estacao usando
#' `ana_stations` ou uma tabela de estacoes fornecida pelo usuario.
#'
#' @param stations Tabela de estacoes. Se `NULL`, usa `ana_stations`.
#' @param station_code Codigo(s) de estacao para filtrar.
#'
#' @return Um `data.frame` com uma linha por estacao e produto.
#'
#' @examples
#' head(table_station_products(station_code = "10100000"))
#' @noRd
table_station_products <- function(stations = NULL, station_code = NULL) {
  if (is.null(stations)) {
    stations <- hydrodatabr_get_ana_stations_data()
  }
  stations <- hydrodatabr_as_data_frame(stations)
  if (nrow(stations) == 0) {
    return(data.frame())
  }
  if (!"station_code" %in% names(stations)) {
    stop("`stations` must contain a `station_code` column.", call. = FALSE)
  }
  stations$station_code <- as.character(stations$station_code)
  if (!is.null(station_code)) {
    stations <- stations[stations$station_code %in% as.character(station_code), , drop = FALSE]
  }
  if (nrow(stations) == 0) {
    return(data.frame())
  }
  product_specs <- list(
    daily_discharge = list(
      label = "Vazao diaria",
      flags = c("has_discharge_data", "has_inventory_flu_data"),
      start = "discharge_start_date",
      end = "discharge_end_date"
    ),
    daily_stage = list(
      label = "Cota diaria",
      flags = c("has_stage_data", "has_inventory_stage_data"),
      start = "stage_start_date",
      end = "stage_end_date"
    ),
    daily_rainfall = list(
      label = "Chuva diaria",
      flags = c("has_rainfall_data", "has_inventory_rainfall_data"),
      start = "rainfall_start_date",
      end = "rainfall_end_date"
    ),
    telemetry = list(
      label = "Telemetria",
      flags = c("has_telemetry"),
      start = "telemetric_start_date",
      end = "telemetric_end_date"
    ),
    discharge_measurements = list(
      label = "Medicoes de descarga",
      flags = c("has_discharge_measurements"),
      start = NA_character_,
      end = NA_character_
    )
  )
  pieces <- vector("list", length(product_specs))
  product_names <- names(product_specs)
  for (i in seq_along(product_specs)) {
    spec <- product_specs[[i]]
    start_date <- if (!is.na(spec$start) && spec$start %in% names(stations)) as.Date(stations[[spec$start]]) else as.Date(NA)
    end_date <- if (!is.na(spec$end) && spec$end %in% names(stations)) as.Date(stations[[spec$end]]) else as.Date(NA)
    available <- hydrodatabr_station_product_available(stations, spec$flags, spec$start)
    pieces[[i]] <- data.frame(
      station_code = stations$station_code,
      station_name = if ("station_name" %in% names(stations)) as.character(stations$station_name) else NA_character_,
      product = product_names[[i]],
      product_label = spec$label,
      available = available,
      start_date = start_date,
      end_date = end_date,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out[order(out$station_code, out$product), , drop = FALSE]
}
#' Tabela de relatorio de requisicao
#'
#' Normaliza relatorios de requisicao produzidos por funcoes de aquisicao do
#' `hydroDataBR`, incluindo objetos que contem o elemento `request_report`.
#'
#' @details
#' A tabela preserva estados operacionais como `success`, `empty`, `skipped` e
#' `error`, quando presentes. Ela nao deve conter credenciais, tokens ou
#' cabecalhos sensiveis.
#'
#' @param report Um `data.frame`, ou uma lista com elemento `request_report`.
#'
#' @return Um `data.frame`.
#'
#' @examples
#' report <- data.frame(
#'   station_code = "001",
#'   product = "daily_discharge",
#'   status = "success",
#'   n_rows = 10
#' )
#' table_request_report(report)
#' @noRd
table_request_report <- function(report) {
  if (is.list(report) && !is.data.frame(report) && "request_report" %in% names(report)) {
    report <- report$request_report
  }
  report <- hydrodatabr_as_data_frame(report)
  if (nrow(report) == 0) {
    return(data.frame())
  }
  if ("status" %in% names(report)) {
    report$status <- as.character(report$status)
    if (!"success" %in% names(report)) {
      report$success <- report$status %in% c("success", "ok")
    }
  }
  numeric_cols <- intersect(
    c("n_records", "n_rows", "n_sections", "n_vertices", "elapsed_seconds", "batch_index"),
    names(report)
  )
  for (col in numeric_cols) {
    report[[col]] <- suppressWarnings(as.numeric(report[[col]]))
  }
  date_cols <- intersect(c("date_start", "date_end", "start_date", "end_date"), names(report))
  for (col in date_cols) {
    report[[col]] <- as.Date(report[[col]])
  }
  preferred <- c(
    "batch_index", "product", "source", "station_code", "date_start", "date_end",
    "status", "success", "n_records", "n_rows", "n_sections", "n_vertices",
    "message", "error_message", "elapsed_seconds", "window_basis"
  )
  report <- report[, c(intersect(preferred, names(report)), setdiff(names(report), preferred)), drop = FALSE]
  rownames(report) <- NULL
  report
}
# Identify hydrometry diagnostic flag columns across station and daily outputs.
hydrodatabr_hydrometry_flag_columns <- function(data) {
  if (!is.data.frame(data) || !length(names(data))) return(character())
  pattern_cols <- names(data)[grepl("_flag$|^flag_", names(data))]
  daily_flag_cols <- c(
    "mathematical_error",
    "missing_discharge",
    "missing_stage",
    "discharge_without_stage",
    "stage_without_discharge",
    "both_missing",
    "non_positive_discharge",
    "non_positive_stage",
    "has_rating_curve_date_coverage",
    "has_applicable_rating_segment",
    "multiple_applicable_rating_segments",
    "has_generated_discharge",
    "no_rating_curve_for_date",
    "no_applicable_rating_segment",
    "stage_below_curve_range",
    "stage_above_curve_range",
    "stage_outside_curve_range",
    "relative_error_exceeds_threshold",
    "diagnostic_problem",
    "duplicate_discharge_records",
    "duplicate_stage_records",
    "has_discharge_record",
    "has_stage_record",
    "has_valid_discharge",
    "has_valid_stage"
  )
  candidates <- unique(c(pattern_cols, intersect(daily_flag_cols, names(data))))
  candidates[vapply(candidates, function(col) {
    x <- data[[col]]
    if (is.logical(x)) return(TRUE)
    if (is.numeric(x) || is.integer(x)) return(all(is.na(x) | x %in% c(0, 1)))
    if (is.character(x)) {
      values <- tolower(trimws(x))
      return(all(is.na(values) | values %in% c("", "true", "false", "t", "f", "1", "0", "yes", "no", "sim", "nao", "nao")))
    }
    FALSE
  }, logical(1))]
}
# Convert supported diagnostic flag vectors to logical values.
hydrodatabr_as_logical_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x) || is.integer(x)) return(ifelse(is.na(x), NA, x != 0))
  values <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(values))
  out[values %in% c("true", "t", "1", "yes", "sim")] <- TRUE
  out[values %in% c("false", "f", "0", "no", "nao", "nao", "")] <- FALSE
  out
}
#' Tabela de diagnosticos diarios de hidrometria
#'
#' Resume colunas logicas de sinalizacao de diagnosticos. Se a entrada for uma
#' lista, tenta usar elementos comuns de resultados de diagnostico.
#'
#' @details
#' A funcao reconhece tanto colunas de flags por padrao de nome quanto colunas
#' diagnosticas nomeadas, como `diagnostic_problem`, `missing_discharge`,
#' `missing_stage`, `no_rating_curve_for_date` e `stage_outside_curve_range`.
#'
#' @param diagnostics Tabela ou lista de diagnosticos, incluindo a lista
#'   retornada por [diagnose_daily_hydrometry()].
#'
#' @return Um `data.frame` com contagem de ocorrencias por tipo de problema.
#'
#' @examples
#' diagnostics <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:2,
#'   missing_stage = c(TRUE, FALSE, FALSE),
#'   no_rating_curve_for_date = c(FALSE, TRUE, FALSE)
#' )
#' table_daily_hydrometry_diagnostics(diagnostics)
#' @noRd
table_daily_hydrometry_diagnostics <- function(diagnostics) {
  if (is.list(diagnostics) && !is.data.frame(diagnostics)) {
    if ("daily_flags" %in% names(diagnostics)) {
      diagnostics <- diagnostics$daily_flags
    } else if ("daily_diagnostics" %in% names(diagnostics)) {
      diagnostics <- diagnostics$daily_diagnostics
    } else if ("diagnostics" %in% names(diagnostics)) {
      diagnostics <- diagnostics$diagnostics
    } else if ("measurement_flags" %in% names(diagnostics)) {
      diagnostics <- diagnostics$measurement_flags
    }
  }
  data <- hydrodatabr_as_data_frame(diagnostics)
  if (nrow(data) == 0) {
    return(data.frame())
  }
  flag_cols <- hydrodatabr_hydrometry_flag_columns(data)
  if (length(flag_cols) == 0 && all(c("issue", "n_flagged") %in% names(data))) {
    return(data)
  }
  if (length(flag_cols) == 0) {
    return(data.frame())
  }
  out <- data.frame(
    issue = flag_cols,
    n_records = nrow(data),
    n_flagged = vapply(flag_cols, function(col) sum(hydrodatabr_as_logical_flag(data[[col]]), na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE
  )
  out$flagged_pct <- ifelse(out$n_records > 0, 100 * out$n_flagged / out$n_records, NA_real_)
  out <- out[order(-out$n_flagged, out$issue), , drop = FALSE]
  rownames(out) <- NULL
  out
}
