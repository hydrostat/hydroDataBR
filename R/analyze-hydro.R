# Source-neutral hydrological analyses.
# These functions do not call live services.
utils::globalVariables(c("station_code", "variable", "date", "value"))
hydrodatabr_valid_year_start_month <- function(year_start_month) {
  year_start_month <- as.integer(year_start_month[[1]])
  if (is.na(year_start_month) || year_start_month < 1L || year_start_month > 12L) {
    stop("`year_start_month` must be an integer from 1 to 12.", call. = FALSE)
  }
  year_start_month
}
hydrodatabr_analysis_year <- function(dates, year_start_month = 1L) {
  dates <- as.Date(dates)
  year_start_month <- hydrodatabr_valid_year_start_month(year_start_month)
  years <- as.integer(format(dates, "%Y"))
  months <- as.integer(format(dates, "%m"))
  if (year_start_month == 1L) {
    return(years)
  }
  years + ifelse(months >= year_start_month, 1L, 0L)
}
hydrodatabr_first_non_missing <- function(x, default = NA_character_) {
  if (length(x) == 0) {
    return(default)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(default)
  }
  as.character(x[[1]])
}
hydrodatabr_filter_daily_for_analysis <- function(data, variable = NULL, station_code = NULL) {
  daily <- hydrodatabr_extract_daily_data(data)
  daily <- hydrodatabr_prepare_daily(
    data = daily,
    variable = variable,
    station_code = station_code
  )
  if (nrow(daily) == 0) {
    stop("No standardized daily data found for the requested analysis.", call. = FALSE)
  }
  daily
}
hydrodatabr_split_by_station_variable <- function(data) {
  key <- paste(data$station_code, data$variable, sep = "\r")
  split(data, key, drop = TRUE)
}
hydrodatabr_split_by_station_variable_year <- function(data, year_start_month = 1L) {
  data$year <- hydrodatabr_analysis_year(data$date, year_start_month)
  key <- paste(data$station_code, data$variable, data$year, sep = "\r")
  split(data, key, drop = TRUE)
}
hydrodatabr_complete_daily_dates <- function(data) {
  data <- data[order(data$date), , drop = FALSE]
  if (nrow(data) == 0 || all(is.na(data$date))) {
    return(data)
  }
  dates <- seq(min(data$date, na.rm = TRUE), max(data$date, na.rm = TRUE), by = "day")
  template <- data.frame(date = dates, stringsAsFactors = FALSE)
  keep <- data[!duplicated(data$date), , drop = FALSE]
  out <- merge(template, keep, by = "date", all.x = TRUE, sort = TRUE)
  fill_cols <- intersect(c("station_code", "variable", "unit", "source"), names(out))
  for (col in fill_cols) {
    first_value <- hydrodatabr_first_non_missing(data[[col]])
    out[[col]][is.na(out[[col]])] <- first_value
  }
  out[order(out$date), , drop = FALSE]
}
hydrodatabr_max_run_length <- function(x) {
  x <- as.logical(x)
  x[is.na(x)] <- FALSE
  if (length(x) == 0 || !any(x)) {
    return(0L)
  }
  runs <- rle(x)
  as.integer(max(runs$lengths[runs$values], na.rm = TRUE))
}
hydrodatabr_rolling_sum <- function(x, width) {
  x <- suppressWarnings(as.numeric(x))
  width <- as.integer(width[[1]])
  if (is.na(width) || width < 1L) {
    stop("Rolling-window width must be a positive integer.", call. = FALSE)
  }
  n <- length(x)
  if (n < width) {
    return(numeric(0))
  }
  out <- rep(NA_real_, n - width + 1L)
  for (i in seq_len(n - width + 1L)) {
    window <- x[i:(i + width - 1L)]
    if (all(is.finite(window))) {
      out[[i]] <- sum(window)
    }
  }
  out
}
hydrodatabr_rolling_mean <- function(x, width) {
  sums <- hydrodatabr_rolling_sum(x, width)
  sums / as.integer(width[[1]])
}
hydrodatabr_flow_duration_analysis <- function(daily, exceedance_probabilities = 1:99) {
  exceedance_probabilities <- suppressWarnings(as.numeric(exceedance_probabilities))
  exceedance_probabilities <- sort(unique(exceedance_probabilities[is.finite(exceedance_probabilities)]))
  exceedance_probabilities <- exceedance_probabilities[
    exceedance_probabilities >= 1 & exceedance_probabilities <= 99
  ]
  if (length(exceedance_probabilities) == 0) {
    stop("`exceedance_probabilities` must contain values from 1 to 99.", call. = FALSE)
  }
  groups <- hydrodatabr_split_by_station_variable(daily)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    values <- suppressWarnings(as.numeric(g$value))
    values <- values[is.finite(values)]
    values <- sort(values, decreasing = TRUE)
    n <- length(values)
    if (n == 0) {
      pieces[[i]] <- data.frame()
      next
    }
    if (n == 1L) {
      interpolated <- rep(values[[1]], length(exceedance_probabilities))
    } else {
      rank <- seq_len(n)
      empirical_probability <- 100 * rank / (n + 1)
      interpolated <- stats::approx(
        x = empirical_probability,
        y = values,
        xout = exceedance_probabilities,
        method = "linear",
        rule = 2,
        ties = "ordered"
      )$y
    }
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = hydrodatabr_first_non_missing(g$unit),
      permanence_pct = exceedance_probabilities,
      exceedance_probability = exceedance_probabilities,
      non_exceedance_probability = 100 - exceedance_probabilities,
      value = interpolated,
      n_values = n,
      interpolation_method = "linear_empirical_fdc",
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  if (nrow(out) == 0) {
    return(data.frame())
  }
  out[order(out$station_code, out$variable, out$permanence_pct), , drop = FALSE]
}
hydrodatabr_flow_indices_analysis <- function(daily) {
  groups <- hydrodatabr_split_by_station_variable(daily)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    values_all <- suppressWarnings(as.numeric(g$value))
    values <- values_all[is.finite(values_all)]
    n_values <- length(values)
    q90 <- if (n_values > 0) as.numeric(stats::quantile(values, probs = 0.10, na.rm = TRUE, type = 8)) else NA_real_
    q95 <- if (n_values > 0) as.numeric(stats::quantile(values, probs = 0.05, na.rm = TRUE, type = 8)) else NA_real_
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = hydrodatabr_first_non_missing(g$unit),
      n_days = nrow(g),
      n_values = n_values,
      missing_days = sum(!is.finite(values_all)),
      qmlt = if (n_values > 0) mean(values) else NA_real_,
      q90 = q90,
      q95 = q95,
      min_value = if (n_values > 0) min(values) else NA_real_,
      max_value = if (n_values > 0) max(values) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  out[order(out$station_code, out$variable), , drop = FALSE]
}
hydrodatabr_rainfall_indices_one_year <- function(g, year_start_month, wet_day_threshold) {
  g <- hydrodatabr_complete_daily_dates(g)
  values <- suppressWarnings(as.numeric(g$value))
  valid <- values[is.finite(values)]
  wet <- is.finite(values) & values >= wet_day_threshold
  dry <- is.finite(values) & values < wet_day_threshold
  wet_values <- values[wet]
  rx5_values <- hydrodatabr_rolling_sum(values, 5L)
  data.frame(
    station_code = g$station_code[[1]],
    variable = g$variable[[1]],
    unit = hydrodatabr_first_non_missing(g$unit, "mm"),
    year = g$year[[1]],
    year_start_month = as.integer(year_start_month),
    n_days = nrow(g),
    n_values = length(valid),
    missing_days = sum(!is.finite(values)),
    total_rainfall = if (length(valid) > 0) sum(valid) else NA_real_,
    wet_days = sum(wet, na.rm = TRUE),
    dry_days = sum(dry, na.rm = TRUE),
    rx1day = if (length(valid) > 0) max(valid) else NA_real_,
    rx5day = if (length(rx5_values[is.finite(rx5_values)]) > 0) max(rx5_values, na.rm = TRUE) else NA_real_,
    r10 = sum(values >= 10, na.rm = TRUE),
    r20 = sum(values >= 20, na.rm = TRUE),
    r50 = sum(values >= 50, na.rm = TRUE),
    sdii = if (length(wet_values) > 0) sum(wet_values) / length(wet_values) else NA_real_,
    cdd = hydrodatabr_max_run_length(dry),
    cwd = hydrodatabr_max_run_length(wet),
    stringsAsFactors = FALSE
  )
}
hydrodatabr_rainfall_indices_analysis <- function(daily, year_start_month, wet_day_threshold) {
  daily$year <- hydrodatabr_analysis_year(daily$date, year_start_month)
  groups <- split(daily, paste(daily$station_code, daily$variable, daily$year, sep = "\r"), drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    pieces[[i]] <- hydrodatabr_rainfall_indices_one_year(groups[[i]], year_start_month, wet_day_threshold)
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  out[order(out$station_code, out$variable, out$year), , drop = FALSE]
}
hydrodatabr_analysis_year_bounds <- function(year, year_start_month) {
  year <- as.integer(year[[1]])
  year_start_month <- hydrodatabr_valid_year_start_month(year_start_month)
  if (year_start_month == 1L) {
    start_date <- as.Date(sprintf("%04d-01-01", year))
    next_start <- as.Date(sprintf("%04d-01-01", year + 1L))
  } else {
    start_date <- as.Date(sprintf("%04d-%02d-01", year - 1L, year_start_month))
    next_start <- as.Date(sprintf("%04d-%02d-01", year, year_start_month))
  }
  data.frame(
    year_start_date = start_date,
    year_end_date = next_start - 1L,
    stringsAsFactors = FALSE
  )
}
hydrodatabr_analysis_year_info <- function(g, year_start_month) {
  bounds <- hydrodatabr_analysis_year_bounds(g$year[[1]], year_start_month)
  expected_dates <- seq(bounds$year_start_date[[1]], bounds$year_end_date[[1]], by = "day")
  observed_dates <- unique(as.Date(g$date[!is.na(g$date)]))
  data.frame(
    year_start_date = bounds$year_start_date[[1]],
    year_end_date = bounds$year_end_date[[1]],
    expected_days = length(expected_dates),
    observed_days = sum(expected_dates %in% observed_dates),
    complete_year = all(expected_dates %in% observed_dates),
    stringsAsFactors = FALSE
  )
}
hydrodatabr_complete_group_to_analysis_year <- function(g, year_start_month) {
  info <- hydrodatabr_analysis_year_info(g, year_start_month)
  template <- data.frame(
    date = seq(info$year_start_date[[1]], info$year_end_date[[1]], by = "day"),
    stringsAsFactors = FALSE
  )
  keep <- g[!duplicated(g$date), , drop = FALSE]
  out <- merge(template, keep, by = "date", all.x = TRUE, sort = TRUE)
  fill_cols <- intersect(c("station_code", "variable", "unit", "source"), names(out))
  for (col in fill_cols) {
    first_value <- hydrodatabr_first_non_missing(g[[col]])
    out[[col]][is.na(out[[col]])] <- first_value
  }
  out$year <- as.integer(g$year[[1]])
  out[order(out$date), , drop = FALSE]
}
hydrodatabr_empty_annual_maxima <- function() {
  data.frame(
    station_code = character(), variable = character(), unit = character(),
    year = integer(), year_start_month = integer(),
    year_start_date = as.Date(character()), year_end_date = as.Date(character()),
    complete_year = logical(), expected_days = integer(), n_days = integer(),
    n_values = integer(), missing_days = integer(), max_value = numeric(),
    max_date = as.Date(character()), stringsAsFactors = FALSE
  )
}
hydrodatabr_empty_low_flows <- function() {
  data.frame(
    station_code = character(), variable = character(), unit = character(),
    year = integer(), year_start_month = integer(), duration_days = integer(),
    year_start_date = as.Date(character()), year_end_date = as.Date(character()),
    complete_year = logical(), expected_days = integer(), n_days = integer(),
    n_valid_windows = integer(), missing_days = integer(), min_value = numeric(),
    stringsAsFactors = FALSE
  )
}
hydrodatabr_annual_maxima_analysis <- function(daily, year_start_month, complete_years_only = TRUE) {
  groups <- hydrodatabr_split_by_station_variable_year(daily, year_start_month)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g_original <- groups[[i]]
    year_info <- hydrodatabr_analysis_year_info(g_original, year_start_month)
    if (isTRUE(complete_years_only) && !isTRUE(year_info$complete_year[[1]])) {
      pieces[[i]] <- data.frame()
      next
    }
    g <- hydrodatabr_complete_group_to_analysis_year(g_original, year_start_month)
    values <- suppressWarnings(as.numeric(g$value))
    finite <- is.finite(values)
    if (any(finite)) {
      max_value <- max(values[finite])
      max_index <- which(finite & values == max_value)[[1]]
      max_date <- g$date[[max_index]]
    } else {
      max_value <- NA_real_
      max_date <- as.Date(NA)
    }
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = hydrodatabr_first_non_missing(g$unit),
      year = g$year[[1]],
      year_start_month = as.integer(year_start_month),
      year_start_date = year_info$year_start_date[[1]],
      year_end_date = year_info$year_end_date[[1]],
      complete_year = year_info$complete_year[[1]],
      expected_days = year_info$expected_days[[1]],
      n_days = year_info$observed_days[[1]],
      n_values = sum(finite),
      missing_days = year_info$expected_days[[1]] - sum(finite),
      max_value = max_value,
      max_date = max_date,
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  if (nrow(out) == 0) {
    return(hydrodatabr_empty_annual_maxima())
  }
  out[order(out$station_code, out$variable, out$year), , drop = FALSE]
}
hydrodatabr_low_flow_analysis <- function(daily,
                                          year_start_month,
                                          durations,
                                          complete_years_only = TRUE) {
  durations <- unique(as.integer(durations))
  durations <- durations[!is.na(durations) & durations > 0L]
  if (length(durations) == 0) {
    stop("`durations` must contain at least one positive integer.", call. = FALSE)
  }
  daily$year <- hydrodatabr_analysis_year(daily$date, year_start_month)
  groups <- split(daily, paste(daily$station_code, daily$variable, daily$year, sep = "\r"), drop = TRUE)
  pieces <- list()
  k <- 0L
  for (i in seq_along(groups)) {
    g_original <- groups[[i]]
    year_info <- hydrodatabr_analysis_year_info(g_original, year_start_month)
    if (isTRUE(complete_years_only) && !isTRUE(year_info$complete_year[[1]])) {
      next
    }
    g <- hydrodatabr_complete_group_to_analysis_year(g_original, year_start_month)
    values <- suppressWarnings(as.numeric(g$value))
    for (duration in durations) {
      roll <- hydrodatabr_rolling_mean(values, duration)
      finite_roll <- roll[is.finite(roll)]
      k <- k + 1L
      pieces[[k]] <- data.frame(
        station_code = g$station_code[[1]],
        variable = g$variable[[1]],
        unit = hydrodatabr_first_non_missing(g$unit),
        year = g$year[[1]],
        year_start_month = as.integer(year_start_month),
        duration_days = duration,
        year_start_date = year_info$year_start_date[[1]],
        year_end_date = year_info$year_end_date[[1]],
        complete_year = year_info$complete_year[[1]],
        expected_days = year_info$expected_days[[1]],
        n_days = year_info$observed_days[[1]],
        n_valid_windows = length(finite_roll),
        missing_days = year_info$expected_days[[1]] - sum(is.finite(values)),
        min_value = if (length(finite_roll) > 0) min(finite_roll) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  if (nrow(out) == 0) {
    return(hydrodatabr_empty_low_flows())
  }
  out[order(out$station_code, out$variable, out$year, out$duration_days), , drop = FALSE]
}
#' Analisar dados hidrológicos padronizados
#'
#' Calcula estatísticas, indicadores e diagnósticos básicos a partir de séries
#' diárias padronizadas ou de objetos retornados por funções de aquisição do
#' pacote. A função não baixa dados e não acessa serviços externos; ela trabalha
#' apenas com objetos já carregados na sessão.
#'
#' @param data Objeto de dados. Pode ser uma série diária padronizada, um objeto
#'   retornado por `get_ana_data()`, ou um lote retornado por
#'   `get_ana_data_batch()`.
#' @param analysis Tipo de análise. Valores aceitos incluem
#'   `"daily_statistics"`, `"daily_availability"`, `"flow_duration"`,
#'   `"flow_indices"`, `"monthly_flow_indices"`, `"annual_maxima"`,
#'   `"low_flows"`, `"rainfall_indices"`,
#'   `"rainfall_annual_maxima"`, `"rainfall_diagnostics"`,
#'   `"daily_gap_summary"`, `"hydrometry_diagnostics"`,
#'   `"measurement_diagnostics"` e `"rating_diagnostics"`.
#' @param variable Variável diária a filtrar, como `"discharge"`, `"stage"`
#'   ou `"rainfall"`. Quando `NULL`, a função usa a variável adequada para a
#'   análise escolhida.
#' @param station_code Código(s) de estação a filtrar.
#' @param period Período usado em algumas análises agregadas.
#' @param year_start_month Mês inicial do ano de análise. Use 1 para ano civil
#'   e 10 para ano hidrológico de outubro a setembro.
#' @param durations Durações, em dias, usadas para mínimas móveis anuais.
#' @param wet_day_threshold Limiar, em mm, para definir dia chuvoso nos índices
#'   pluviométricos.
#' @param exceedance_probabilities Probabilidades de permanência, em porcentagem,
#'   usadas na curva de permanência. O padrão retorna 1 a 99%.
#' @param complete_years_only Se `TRUE`, remove anos incompletos em análises de
#'   máximos anuais e mínimas anuais.
#' @param ... Argumentos adicionais usados por análises específicas.
#'
#' @details
#' A função foi pensada para o fluxo típico de engenharia hidrológica: obter ou
#' ler os dados, padronizar a série diária e então calcular indicadores de
#' disponibilidade, regime, permanência, extremos e consistência. Para vazões,
#' as análises incluem curva de permanência, QMLT, Q90, Q95, índices mensais,
#' máximos anuais e mínimas móveis. Para chuva, incluem índices anuais, máximos
#' anuais e diagnósticos de falhas/consistência. Diagnósticos fluviométricos
#' podem usar a base hidrométrica interna do pacote quando as referências não
#' são fornecidas explicitamente.
#'
#' @return Em geral, um `data.frame`. Algumas análises técnicas podem retornar
#'   listas com tabelas auxiliares.
#' @export
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:29,
#'   variable = "discharge",
#'   value = seq(10, 39),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#'
#' analyze_hydro_data(daily, analysis = "flow_indices")
#' analyze_hydro_data(daily, analysis = "daily_availability")
#'
#' # A mesma interface tambem pode ser usada com objetos de aquisicao.
#' objeto <- list(daily_data = daily)
#' analyze_hydro_data(objeto, analysis = "flow_duration")
analyze_hydro_data <- function(data,
                               analysis = c(
                                 "daily_statistics", "daily_availability",
                                 "flow_duration", "flow_indices",
                                 "monthly_flow_indices", "rainfall_indices",
                                 "annual_maxima", "rainfall_annual_maxima",
                                 "low_flows", "rainfall_diagnostics",
                                 "daily_gap_summary", "hydrometry_diagnostics",
                                 "measurement_diagnostics", "rating_diagnostics"
                               ),
                               variable = NULL,
                               station_code = NULL,
                               period = "annual",
                               year_start_month = 1L,
                               durations = c(1L, 3L, 7L, 15L, 30L),
                               wet_day_threshold = 1,
                               exceedance_probabilities = 1:99,
                               complete_years_only = TRUE,
                               ...) {
  # Stage 09F daily consistency preference - analyze_hydro_data
  data <- hydro_prefer_daily_observations(data)
  analysis_choices <- c(
    "daily_statistics", "daily_availability", "flow_duration",
    "flow_indices", "monthly_flow_indices", "rainfall_indices",
    "annual_maxima", "rainfall_annual_maxima", "low_flows",
    "rainfall_diagnostics", "daily_gap_summary", "hydrometry_diagnostics",
    "measurement_diagnostics", "rating_diagnostics"
  )
  analysis_aliases <- c(
    availability = "daily_availability",
    statistics = "daily_statistics",
    stats = "daily_statistics",
    fdc = "flow_duration",
    permanence = "flow_duration",
    discharge_indices = "flow_indices",
    q_indices = "flow_indices",
    monthly_q = "monthly_flow_indices",
    monthly_flow = "monthly_flow_indices",
    rain_indices = "rainfall_indices",
    precipitation_indices = "rainfall_indices",
    maxima = "annual_maxima",
    annual_max = "annual_maxima",
    rainfall_maxima = "rainfall_annual_maxima",
    rainfall_annual_max = "rainfall_annual_maxima",
    low_flow = "low_flows",
    minimum_flows = "low_flows",
    rain_diagnostics = "rainfall_diagnostics",
    precipitation_diagnostics = "rainfall_diagnostics",
    gaps = "daily_gap_summary",
    gap_summary = "daily_gap_summary",
    hydrometry = "hydrometry_diagnostics",
    daily_hydrometry_diagnostics = "hydrometry_diagnostics",
    daily_hydrometry = "hydrometry_diagnostics",
    measurement_flags = "measurement_diagnostics",
    discharge_measurement_diagnostics = "measurement_diagnostics",
    rating = "rating_diagnostics",
    rating_curve_diagnostics = "rating_diagnostics"
  )
  analysis <- hydrodatabr_normalize_choice(analysis[[1]], analysis_choices, analysis_aliases, arg = "analysis")
  year_start_month <- hydrodatabr_valid_year_start_month(year_start_month)
  if (identical(analysis, "daily_statistics")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(table_daily_statistics(daily, period = period, variable = NULL, station_code = NULL, ...))
  }
  if (identical(analysis, "daily_availability")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(table_daily_availability(daily, period = period, variable = NULL, station_code = NULL, ...))
  }
  if (identical(analysis, "flow_duration")) {
    if (is.null(variable)) variable <- "discharge"
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(hydrodatabr_flow_duration_analysis(daily, exceedance_probabilities = exceedance_probabilities))
  }
  if (identical(analysis, "flow_indices")) {
    if (is.null(variable)) variable <- "discharge"
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(hydrodatabr_flow_indices_analysis(daily))
  }
  if (identical(analysis, "monthly_flow_indices")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = "discharge", station_code = station_code)
    return(hydrodatabr_monthly_flow_indices_analysis(daily))
  }
  if (identical(analysis, "rainfall_indices")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = "rainfall", station_code = station_code)
    return(hydrodatabr_rainfall_indices_analysis(daily, year_start_month, wet_day_threshold))
  }
  if (identical(analysis, "annual_maxima")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(hydrodatabr_annual_maxima_analysis_e(daily, year_start_month, complete_years_only = complete_years_only, ...))
  }
  if (identical(analysis, "rainfall_annual_maxima")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = "rainfall", station_code = station_code)
    return(hydrodatabr_annual_maxima_analysis_e(daily, year_start_month, complete_years_only = complete_years_only, ...))
  }
  if (identical(analysis, "low_flows")) {
    if (is.null(variable)) variable <- "discharge"
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(hydrodatabr_low_flow_analysis_e(daily, year_start_month, durations, complete_years_only = complete_years_only, ...))
  }
  if (identical(analysis, "rainfall_diagnostics")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = "rainfall", station_code = station_code)
    return(hydrodatabr_rainfall_diagnostics_analysis(daily, ...))
  }
  if (identical(analysis, "daily_gap_summary")) {
    daily <- hydrodatabr_filter_daily_for_analysis(data, variable = variable, station_code = station_code)
    return(hydrodatabr_daily_gap_summary_analysis(daily, period = period))
  }
  if (identical(analysis, "hydrometry_diagnostics")) {
    return(diagnose_daily_hydrometry(data, station_code = station_code, ...))
  }
  if (identical(analysis, "measurement_diagnostics")) {
    explicit_measurements <- if (is.data.frame(data) && !hydrodatabr_is_daily_frame(data)) {
      as.data.frame(data, stringsAsFactors = FALSE)
    } else if (is.data.frame(data)) {
      data.frame()
    } else {
      hydrodatabr_extract_component_data(data, "measurements")
    }
    reference <- ana_hydro_ref_resolve(
      x = data,
      station_code = station_code,
      measurements = explicit_measurements,
      use_internal_database = TRUE
    )
    measurements <- hydrodatabr_filter_station(reference$measurements, station_code)
    out <- hydrodatabr_measurement_diagnostics_analysis(measurements, ...)
    if (nrow(out) > 0) {
      out$hydrometry_reference_source <- reference$source
      out$hydrometry_reference_snapshot <- reference$snapshot
    }
    return(out)
  }
  explicit_curves <- if (is.data.frame(data)) {
    data.frame()
  } else {
    hydrodatabr_extract_component_data(data, "rating_curves")
  }
  explicit_measurements <- if (is.data.frame(data)) {
    data.frame()
  } else {
    hydrodatabr_extract_component_data(data, "measurements")
  }
  if (is.data.frame(data) && !hydrodatabr_is_daily_frame(data)) {
    names_data <- names(data)
    looks_like_curve <- any(c("coefficient_a", "coefficient_n", "rating_curve_id") %in% names_data)
    looks_like_measurement <- any(c("measurement_date", "measurement_datetime", "discharge_m3s") %in% names_data) &&
      !looks_like_curve
    if (isTRUE(looks_like_curve)) {
      explicit_curves <- as.data.frame(data, stringsAsFactors = FALSE)
    } else if (isTRUE(looks_like_measurement)) {
      explicit_measurements <- as.data.frame(data, stringsAsFactors = FALSE)
    }
  }
  reference <- ana_hydro_ref_resolve(
    x = data,
    station_code = station_code,
    measurements = explicit_measurements,
    rating_curves = explicit_curves,
    use_internal_database = TRUE
  )
  curves <- hydrodatabr_filter_station(reference$rating_curves, station_code)
  measurements <- hydrodatabr_filter_station(reference$measurements, station_code)
  out <- hydrodatabr_rating_diagnostics_analysis(curves, measurements, ...)
  if (nrow(out) > 0) {
    out$hydrometry_reference_source <- reference$source
    out$hydrometry_reference_snapshot <- reference$snapshot
  }
  out
}
# Analysis coverage, flags, diagnostic tables, and derived plots.
# Stage 08E analysis, diagnostic, and plotting coverage helpers.
# These helpers are source-neutral and deterministic.
utils::globalVariables(c(
  ".data", "year", "value", "max_value", "min_value", "duration_days",
  "flag_class", "n_flags", "diagnostic_flag", "n_occurrences", "date",
  "relative_error_pct", "log_residual", "discharge_m3s", "stage_cm",
  "measurement_date", "month", "month_label", "index", "index_value",
  "rank_descending", "rank_ascending", "selected_section", "curve_label",
  "duration_label"
))
hydrodatabr_flag_class <- function(n_flags) {
  out <- ifelse(is.na(n_flags), "Nao avaliado",
    ifelse(n_flags <= 0, "Sem flags",
      ifelse(n_flags == 1, "1 flag",
        ifelse(n_flags == 2, "2 flags", "3+ flags")
      )
    )
  )
  factor(out, levels = c("Sem flags", "1 flag", "2 flags", "3+ flags", "Nao avaliado"))
}
hydrodatabr_attention_level <- function(n_flags) {
  out <- ifelse(is.na(n_flags), "nao_avaliado",
    ifelse(n_flags <= 0, "baixo",
      ifelse(n_flags == 1, "moderado", "alto")
    )
  )
  as.character(out)
}
hydrodatabr_compact_flags <- function(data, flag_cols) {
  if (!is.data.frame(data) || nrow(data) == 0 || length(flag_cols) == 0) {
    return(rep("", if (is.data.frame(data)) nrow(data) else 0L))
  }
  flag_cols <- flag_cols[flag_cols %in% names(data)]
  if (length(flag_cols) == 0) {
    return(rep("", nrow(data)))
  }
  labels <- gsub("^flag_", "", flag_cols)
  labels <- gsub("_", " ", labels)
  vapply(seq_len(nrow(data)), function(i) {
    hit <- flag_cols[which(as.logical(data[i, flag_cols, drop = TRUE]))]
    if (length(hit) == 0) {
      return("Nenhuma")
    }
    paste(labels[match(hit, flag_cols)], collapse = "; ")
  }, character(1))
}
hydrodatabr_daily_n_flags <- function(data) {
  n <- if (is.data.frame(data)) nrow(data) else 0L
  if (n == 0) {
    return(integer(0))
  }
  if ("n_flags" %in% names(data)) {
    out <- suppressWarnings(as.integer(data$n_flags))
    out[is.na(out)] <- 0L
    return(out)
  }
  if ("flag_count" %in% names(data)) {
    out <- suppressWarnings(as.integer(data$flag_count))
    out[is.na(out)] <- 0L
    return(out)
  }
  flag_cols <- grep("^flag_", names(data), value = TRUE)
  if (length(flag_cols) > 0) {
    m <- data[, flag_cols, drop = FALSE]
    return(as.integer(rowSums(as.data.frame(lapply(m, function(x) as.logical(x) %in% TRUE)), na.rm = TRUE)))
  }
  if ("flagged" %in% names(data)) {
    return(as.integer(as.logical(data$flagged) %in% TRUE))
  }
  rep(0L, n)
}
hydrodatabr_flag_cols <- function(data) {
  grep("^flag_", names(data), value = TRUE)
}
hydrodatabr_add_flag_summary <- function(data) {
  flag_cols <- hydrodatabr_flag_cols(data)
  if (length(flag_cols) == 0) {
    data$n_flags <- NA_integer_
    data$flags_summary <- "Nao avaliado"
  } else {
    data$n_flags <- as.integer(rowSums(as.data.frame(lapply(data[, flag_cols, drop = FALSE], function(x) as.logical(x) %in% TRUE)), na.rm = TRUE))
    data$flags_summary <- hydrodatabr_compact_flags(data, flag_cols)
  }
  data$flag_class <- hydrodatabr_flag_class(data$n_flags)
  data$attention_level <- hydrodatabr_attention_level(data$n_flags)
  data
}
hydrodatabr_iqr_outlier_flags <- function(values, high_multiplier = 3, low_multiplier = 3) {
  values <- suppressWarnings(as.numeric(values))
  finite <- values[is.finite(values)]
  out <- data.frame(high = rep(FALSE, length(values)), low = rep(FALSE, length(values)))
  if (length(finite) < 5) {
    return(out)
  }
  qs <- stats::quantile(finite, probs = c(0.25, 0.75), na.rm = TRUE, type = 8)
  iqr <- qs[[2]] - qs[[1]]
  if (!is.finite(iqr) || iqr <= 0) {
    return(out)
  }
  high_limit <- qs[[2]] + high_multiplier * iqr
  low_limit <- qs[[1]] - low_multiplier * iqr
  out$high <- is.finite(values) & values > high_limit
  out$low <- is.finite(values) & values < low_limit
  out
}
hydrodatabr_window_missing <- function(g, center_date, before = 3L, after = 3L) {
  if (is.na(center_date)) {
    return(FALSE)
  }
  dates <- seq(as.Date(center_date) - before, as.Date(center_date) + after, by = "day")
  rows <- match(dates, as.Date(g$date))
  if (any(is.na(rows))) {
    return(TRUE)
  }
  values <- suppressWarnings(as.numeric(g$value[rows]))
  any(!is.finite(values))
}
hydrodatabr_count_flags_in_dates <- function(g, dates) {
  if (length(dates) == 0) {
    return(0L)
  }
  rows <- match(as.Date(dates), as.Date(g$date))
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0) {
    return(0L)
  }
  sum(hydrodatabr_daily_n_flags(g[rows, , drop = FALSE]) > 0, na.rm = TRUE)
}
hydrodatabr_annual_maxima_analysis_e <- function(daily,
                                                 year_start_month,
                                                 complete_years_only = TRUE,
                                                 valid_fraction_threshold = 0.90,
                                                 gap_window_days = 3L,
                                                 repeated_digits = 6L) {
  groups <- hydrodatabr_split_by_station_variable_year(daily, year_start_month)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g_original <- groups[[i]]
    year_info <- hydrodatabr_analysis_year_info(g_original, year_start_month)
    if (isTRUE(complete_years_only) && !isTRUE(year_info$complete_year[[1]])) {
      pieces[[i]] <- data.frame()
      next
    }
    g <- hydrodatabr_complete_group_to_analysis_year(g_original, year_start_month)
    values <- suppressWarnings(as.numeric(g$value))
    finite <- is.finite(values)
    n_values <- sum(finite)
    max_value <- NA_real_
    max_date <- as.Date(NA)
    n_equal <- 0L
    dates_equal <- character(0)
    if (n_values > 0) {
      max_value <- max(values[finite])
      equal_idx <- which(finite & values == max_value)
      n_equal <- length(equal_idx)
      dates_equal <- format(g$date[equal_idx], "%Y-%m-%d")
      max_date <- g$date[equal_idx[[1]]]
    }
    valid_fraction <- if (year_info$expected_days[[1]] > 0) n_values / year_info$expected_days[[1]] else NA_real_
    negative_values_year <- sum(is.finite(values) & values < 0, na.rm = TRUE)
    zero_values_year <- sum(is.finite(values) & values == 0, na.rm = TRUE)
    is_rain <- identical(as.character(g$variable[[1]]), "rainfall")
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = hydrodatabr_first_non_missing(g$unit),
      year = g$year[[1]],
      year_start_month = as.integer(year_start_month),
      year_start_date = year_info$year_start_date[[1]],
      year_end_date = year_info$year_end_date[[1]],
      complete_year = year_info$complete_year[[1]],
      expected_days = year_info$expected_days[[1]],
      n_days = year_info$observed_days[[1]],
      n_values = n_values,
      missing_days = year_info$expected_days[[1]] - n_values,
      missing_fraction = if (year_info$expected_days[[1]] > 0) 1 - valid_fraction else NA_real_,
      completeness_pct = 100 * valid_fraction,
      max_value = max_value,
      max_date = max_date,
      n_days_equal_to_max = n_equal,
      dates_equal_to_max = paste(dates_equal, collapse = ";"),
      negative_values_year = negative_values_year,
      zero_values_year = zero_values_year,
      flag_partial_year = !isTRUE(year_info$complete_year[[1]]),
      flag_few_valid_days = is.finite(valid_fraction) && valid_fraction < valid_fraction_threshold,
      flag_gap_near_max = hydrodatabr_window_missing(g, max_date, gap_window_days, gap_window_days),
      flag_tied_max_within_year = n_equal > 1L,
      flag_consistency_issue_on_max = hydrodatabr_count_flags_in_dates(g, max_date) > 0,
      flag_zero_maximum = isTRUE(is_rain) && is.finite(max_value) && max_value <= 0,
      flag_negative_values_year = isTRUE(is_rain) && negative_values_year > 0,
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  if (nrow(out) == 0) {
    return(hydrodatabr_empty_annual_maxima())
  }
  out$flag_high_outlier_iqr <- FALSE
  out$flag_low_outlier_iqr <- FALSE
  out$flag_repeated_annual_max_across_years <- FALSE
  keys <- paste(out$station_code, out$variable, sep = "\r")
  for (key in unique(keys)) {
    idx <- which(keys == key)
    outlier <- hydrodatabr_iqr_outlier_flags(out$max_value[idx])
    out$flag_high_outlier_iqr[idx] <- outlier$high
    out$flag_low_outlier_iqr[idx] <- outlier$low
    rounded <- round(out$max_value[idx], repeated_digits)
    repeated <- duplicated(rounded) | duplicated(rounded, fromLast = TRUE)
    repeated[!is.finite(rounded)] <- FALSE
    out$flag_repeated_annual_max_across_years[idx] <- repeated
  }
  out$flag_candidate_exclusion <- out$flag_high_outlier_iqr | out$flag_low_outlier_iqr
  out$flag_possible_underestimated_maximum <- out$flag_gap_near_max | out$flag_few_valid_days
  out <- hydrodatabr_add_flag_summary(out)
  out$rank_descending <- ave(-out$max_value, out$station_code, out$variable, FUN = function(z) rank(z, ties.method = "min", na.last = "keep"))
  out[order(out$station_code, out$variable, out$year), , drop = FALSE]
}
hydrodatabr_low_flow_analysis_e <- function(daily,
                                            year_start_month,
                                            durations,
                                            complete_years_only = TRUE,
                                            valid_window_fraction_threshold = 0.80,
                                            gap_window_days = 3L) {
  durations <- unique(as.integer(durations))
  durations <- durations[!is.na(durations) & durations > 0L]
  if (length(durations) == 0) {
    stop("`durations` must contain at least one positive integer.", call. = FALSE)
  }
  daily$year <- hydrodatabr_analysis_year(daily$date, year_start_month)
  groups <- split(daily, paste(daily$station_code, daily$variable, daily$year, sep = "\r"), drop = TRUE)
  pieces <- list()
  k <- 0L
  for (i in seq_along(groups)) {
    g_original <- groups[[i]]
    year_info <- hydrodatabr_analysis_year_info(g_original, year_start_month)
    if (isTRUE(complete_years_only) && !isTRUE(year_info$complete_year[[1]])) {
      next
    }
    g <- hydrodatabr_complete_group_to_analysis_year(g_original, year_start_month)
    values <- suppressWarnings(as.numeric(g$value))
    for (duration in durations) {
      roll <- hydrodatabr_rolling_mean(values, duration)
      finite_roll <- is.finite(roll)
      n_expected <- max(0L, length(values) - duration + 1L)
      n_valid <- sum(finite_roll)
      min_value <- NA_real_
      min_start <- as.Date(NA)
      min_end <- as.Date(NA)
      month_min <- NA_integer_
      month_failure_pct <- NA_real_
      n_flags_window <- 0L
      if (n_valid > 0) {
        min_value <- min(roll[finite_roll])
        min_idx <- which(finite_roll & roll == min_value)[[1]]
        min_start <- g$date[[min_idx]]
        min_end <- g$date[[min_idx + duration - 1L]]
        month_min <- as.integer(format(min_start, "%m"))
        month_rows <- as.integer(format(g$date, "%m")) == month_min
        month_values <- values[month_rows]
        month_failure_pct <- 100 * sum(!is.finite(month_values)) / length(month_values)
        n_flags_window <- hydrodatabr_count_flags_in_dates(g, seq(min_start, min_end, by = "day"))
      }
      valid_pct <- if (n_expected > 0) 100 * n_valid / n_expected else NA_real_
      k <- k + 1L
      pieces[[k]] <- data.frame(
        station_code = g$station_code[[1]],
        variable = g$variable[[1]],
        unit = hydrodatabr_first_non_missing(g$unit),
        year = g$year[[1]],
        year_start_month = as.integer(year_start_month),
        duration_days = duration,
        year_start_date = year_info$year_start_date[[1]],
        year_end_date = year_info$year_end_date[[1]],
        complete_year = year_info$complete_year[[1]],
        expected_days = year_info$expected_days[[1]],
        n_days = year_info$observed_days[[1]],
        n_windows_expected = n_expected,
        n_valid_windows = n_valid,
        windows_valid_pct = valid_pct,
        missing_days = year_info$expected_days[[1]] - sum(is.finite(values)),
        min_value = min_value,
        min_start_date = min_start,
        min_end_date = min_end,
        month_minimum = month_min,
        month_minimum_label = if (!is.na(month_min)) hydrodatabr_month_labels[[month_min]] else NA_character_,
        failures_pct_month_minimum = month_failure_pct,
        flag_few_valid_windows = is.finite(valid_pct) && valid_pct < 100 * valid_window_fraction_threshold,
        flag_failures_month_minimum = is.finite(month_failure_pct) && month_failure_pct > 0,
        flag_gap_near_minimum = hydrodatabr_window_missing(g, min_start, gap_window_days, duration + gap_window_days - 1L),
        flag_consistency_issue_on_min_window = n_flags_window > 0,
        flag_zero_or_negative_minimum = is.finite(min_value) && min_value <= 0,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  if (nrow(out) == 0) {
    return(hydrodatabr_empty_low_flows())
  }
  out$flag_high_outlier_iqr <- FALSE
  out$flag_low_outlier_iqr <- FALSE
  keys <- paste(out$station_code, out$variable, out$duration_days, sep = "\r")
  for (key in unique(keys)) {
    idx <- which(keys == key)
    outlier <- hydrodatabr_iqr_outlier_flags(out$min_value[idx])
    out$flag_high_outlier_iqr[idx] <- outlier$high
    out$flag_low_outlier_iqr[idx] <- outlier$low
  }
  out$flag_candidate_exclusion <- out$flag_low_outlier_iqr | out$flag_zero_or_negative_minimum
  out <- hydrodatabr_add_flag_summary(out)
  out$rank_ascending <- ave(out$min_value, out$station_code, out$variable, out$duration_days, FUN = function(z) rank(z, ties.method = "min", na.last = "keep"))
  out[order(out$station_code, out$variable, out$duration_days, out$year), , drop = FALSE]
}
hydrodatabr_monthly_flow_indices_analysis <- function(daily) {
  daily <- daily[as.character(daily$variable) %in% "discharge", , drop = FALSE]
  if (nrow(daily) == 0) {
    return(data.frame())
  }
  daily$month <- as.integer(format(as.Date(daily$date), "%m"))
  groups <- split(daily, paste(daily$station_code, daily$month, sep = "\r"), drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    values <- suppressWarnings(as.numeric(g$value))
    values <- values[is.finite(values)]
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = "discharge",
      unit = hydrodatabr_first_non_missing(g$unit, "m3/s"),
      month = g$month[[1]],
      month_label = hydrodatabr_month_labels[[g$month[[1]]]],
      n_values = length(values),
      qmlt_month = if (length(values) > 0) mean(values) else NA_real_,
      q90_month = if (length(values) > 0) as.numeric(stats::quantile(values, probs = 0.10, na.rm = TRUE, type = 8)) else NA_real_,
      q95_month = if (length(values) > 0) as.numeric(stats::quantile(values, probs = 0.05, na.rm = TRUE, type = 8)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  out[order(out$station_code, out$month), , drop = FALSE]
}
hydrodatabr_daily_gap_summary_analysis <- function(daily, period = "monthly") {
  period <- match.arg(period, c("monthly", "annual"))
  daily <- hydrodatabr_prepare_daily(daily)
  if (nrow(daily) == 0) {
    return(data.frame())
  }
  daily$year <- as.integer(format(daily$date, "%Y"))
  daily$month <- as.integer(format(daily$date, "%m"))
  key <- if (identical(period, "monthly")) {
    paste(daily$station_code, daily$variable, daily$year, daily$month, sep = "\r")
  } else {
    paste(daily$station_code, daily$variable, daily$year, sep = "\r")
  }
  groups <- split(daily, key, drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- hydrodatabr_complete_daily_dates(groups[[i]])
    values <- suppressWarnings(as.numeric(g$value))
    pieces[[i]] <- data.frame(
      station_code = g$station_code[[1]],
      variable = g$variable[[1]],
      unit = hydrodatabr_first_non_missing(g$unit),
      period = period,
      year = as.integer(format(g$date[[1]], "%Y")),
      month = if (identical(period, "monthly")) as.integer(format(g$date[[1]], "%m")) else NA_integer_,
      month_label = if (identical(period, "monthly")) hydrodatabr_month_labels[[as.integer(format(g$date[[1]], "%m"))]] else NA_character_,
      n_days = nrow(g),
      n_values = sum(is.finite(values)),
      missing_days = sum(!is.finite(values)),
      missing_pct = 100 * sum(!is.finite(values)) / nrow(g),
      zero_or_negative_days = sum(is.finite(values) & values <= 0),
      stringsAsFactors = FALSE
    )
  }
  out <- hydrodatabr_bind_rows_base(pieces)
  out[order(out$station_code, out$variable, out$year, out$month), , drop = FALSE]
}
hydrodatabr_rainfall_diagnostics_analysis_one <- function(daily,
                                                          very_high_threshold = 400,
                                                          zero_run_length = 20,
                                                          repeated_value_min_count = 5,
                                                          robust_mad_multiplier = 8) {
  daily <- hydrodatabr_complete_daily_dates(daily)
  values <- suppressWarnings(as.numeric(daily$value))
  daily$flag_negative_rainfall <- is.finite(values) & values < 0
  daily$flag_very_high_rainfall <- is.finite(values) & values >= very_high_threshold
  finite_values <- values[is.finite(values)]
  med <- if (length(finite_values) > 0) stats::median(finite_values) else NA_real_
  madv <- if (length(finite_values) > 0) stats::mad(finite_values, constant = 1.4826) else NA_real_
  daily$flag_high_robust_outlier <- is.finite(values) & is.finite(med) & is.finite(madv) & madv > 0 & values > med + robust_mad_multiplier * madv
  zero <- is.finite(values) & values == 0
  long_zero <- rep(FALSE, length(zero))
  if (any(zero)) {
    r <- rle(zero)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    long_runs <- which(r$values & r$lengths >= zero_run_length)
    for (j in long_runs) long_zero[starts[[j]]:ends[[j]]] <- TRUE
  }
  daily$flag_long_zero_sequence <- long_zero
  positive <- values[is.finite(values) & values > 0]
  repeated_values <- names(which(table(round(positive, 6)) >= repeated_value_min_count))
  daily$flag_repeated_positive_value <- is.finite(values) & values > 0 & as.character(round(values, 6)) %in% repeated_values
  status <- if ("source_status" %in% names(daily)) tolower(as.character(daily$source_status)) else rep("", nrow(daily))
  daily$flag_status_attention <- nzchar(status) & !status %in% c("ok", "good", "1", "2", "consistente", "bruto", "na", "nan")
  hydrodatabr_add_flag_summary(daily)
}
hydrodatabr_rainfall_diagnostics_analysis <- function(daily,
                                                      very_high_threshold = 400,
                                                      zero_run_length = 20,
                                                      repeated_value_min_count = 5,
                                                      robust_mad_multiplier = 8,
                                                      flagged_only = TRUE) {
  daily <- daily[as.character(daily$variable) %in% "rainfall", , drop = FALSE]
  if (nrow(daily) == 0) {
    return(data.frame())
  }
  groups <- hydrodatabr_split_by_station_variable(daily)
  pieces <- lapply(groups, hydrodatabr_rainfall_diagnostics_analysis_one,
    very_high_threshold = very_high_threshold,
    zero_run_length = zero_run_length,
    repeated_value_min_count = repeated_value_min_count,
    robust_mad_multiplier = robust_mad_multiplier
  )
  out <- hydrodatabr_bind_rows_base(pieces)
  if (isTRUE(flagged_only)) {
    out <- out[!is.na(out$n_flags) & out$n_flags > 0, , drop = FALSE]
  }
  out[order(out$station_code, out$date), , drop = FALSE]
}
hydrodatabr_measurement_diagnostics_analysis <- function(measurements,
                                                         stage_tolerance = 0,
                                                         discharge_tolerance = 0,
                                                         min_group_size = 2L) {
  raw <- hydrodatabr_as_data_frame(measurements)
  if (nrow(raw) == 0) {
    return(data.frame())
  }
  prep <- hydrodatabr_prepare_measurements(raw)
  if ("station_code" %in% names(raw)) prep$station_code <- as.character(raw$station_code) else prep$station_code <- NA_character_
  prep$flag_stage_non_positive <- is.finite(prep$stage_cm) & prep$stage_cm <= 0
  prep$flag_discharge_non_positive <- is.finite(prep$discharge_m3s) & prep$discharge_m3s <= 0
  prep$flag_repeated_stage_variable_discharge <- FALSE
  prep$flag_repeated_discharge_variable_stage <- FALSE
  stage_key <- ifelse(is.finite(prep$stage_cm), as.character(round(prep$stage_cm / max(stage_tolerance, .Machine$double.eps)) * max(stage_tolerance, .Machine$double.eps)), NA_character_)
  for (key in unique(stage_key[!is.na(stage_key)])) {
    idx <- which(stage_key == key)
    if (length(idx) >= min_group_size && diff(range(prep$discharge_m3s[idx], na.rm = TRUE)) > discharge_tolerance) {
      prep$flag_repeated_stage_variable_discharge[idx] <- TRUE
    }
  }
  q_key <- ifelse(is.finite(prep$discharge_m3s), as.character(round(prep$discharge_m3s / max(discharge_tolerance, .Machine$double.eps)) * max(discharge_tolerance, .Machine$double.eps)), NA_character_)
  for (key in unique(q_key[!is.na(q_key)])) {
    idx <- which(q_key == key)
    if (length(idx) >= min_group_size && diff(range(prep$stage_cm[idx], na.rm = TRUE)) > stage_tolerance) {
      prep$flag_repeated_discharge_variable_stage[idx] <- TRUE
    }
  }
  prep <- hydrodatabr_add_flag_summary(prep)
  prep[order(prep$station_code, prep$measurement_date), , drop = FALSE]
}
hydrodatabr_rating_curve_value <- function(curve_row, stage_cm) {
  a_col <- hydrodatabr_first_existing_name(curve_row, c("coefficient_a", "a"))
  h0_col <- hydrodatabr_first_existing_name(curve_row, c("coefficient_h0_cm", "h0_cm", "coefficient_h0"))
  b_col <- hydrodatabr_first_existing_name(curve_row, c("coefficient_b", "coefficient_n", "b", "n"))
  if (any(is.na(c(a_col, h0_col, b_col)))) return(NA_real_)
  a <- suppressWarnings(as.numeric(curve_row[[a_col]][[1]]))
  h0 <- suppressWarnings(as.numeric(curve_row[[h0_col]][[1]]))
  b <- suppressWarnings(as.numeric(curve_row[[b_col]][[1]]))
  if (!all(is.finite(c(a, h0, b, stage_cm))) || stage_cm <= h0) return(NA_real_)
  a * ((stage_cm - h0) ^ b)
}
hydrodatabr_rating_match_curve <- function(curves, measurement_date, stage_cm) {
  if (nrow(curves) == 0) return(integer(0))
  stage_min_col <- hydrodatabr_first_existing_name(curves, c("stage_min_cm", "h_min_cm", "stage_min"))
  stage_max_col <- hydrodatabr_first_existing_name(curves, c("stage_max_cm", "h_max_cm", "stage_max"))
  valid_from_col <- hydrodatabr_first_existing_name(curves, c("valid_from", "valid_from_date", "validity_start_date", "start_date", "date_start"))
  valid_to_col <- hydrodatabr_first_existing_name(curves, c("valid_to", "valid_to_date", "validity_end_date", "end_date", "date_end"))
  ok <- rep(TRUE, nrow(curves))
  if (!is.na(stage_min_col)) ok <- ok & suppressWarnings(as.numeric(curves[[stage_min_col]])) <= stage_cm
  if (!is.na(stage_max_col)) ok <- ok & suppressWarnings(as.numeric(curves[[stage_max_col]])) >= stage_cm
  if (!is.na(valid_from_col) && !is.na(measurement_date)) {
    vf <- as.Date(curves[[valid_from_col]])
    ok <- ok & (is.na(vf) | vf <= measurement_date)
  }
  if (!is.na(valid_to_col) && !is.na(measurement_date)) {
    vt <- as.Date(curves[[valid_to_col]])
    ok <- ok & (is.na(vt) | vt >= measurement_date)
  }
  which(ok)[1]
}
hydrodatabr_rating_diagnostics_analysis <- function(rating_curves,
                                                    measurements,
                                                    envelope_quantile = 0.95) {
  curves <- hydrodatabr_as_data_frame(rating_curves)
  measurements <- hydrodatabr_as_data_frame(measurements)
  if (nrow(curves) == 0 || nrow(measurements) == 0) {
    return(data.frame())
  }
  meas <- hydrodatabr_prepare_measurements(measurements)
  if ("station_code" %in% names(measurements)) meas$station_code <- as.character(measurements$station_code) else meas$station_code <- NA_character_
  if ("station_code" %in% names(curves)) curves$station_code <- as.character(curves$station_code) else curves$station_code <- NA_character_
  curve_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_id", "curve_id"))
  segment_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_segment_id", "segment_id"))
  out <- vector("list", nrow(meas))
  for (i in seq_len(nrow(meas))) {
    st <- meas$station_code[[i]]
    st_curves <- if (!is.na(st) && "station_code" %in% names(curves)) curves[curves$station_code %in% st, , drop = FALSE] else curves
    idx <- hydrodatabr_rating_match_curve(st_curves, meas$measurement_date[[i]], meas$stage_cm[[i]])
    predicted <- if (length(idx) == 1) hydrodatabr_rating_curve_value(st_curves[idx, , drop = FALSE], meas$stage_cm[[i]]) else NA_real_
    observed <- meas$discharge_m3s[[i]]
    rel <- if (is.finite(observed) && observed != 0 && is.finite(predicted)) 100 * (observed - predicted) / observed else NA_real_
    log_res <- if (is.finite(observed) && observed > 0 && is.finite(predicted) && predicted > 0) log(observed) - log(predicted) else NA_real_
    out[[i]] <- data.frame(
      station_code = st,
      measurement_date = meas$measurement_date[[i]],
      stage_cm = meas$stage_cm[[i]],
      discharge_m3s = observed,
      predicted_discharge_m3s = predicted,
      relative_error_pct = rel,
      log_residual = log_res,
      rating_curve_id = if (length(idx) == 1 && !is.na(curve_id_col)) as.character(st_curves[[curve_id_col]][[idx]]) else NA_character_,
      rating_curve_segment_id = if (length(idx) == 1 && !is.na(segment_id_col)) as.character(st_curves[[segment_id_col]][[idx]]) else NA_character_,
      flag_no_valid_curve = length(idx) == 0,
      flag_non_positive_measurement = is.finite(observed) && observed <= 0,
      stringsAsFactors = FALSE
    )
  }
  res <- hydrodatabr_bind_rows_base(out)
  if (nrow(res) == 0) return(res)
  abs_res <- abs(res$log_residual)
  limit <- if (sum(is.finite(abs_res)) >= 5) as.numeric(stats::quantile(abs_res, probs = envelope_quantile, na.rm = TRUE, type = 8)) else NA_real_
  res$envelope_limit_abs_log_residual <- limit
  res$flag_outside_residual_envelope <- is.finite(abs_res) & is.finite(limit) & abs_res > limit
  res$flag_large_relative_error_10pct <- is.finite(res$relative_error_pct) & abs(res$relative_error_pct) > 10
  res$flag_large_relative_error_25pct <- is.finite(res$relative_error_pct) & abs(res$relative_error_pct) > 25
  res <- hydrodatabr_add_flag_summary(res)
  res[order(res$station_code, res$measurement_date), , drop = FALSE]
}
hydrodatabr_diagnostic_label <- function(x) {
  x <- gsub("^flag_", "", as.character(x))
  map <- c(
    manual = "Sinalizacao manual",
    partial_year = "Ano parcial",
    few_valid_days = "Poucos dias validos",
    few_valid_windows = "Poucas janelas validas",
    gap_near_max = "Falha no entorno do maximo",
    gap_near_minimum = "Falha no entorno da minima",
    tied_max_within_year = "Empate no maximo anual",
    repeated_annual_max_across_years = "Maximo repetido entre anos",
    consistency_issue_on_max = "Flag no dia do maximo",
    consistency_issue_on_min_window = "Flag na janela da minima",
    zero_maximum = "Maximo igual a zero",
    negative_values_year = "Valor negativo no ano",
    high_outlier_iqr = "Possivel outlier alto",
    low_outlier_iqr = "Possivel outlier baixo",
    candidate_exclusion = "Candidato a exclusao",
    possible_underestimated_maximum = "Maximo possivelmente subestimado",
    failures_month_minimum = "Falhas no mes da minima",
    zero_or_negative_minimum = "Minima nula ou negativa",
    negative_rainfall = "Chuva negativa",
    very_high_rainfall = "Chuva muito alta",
    high_robust_outlier = "Outlier robusto alto",
    long_zero_sequence = "Sequencia longa de zeros",
    repeated_positive_value = "Valor positivo repetido",
    status_attention = "Status de atencao",
    stage_non_positive = "Cota nao positiva",
    discharge_non_positive = "Vazao nao positiva",
    repeated_stage_variable_discharge = "Cota repetida com vazao variavel",
    repeated_discharge_variable_stage = "Vazao repetida com cota variavel",
    no_valid_curve = "Sem curva valida",
    non_positive_measurement = "Medicao nao positiva",
    outside_residual_envelope = "Fora do envelope de residuos",
    large_relative_error_10pct = "Erro relativo > 10%",
    large_relative_error_25pct = "Erro relativo > 25%"
  )
  out <- unname(map[x])
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- gsub("_", " ", x[missing])
  out
}
hydrodatabr_diagnostic_count_table <- function(data) {
  flag_cols <- hydrodatabr_flag_cols(data)
  if (length(flag_cols) == 0 || nrow(data) == 0) {
    return(data.frame(
      diagnostic_flag = character(),
      diagnostic_label = character(),
      n_occurrences = integer(),
      stringsAsFactors = FALSE
    ))
  }
  raw <- gsub("^flag_", "", flag_cols)
  data.frame(
    diagnostic_flag = raw,
    diagnostic_label = hydrodatabr_diagnostic_label(raw),
    n_occurrences = as.integer(vapply(flag_cols, function(col) sum(as.logical(data[[col]]) %in% TRUE, na.rm = TRUE), integer(1))),
    stringsAsFactors = FALSE
  )
}
hydrodatabr_flag_palette <- function(classes) {
  classes <- as.character(classes)
  values <- c(
    "Sem flags" = "#2c7fb8",
    "1 flag" = "#fdae61",
    "2 flags" = "#f46d43",
    "3+ flags" = "#a50026",
    "Nao avaliado" = "grey65"
  )
  values[intersect(names(values), unique(classes))]
}
hydrodatabr_plot_year_scale <- function(years, max_year_labels = 7) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  ggplot2::scale_x_continuous(
    name = "Ano",
    breaks = hydrodatabr_year_breaks(years, max_breaks = max_year_labels),
    guide = ggplot2::guide_axis(check.overlap = TRUE)
  )
}
plot_extreme_annual_maxima <- function(data, title = NULL, subtitle = NULL,
                                       max_year_labels = 7, base_size = 11, ...) {
  data <- hydrodatabr_as_data_frame(data)
  if (nrow(data) == 0 || !all(c("year", "max_value") %in% names(data))) {
    stop("Annual-maxima data must contain `year` and `max_value`.", call. = FALSE)
  }
  if (!"flag_class" %in% names(data)) data$flag_class <- hydrodatabr_flag_class(if ("n_flags" %in% names(data)) data$n_flags else NA_integer_)
  y_lab <- hydrodatabr_variable_label(hydrodatabr_first_non_missing(data$variable, "discharge"), hydrodatabr_first_non_missing(data$unit, NA_character_))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["year"]], y = .data[["max_value"]])) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[["year"]], y = 0, yend = .data[["max_value"]]), color = "grey75", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(color = .data[["flag_class"]]), size = 2.2, alpha = 0.95) +
    hydrodatabr_plot_year_scale(data$year, max_year_labels = max_year_labels) +
    ggplot2::scale_y_continuous(name = paste0("M\u00e1xima anual - ", y_lab), labels = hydrodatabr_number_labels()) +
    ggplot2::scale_color_manual(name = "Sinaliza\u00e7\u00e3o", values = hydrodatabr_flag_palette(data$flag_class), drop = FALSE) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
  if ("station_code" %in% names(data) && length(unique(data$station_code)) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["station_code"]]), scales = "free_y")
  }
  p
}
plot_extreme_low_flows <- function(data, duration_days = NULL, title = NULL,
                                   subtitle = NULL, max_year_labels = 6,
                                   base_size = 11, ...) {
  data <- hydrodatabr_as_data_frame(data)
  if (nrow(data) == 0 || !all(c("year", "min_value", "duration_days") %in% names(data))) {
    stop("Low-flow data must contain `year`, `min_value`, and `duration_days`.", call. = FALSE)
  }
  if (!is.null(duration_days)) {
    data <- data[data$duration_days %in% as.integer(duration_days), , drop = FALSE]
  }
  if (nrow(data) == 0) stop("No low-flow data available for the selected duration.", call. = FALSE)
  if (!"flag_class" %in% names(data)) data$flag_class <- hydrodatabr_flag_class(if ("n_flags" %in% names(data)) data$n_flags else NA_integer_)
  y_lab <- hydrodatabr_variable_label(hydrodatabr_first_non_missing(data$variable, "discharge"), hydrodatabr_first_non_missing(data$unit, NA_character_))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["year"]], y = .data[["min_value"]])) +
    ggplot2::geom_line(color = "grey65", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(color = .data[["flag_class"]]), size = 2.1, alpha = 0.95) +
    hydrodatabr_plot_year_scale(data$year, max_year_labels = max_year_labels) +
    ggplot2::scale_y_continuous(name = paste0("M\u00ednima anual - ", y_lab), labels = hydrodatabr_number_labels()) +
    ggplot2::scale_color_manual(name = "Sinaliza\u00e7\u00e3o", values = hydrodatabr_flag_palette(data$flag_class), drop = FALSE) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
  if (length(unique(data$duration_days)) > 1) {
    duration_values <- sort(unique(as.integer(data$duration_days)))
    data$duration_label <- factor(
      paste0(data$duration_days, ifelse(data$duration_days == 1, " dia", " dias")),
      levels = paste0(duration_values, ifelse(duration_values == 1, " dia", " dias"))
    )
    p$data <- data
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["duration_label"]]), scales = "free_y", ncol = 2)
  }
  if ("station_code" %in% names(data) && length(unique(data$station_code)) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["station_code"]], .data[["duration_days"]]), scales = "free_y")
  }
  p
}
plot_rainfall_indices_table <- function(data, type = c("depth", "threshold", "dry_wet"),
                                        title = NULL, subtitle = NULL,
                                        max_year_labels = 5, base_size = 11, ...) {
  type <- match.arg(type)
  data <- hydrodatabr_as_data_frame(data)
  if (nrow(data) == 0) stop("No rainfall-index data available for plotting.", call. = FALSE)
  cols <- switch(type,
    depth = c("rx1day", "rx5day", "sdii"),
    threshold = c("r10", "r20", "r50"),
    dry_wet = c("cdd", "cwd")
  )
  cols <- cols[cols %in% names(data)]
  pieces <- lapply(cols, function(col) data.frame(data[, intersect(c("station_code", "year"), names(data)), drop = FALSE], index = col, index_value = data[[col]], stringsAsFactors = FALSE))
  plot_data <- hydrodatabr_bind_rows_base(pieces)
  plot_data$index <- factor(plot_data$index, levels = cols)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["year"]], y = .data[["index_value"]])) +
    ggplot2::geom_line(linewidth = 0.35, color = "grey55") +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["index"]]), scales = "free_y") +
    hydrodatabr_plot_year_scale(plot_data$year, max_year_labels = max_year_labels) +
    ggplot2::scale_y_continuous(name = "Valor do \u00edndice", labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
}
plot_rainfall_monthly_boxplot <- function(daily, title = NULL, subtitle = NULL,
                                          base_size = 11, ...) {
  daily <- hydrodatabr_prepare_daily(daily, variable = "rainfall")
  if (nrow(daily) == 0) stop("No rainfall data available for monthly boxplot.", call. = FALSE)
  daily$month <- as.integer(format(daily$date, "%m"))
  daily$month_label <- factor(hydrodatabr_month_labels[daily$month], levels = hydrodatabr_month_labels)
  ggplot2::ggplot(daily, ggplot2::aes(x = .data[["month_label"]], y = .data[["value"]])) +
    ggplot2::geom_boxplot(outlier.alpha = 0.35, linewidth = 0.25) +
    ggplot2::labs(x = "M\u00eas", y = "Precipita\u00e7\u00e3o di\u00e1ria (mm)", title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
}
plot_diagnostic_counts <- function(data, title = NULL, subtitle = NULL,
                                   min_categories = 2L, base_size = 11, ...) {
  counts <- hydrodatabr_diagnostic_count_table(data)
  counts <- counts[counts$n_occurrences > 0, , drop = FALSE]
  min_categories <- suppressWarnings(as.integer(min_categories[[1]]))
  if (!is.finite(min_categories) || min_categories < 1L) min_categories <- 1L
  if (nrow(counts) == 0) stop("No diagnostic flags available for plotting.", call. = FALSE)
  if (nrow(counts) < min_categories) {
    stop(
      "Diagnostic-count plots need at least ", min_categories,
      " diagnostic categories with occurrences. Use `table_hydro_data()` ",
      "or set `min_categories = 1` to force a simple count plot.",
      call. = FALSE
    )
  }
  ggplot2::ggplot(counts, ggplot2::aes(x = stats::reorder(.data[["diagnostic_label"]], .data[["n_occurrences"]]), y = .data[["n_occurrences"]])) +
    ggplot2::geom_col(width = 0.65, fill = "grey75", color = "grey35", linewidth = 0.25) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Ocorr\u00eancias", title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
}
plot_rating_diagnostics_table <- function(data,
                                          type = c("residual_discharge", "residual_time", "envelope", "counts"),
                                          title = NULL, subtitle = NULL,
                                          base_size = 11, ...) {
  type <- match.arg(type)
  data <- hydrodatabr_as_data_frame(data)
  if (nrow(data) == 0) stop("No rating-diagnostic data available for plotting.", call. = FALSE)
  if (!"flag_class" %in% names(data)) {
    data$flag_class <- hydrodatabr_flag_class(if ("n_flags" %in% names(data)) data$n_flags else NA_integer_)
  }
  if (identical(type, "counts")) {
    return(plot_diagnostic_counts(data, title = title, subtitle = subtitle, base_size = base_size, ...))
  }
  caption <- "Cores indicam a quantidade de sinalizacoes por medicao."
  if (identical(type, "residual_time")) {
    return(ggplot2::ggplot(data, ggplot2::aes(x = .data[["measurement_date"]], y = .data[["relative_error_pct"]])) +
      ggplot2::geom_hline(yintercept = 0, color = "grey70") +
      ggplot2::geom_hline(yintercept = c(-10, 10), linetype = "dashed", color = "grey55", linewidth = 0.3) +
      ggplot2::geom_hline(yintercept = c(-25, 25), linetype = "dotted", color = "grey55", linewidth = 0.3) +
      ggplot2::geom_point(ggplot2::aes(color = .data[["flag_class"]]), size = 1.8, alpha = 0.9) +
      ggplot2::scale_color_manual(name = "Sinalizacao", values = hydrodatabr_flag_palette(data$flag_class), drop = FALSE) +
      ggplot2::labs(x = "Data", y = "Erro relativo (%)", title = title, subtitle = subtitle,
                    caption = "Linhas tracejadas: +/-10%; linhas pontilhadas: +/-25%.") +
      theme_hydrodatabr(base_size = base_size))
  }
  if (identical(type, "envelope")) {
    envelope <- suppressWarnings(as.numeric(data$envelope_limit_abs_log_residual))
    envelope <- envelope[is.finite(envelope)]
    envelope_value <- if (length(envelope) > 0) envelope[[1]] else NA_real_
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["discharge_m3s"]], y = .data[["log_residual"]])) +
      ggplot2::geom_hline(yintercept = 0, color = "grey70")
    if (is.finite(envelope_value)) {
      p <- p +
        ggplot2::geom_hline(yintercept = envelope_value, linetype = "dashed", color = "grey45") +
        ggplot2::geom_hline(yintercept = -envelope_value, linetype = "dashed", color = "grey45")
    }
    return(p +
      ggplot2::geom_point(ggplot2::aes(color = .data[["flag_class"]]), size = 1.8, alpha = 0.9) +
      ggplot2::scale_color_manual(name = "Sinalizacao", values = hydrodatabr_flag_palette(data$flag_class), drop = FALSE) +
      ggplot2::scale_x_continuous(name = "Vazao medida (m3/s)", labels = hydrodatabr_number_labels()) +
      ggplot2::labs(y = "Residuo logaritmico", title = title, subtitle = subtitle,
                    caption = "Linhas tracejadas: envelope empirico dos residuos logaritmicos.") +
      theme_hydrodatabr(base_size = base_size))
  }
  ggplot2::ggplot(data, ggplot2::aes(x = .data[["discharge_m3s"]], y = .data[["relative_error_pct"]])) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70") +
    ggplot2::geom_hline(yintercept = c(-10, 10), linetype = "dashed", color = "grey55", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-25, 25), linetype = "dotted", color = "grey55", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(color = .data[["flag_class"]]), size = 1.8, alpha = 0.9) +
    ggplot2::scale_color_manual(name = "Sinalizacao", values = hydrodatabr_flag_palette(data$flag_class), drop = FALSE) +
    ggplot2::scale_x_continuous(name = "Vazao medida (m3/s)", labels = hydrodatabr_number_labels()) +
    ggplot2::labs(y = "Erro relativo (%)", title = title, subtitle = subtitle,
                  caption = "Linhas tracejadas: +/-10%; linhas pontilhadas: +/-25%.") +
    theme_hydrodatabr(base_size = base_size)
}
