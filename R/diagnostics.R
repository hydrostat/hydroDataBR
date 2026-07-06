# Hydrometry diagnostics.
# Source-neutral diagnostics code; does not call live services.
# Internal hydrometric-reference resolver.
# It never calls live services and never lets lower-priority data replace
# user-supplied or freshly downloaded reference tables.
ana_hydro_ref_is_nonempty_data_frame <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}

ana_hydro_ref_bind_rows <- function(x) {
  x <- x[vapply(x, is.data.frame, logical(1))]
  x <- x[vapply(x, nrow, integer(1)) > 0]
  if (length(x) == 0) {
    return(data.frame())
  }
  x <- lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE))
  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(z) {
    missing_names <- setdiff(all_names, names(z))
    for (nm in missing_names) {
      z[[nm]] <- NA
    }
    z[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

ana_hydro_ref_is_daily_frame <- function(x) {
  is.data.frame(x) && all(c("station_code", "date", "variable", "value") %in% names(x))
}

ana_hydro_ref_add_station_if_missing <- function(x, station_code) {
  if (!is.data.frame(x) || nrow(x) == 0 || is.null(station_code) || !nzchar(station_code)) {
    return(x)
  }
  if (!"station_code" %in% names(x)) {
    x$station_code <- as.character(station_code)
  }
  x
}

ana_hydro_ref_get_component <- function(x, component_names) {
  if (!is.list(x)) {
    return(NULL)
  }
  hit <- component_names[component_names %in% names(x)]
  if (length(hit) == 0) {
    return(NULL)
  }
  x[[hit[[1]]]]
}

ana_hydro_ref_extract_daily_data <- function(x) {
  if (ana_hydro_ref_is_daily_frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (!is.list(x)) {
    return(data.frame())
  }
  if ("results" %in% names(x) && is.list(x$results)) {
    result_names <- names(x$results)
    pieces <- vector("list", length(x$results))
    for (i in seq_along(x$results)) {
      piece <- ana_hydro_ref_extract_daily_data(x$results[[i]])
      if (length(result_names) >= i && !is.na(result_names[[i]]) && nzchar(result_names[[i]])) {
        piece <- ana_hydro_ref_add_station_if_missing(piece, result_names[[i]])
      }
      pieces[[i]] <- piece
    }
    return(ana_hydro_ref_bind_rows(pieces))
  }
  if (ana_hydro_ref_is_daily_frame(x$daily_data)) {
    return(as.data.frame(x$daily_data, stringsAsFactors = FALSE))
  }
  if (ana_hydro_ref_is_daily_frame(x$data)) {
    return(as.data.frame(x$data, stringsAsFactors = FALSE))
  }
  daily_names <- c("daily_discharge", "daily_stage", "daily_rainfall")
  pieces <- lapply(daily_names[daily_names %in% names(x)], function(nm) x[[nm]])
  pieces <- pieces[vapply(pieces, ana_hydro_ref_is_daily_frame, logical(1))]
  ana_hydro_ref_bind_rows(pieces)
}

ana_hydro_ref_extract_component <- function(x, component) {
  if (!is.list(x) || is.data.frame(x)) {
    return(data.frame())
  }
  if ("results" %in% names(x) && is.list(x$results)) {
    result_names <- names(x$results)
    pieces <- vector("list", length(x$results))
    for (i in seq_along(x$results)) {
      piece <- ana_hydro_ref_extract_component(x$results[[i]], component)
      if (length(result_names) >= i && !is.na(result_names[[i]]) && nzchar(result_names[[i]])) {
        piece <- ana_hydro_ref_add_station_if_missing(piece, result_names[[i]])
      }
      pieces[[i]] <- piece
    }
    return(ana_hydro_ref_bind_rows(pieces))
  }
  component_names <- switch(
    component,
    measurements = c("discharge_measurements", "measurements"),
    rating_curves = c("rating_curves", "curves"),
    rating_curve_summary = c("rating_curve_summary", "curve_summary"),
    character(0)
  )
  value <- ana_hydro_ref_get_component(x, component_names)
  if (is.data.frame(value)) {
    return(as.data.frame(value, stringsAsFactors = FALSE))
  }
  data.frame()
}

ana_hydro_ref_filter_station <- function(x, station_code = NULL) {
  if (is.null(station_code) || !is.data.frame(x) || !"station_code" %in% names(x)) {
    return(x)
  }
  station_code <- as.character(station_code)
  station_code <- station_code[!is.na(station_code) & nzchar(station_code)]
  if (length(station_code) == 0) {
    return(x)
  }
  x[as.character(x$station_code) %in% station_code, , drop = FALSE]
}

ana_hydro_ref_station_values <- function(...) {
  objects <- list(...)
  out <- character(0)
  for (obj in objects) {
    if (is.data.frame(obj) && "station_code" %in% names(obj) && nrow(obj) > 0) {
      values <- as.character(obj$station_code)
      out <- c(out, values[!is.na(values) & nzchar(values)])
    }
  }
  unique(out)
}

ana_hydro_ref_get_internal_data <- function(name) {
  ns <- environment(ana_hydro_ref_get_internal_data)
  if (!exists(name, envir = ns, inherits = TRUE)) {
    return(data.frame())
  }
  value <- get(name, envir = ns, inherits = TRUE)
  if (!is.data.frame(value)) {
    return(data.frame())
  }
  as.data.frame(value, stringsAsFactors = FALSE)
}

ana_hydro_ref_internal_product <- function(product, station_code = NULL) {
  object_name <- switch(
    product,
    measurements = "ana_discharge_measurements",
    rating_curves = "ana_rating_curves",
    rating_curve_summary = "ana_rating_curve_summary",
    NA_character_
  )
  if (is.na(object_name)) {
    return(data.frame())
  }
  out <- ana_hydro_ref_get_internal_data(object_name)
  ana_hydro_ref_filter_station(out, station_code)
}

ana_hydro_ref_pick_product <- function(product,
                                       station_code = NULL,
                                       user_value = NULL,
                                       downloaded_value = NULL,
                                       use_internal_database = TRUE) {
  user_value <- ana_hydro_ref_filter_station(user_value, station_code)
  if (ana_hydro_ref_is_nonempty_data_frame(user_value)) {
    return(list(data = user_value, source = "user_supplied", snapshot = NA_character_))
  }
  downloaded_value <- ana_hydro_ref_filter_station(downloaded_value, station_code)
  if (ana_hydro_ref_is_nonempty_data_frame(downloaded_value)) {
    return(list(data = downloaded_value, source = "downloaded_object", snapshot = NA_character_))
  }
  if (isTRUE(use_internal_database)) {
    internal_value <- ana_hydro_ref_internal_product(product, station_code)
    if (ana_hydro_ref_is_nonempty_data_frame(internal_value)) {
      return(list(data = internal_value, source = "internal_database", snapshot = "2026-06"))
    }
  }
  list(data = data.frame(), source = "not_available", snapshot = NA_character_)
}

ana_hydro_ref_overall_source <- function(details) {
  if (!is.data.frame(details) || nrow(details) == 0) {
    return("not_available")
  }
  active <- details[details$n_rows > 0, , drop = FALSE]
  if (nrow(active) == 0) {
    return("not_available")
  }
  if (any(active$source == "user_supplied")) {
    return("user_supplied")
  }
  if (any(active$source == "downloaded_object")) {
    return("downloaded_object")
  }
  if (any(active$source == "internal_database")) {
    return("internal_database")
  }
  "not_available"
}

ana_hydro_ref_resolve <- function(x = NULL,
                                  station_code = NULL,
                                  measurements = data.frame(),
                                  rating_curves = data.frame(),
                                  rating_curve_summary = data.frame(),
                                  use_internal_database = TRUE) {
  downloaded_measurements <- ana_hydro_ref_extract_component(x, "measurements")
  downloaded_rating_curves <- ana_hydro_ref_extract_component(x, "rating_curves")
  downloaded_rating_curve_summary <- ana_hydro_ref_extract_component(x, "rating_curve_summary")
  if (is.null(station_code)) {
    station_code <- ana_hydro_ref_station_values(
      measurements,
      rating_curves,
      rating_curve_summary,
      downloaded_measurements,
      downloaded_rating_curves,
      downloaded_rating_curve_summary,
      ana_hydro_ref_extract_daily_data(x)
    )
  }
  picked_measurements <- ana_hydro_ref_pick_product(
    "measurements",
    station_code = station_code,
    user_value = measurements,
    downloaded_value = downloaded_measurements,
    use_internal_database = use_internal_database
  )
  picked_rating_curves <- ana_hydro_ref_pick_product(
    "rating_curves",
    station_code = station_code,
    user_value = rating_curves,
    downloaded_value = downloaded_rating_curves,
    use_internal_database = use_internal_database
  )
  picked_rating_curve_summary <- ana_hydro_ref_pick_product(
    "rating_curve_summary",
    station_code = station_code,
    user_value = rating_curve_summary,
    downloaded_value = downloaded_rating_curve_summary,
    use_internal_database = use_internal_database
  )
  details <- data.frame(
    product = c("discharge_measurements", "rating_curves", "rating_curve_summary"),
    source = c(picked_measurements$source, picked_rating_curves$source, picked_rating_curve_summary$source),
    snapshot = c(picked_measurements$snapshot, picked_rating_curves$snapshot, picked_rating_curve_summary$snapshot),
    n_rows = c(nrow(picked_measurements$data), nrow(picked_rating_curves$data), nrow(picked_rating_curve_summary$data)),
    stringsAsFactors = FALSE
  )
  list(
    measurements = picked_measurements$data,
    rating_curves = picked_rating_curves$data,
    rating_curve_summary = picked_rating_curve_summary$data,
    source = ana_hydro_ref_overall_source(details),
    snapshot = if (any(details$source == "internal_database" & details$n_rows > 0)) "2026-06" else NA_character_,
    details = details
  )
}

ana_hydro_ref_measurement_year_summary <- function(measurements) {
  m <- ana_diag_standardize_measurements(measurements)
  if (nrow(m) == 0) {
    return(data.frame())
  }
  m <- m[!is.na(m$measurement_year), , drop = FALSE]
  if (nrow(m) == 0) {
    return(data.frame())
  }
  groups <- split(m, paste(m$station_code, m$measurement_year, sep = "\r"), drop = TRUE)
  pieces <- lapply(groups, function(x) {
    data.frame(
      station_code = x$station_code[[1]],
      measurement_year = x$measurement_year[[1]],
      n_measurements = nrow(x),
      n_valid_stage_measurements = sum(is.finite(x$stage_cm)),
      n_valid_discharge_measurements = sum(is.finite(x$discharge_m3s)),
      min_measured_stage_cm = ana_diag_safe_min(x$stage_cm),
      max_measured_stage_cm = ana_diag_safe_max(x$stage_cm),
      min_measured_discharge_m3s = ana_diag_safe_min(x$discharge_m3s),
      max_measured_discharge_m3s = ana_diag_safe_max(x$discharge_m3s),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  out <- out[order(out$station_code, out$measurement_year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

ana_hydro_ref_summary <- function(station_code = NULL, reference) {
  m <- ana_diag_standardize_measurements(reference$measurements)
  rc <- ana_diag_standardize_rating_curves(reference$rating_curves)
  rcs <- as.data.frame(reference$rating_curve_summary, stringsAsFactors = FALSE)
  stations <- unique(c(
    as.character(station_code),
    ana_hydro_ref_station_values(m, rc, rcs)
  ))
  stations <- stations[!is.na(stations) & nzchar(stations)]
  if (length(stations) == 0) {
    stations <- NA_character_
  }
  pieces <- lapply(stations, function(station) {
    sm <- ana_hydro_ref_filter_station(m, station)
    src <- ana_hydro_ref_filter_station(rc, station)
    ssum <- ana_hydro_ref_filter_station(rcs, station)
    data.frame(
      station_code = station,
      hydrometry_reference_source = reference$source,
      hydrometry_reference_snapshot = reference$snapshot,
      n_reference_measurements = nrow(sm),
      first_reference_measurement_date = ana_daily_hydro_min_date(sm$measurement_date),
      last_reference_measurement_date = ana_daily_hydro_max_date(sm$measurement_date),
      n_reference_measurement_years = length(unique(sm$measurement_year[!is.na(sm$measurement_year)])),
      min_reference_measured_stage_cm = ana_diag_safe_min(sm$stage_cm),
      max_reference_measured_stage_cm = ana_diag_safe_max(sm$stage_cm),
      min_reference_measured_discharge_m3s = ana_diag_safe_min(sm$discharge_m3s),
      max_reference_measured_discharge_m3s = ana_diag_safe_max(sm$discharge_m3s),
      n_reference_rating_curves = if (nrow(src) > 0) length(unique(src$rating_curve_id)) else if (nrow(ssum) > 0 && "rating_curve_id" %in% names(ssum)) length(unique(ssum$rating_curve_id)) else 0L,
      n_reference_rating_curve_segments = if (nrow(src) > 0) length(unique(src$rating_curve_segment_id)) else 0L,
      first_reference_rating_curve_date = ana_daily_hydro_min_date(src$valid_from),
      last_reference_rating_curve_date = ana_daily_hydro_max_date(src$valid_to),
      min_reference_rating_stage_cm = ana_diag_safe_min(src$stage_min_cm),
      max_reference_rating_stage_cm = ana_diag_safe_max(src$stage_max_cm),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

ana_hydro_ref_add_summary_columns <- function(summary, reference_summary) {
  if (!is.data.frame(summary) || nrow(summary) == 0 ||
      !is.data.frame(reference_summary) || nrow(reference_summary) == 0 ||
      !"station_code" %in% names(summary) || !"station_code" %in% names(reference_summary)) {
    return(summary)
  }
  merge(summary, reference_summary, by = "station_code", all.x = TRUE, sort = FALSE)
}

#' Diagnosticar consistência diária entre vazão, cota e curva-chave
#'
#' Compara séries diárias de vazão e cota com os segmentos de curva-chave
#' disponíveis. A função identifica lacunas cruzadas entre vazão e cota, valores
#' não positivos, períodos sem curva aplicável, cotas fora da faixa de validade
#' da curva e diferenças entre a vazão observada e a vazão estimada pela
#' curva-chave.
#'
#' @param daily_data Série diária no contrato padronizado do pacote. Deve conter
#'   pelo menos `station_code`, `date`, `variable` e `value`. Linhas com
#'   `variable = "discharge"` são tratadas como vazão em m3/s; linhas com
#'   `variable = "stage"` são tratadas como cota em cm.
#' @param rating_curves Tabela de curvas-chave com datas de validade, limites
#'   de cota e coeficientes da equação. A coluna `coefficient_h0_m` também é
#'   aceita por compatibilidade.
#' @param relative_error_threshold_pct Limiar absoluto, em porcentagem, para
#'   sinalizar diferença elevada entre vazão observada e vazão calculada pela
#'   curva-chave.
#' @param measurements Tabela opcional de medições de descarga. Quando ausente,
#'   a função tenta usar medições presentes em objetos agregados e, por fim, a
#'   base hidrométrica interna do pacote.
#' @param rating_curve_summary Tabela opcional de resumo das curvas-chave.
#' @param station_code Código(s) de estação a filtrar.
#' @param use_internal_database Se `TRUE`, usa a base hidrométrica interna do
#'   pacote quando não houver referência fornecida pelo usuário nem no objeto
#'   de aquisição.
#'
#' @details
#' Quando medições de descarga ou curvas-chave não são fornecidas pelo usuário
#' nem estão presentes em um objeto agregado de `get_ana_data(product = "all")`,
#' a função consulta a base hidrométrica interna do pacote, correspondente a um
#' retrato da ANA de junho de 2026. O objeto retornado informa a origem da
#' referência hidrométrica usada.
#'
#' @return Lista com as tabelas `summary`, `indices`, `daily_flags`,
#'   `rating_matches`, `rating_curve_coverage`, `measurement_year_summary`,
#'   `hydrometry_reference_summary` e metadados da referência usada.
#' @export
#'
#' @examples
#' daily_data <- data.frame(
#'   station_code = "001",
#'   date = as.Date(c("2020-01-01", "2020-01-01")),
#'   variable = c("discharge", "stage"),
#'   value = c(10, 100)
#' )
#'
#' rating_curves <- data.frame(
#'   station_code = "001",
#'   valid_from = as.Date("2019-01-01"),
#'   valid_to = as.Date("2021-12-31"),
#'   stage_min_cm = 50,
#'   stage_max_cm = 200,
#'   coefficient_a = 10,
#'   coefficient_h0_cm = 0,
#'   coefficient_n = 1
#' )
#'
#' diagnostico <- diagnose_daily_hydrometry(daily_data, rating_curves)
#' diagnostico$summary
diagnose_daily_hydrometry <- function(
    daily_data,
    rating_curves = data.frame(),
    relative_error_threshold_pct = 20,
    measurements = data.frame(),
    rating_curve_summary = data.frame(),
    station_code = NULL,
    use_internal_database = TRUE) {
  raw_input <- daily_data
  daily <- ana_daily_hydro_standardize_daily_data(
    ana_hydro_ref_extract_daily_data(daily_data)
  )
  if (!is.null(station_code)) {
    daily <- daily[as.character(daily$station_code) %in% as.character(station_code), , drop = FALSE]
    if (nrow(daily) == 0) {
      stop("No standardized daily data found for the requested station.", call. = FALSE)
    }
  }
  reference <- ana_hydro_ref_resolve(
    x = raw_input,
    station_code = unique(daily$station_code),
    measurements = measurements,
    rating_curves = rating_curves,
    rating_curve_summary = rating_curve_summary,
    use_internal_database = use_internal_database
  )
  curves <- ana_diag_standardize_rating_curves(reference$rating_curves)
  curve_segments <- ana_diag_make_curve_segment_metadata(curves)
  expected <- ana_daily_hydro_make_expected_days(daily)
  values <- ana_daily_hydro_make_daily_values(daily)
  flags <- merge(expected, values, by = c("station_code", "date"), all.x = TRUE, sort = FALSE)
  flags <- ana_daily_hydro_complete_value_columns(flags)
  coverage <- ana_daily_hydro_make_curve_coverage(flags, curve_segments)
  matches <- ana_daily_hydro_match_rating_curves(flags, curve_segments)
  selected <- ana_daily_hydro_select_rating_match(matches)
  flags <- merge(flags, coverage, by = c("station_code", "date"), all.x = TRUE, sort = FALSE)
  flags <- merge(flags, selected, by = c("station_code", "date"), all.x = TRUE, sort = FALSE)
  flags <- ana_daily_hydro_complete_match_columns(flags)
  flags <- ana_daily_hydro_add_flags(flags, relative_error_threshold_pct)
  reference_summary <- ana_hydro_ref_summary(unique(daily$station_code), reference)
  measurement_year_summary <- ana_hydro_ref_measurement_year_summary(reference$measurements)
  flags$hydrometry_reference_source <- reference$source
  flags$hydrometry_reference_snapshot <- reference$snapshot
  summary <- ana_daily_hydro_make_summary(flags, relative_error_threshold_pct)
  summary <- ana_hydro_ref_add_summary_columns(summary, reference_summary)
  indices <- ana_daily_hydro_make_indices(summary)
  rating_curve_coverage <- ana_daily_hydro_make_rating_coverage_summary(flags)
  rating_curve_coverage <- ana_hydro_ref_add_summary_columns(rating_curve_coverage, reference_summary)
  list(
    summary = summary,
    indices = indices,
    daily_flags = flags,
    rating_matches = matches,
    rating_curve_coverage = rating_curve_coverage,
    measurement_year_summary = measurement_year_summary,
    hydrometry_reference_summary = reference_summary,
    hydrometry_reference_details = reference$details,
    hydrometry_reference_source = reference$source,
    hydrometry_reference_snapshot = reference$snapshot
  )
}
ana_daily_hydro_standardize_daily_data <- function(daily_data) {
  if (is.null(daily_data) || !is.data.frame(daily_data)) {
    stop("`daily_data` must be a data frame.", call. = FALSE)
  }
  required <- c("station_code", "date", "variable", "value")
  missing_cols <- setdiff(required, names(daily_data))
  if (length(missing_cols) > 0) {
    stop(
      "`daily_data` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  out <- as.data.frame(daily_data, stringsAsFactors = FALSE)
  out$station_code <- as.character(out$station_code)
  out$date <- as.Date(out$date)
  out$variable <- as.character(out$variable)
  out$value <- suppressWarnings(as.numeric(out$value))
  keep <- out$variable %in% c("discharge", "stage")
  out <- out[keep & !is.na(out$station_code) & !is.na(out$date), , drop = FALSE]
  if (nrow(out) == 0) {
    stop("`daily_data` must contain at least one discharge or stage record.", call. = FALSE)
  }
  out[order(out$station_code, out$date, out$variable), , drop = FALSE]
}
ana_daily_hydro_make_expected_days <- function(daily) {
  stations <- unique(daily$station_code)
  pieces <- lapply(stations, function(station) {
    x <- daily[daily$station_code == station, , drop = FALSE]
    start_date <- min(x$date, na.rm = TRUE)
    end_date <- max(x$date, na.rm = TRUE)
    data.frame(
      station_code = station,
      date = seq(start_date, end_date, by = "day"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}
ana_daily_hydro_make_daily_values <- function(daily) {
  keys <- unique(paste(daily$station_code, daily$date, sep = "\r"))
  pieces <- lapply(keys, function(key) {
    x <- daily[paste(daily$station_code, daily$date, sep = "\r") == key, , drop = FALSE]
    discharge <- x[x$variable == "discharge", , drop = FALSE]
    stage <- x[x$variable == "stage", , drop = FALSE]
    data.frame(
      station_code = x$station_code[1],
      date = x$date[1],
      discharge_m3s = ana_daily_hydro_mean_or_na(discharge$value),
      stage_cm = ana_daily_hydro_mean_or_na(stage$value),
      discharge_records = nrow(discharge),
      stage_records = nrow(stage),
      valid_discharge_records = sum(!is.na(discharge$value) & is.finite(discharge$value)),
      valid_stage_records = sum(!is.na(stage$value) & is.finite(stage$value)),
      duplicate_discharge_records = nrow(discharge) > 1,
      duplicate_stage_records = nrow(stage) > 1,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  out[order(out$station_code, out$date), , drop = FALSE]
}
ana_daily_hydro_mean_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}
ana_daily_hydro_complete_value_columns <- function(flags) {
  value_cols <- c("discharge_m3s", "stage_cm")
  count_cols <- c("discharge_records", "stage_records", "valid_discharge_records", "valid_stage_records")
  logical_cols <- c("duplicate_discharge_records", "duplicate_stage_records")
  for (col in value_cols) {
    if (!col %in% names(flags)) flags[[col]] <- NA_real_
  }
  for (col in count_cols) {
    if (!col %in% names(flags)) flags[[col]] <- 0L
    flags[[col]][is.na(flags[[col]])] <- 0L
    flags[[col]] <- as.integer(flags[[col]])
  }
  for (col in logical_cols) {
    if (!col %in% names(flags)) flags[[col]] <- FALSE
    flags[[col]][is.na(flags[[col]])] <- FALSE
  }
  flags$has_discharge_record <- flags$discharge_records > 0
  flags$has_stage_record <- flags$stage_records > 0
  flags$has_valid_discharge <- !is.na(flags$discharge_m3s) & is.finite(flags$discharge_m3s)
  flags$has_valid_stage <- !is.na(flags$stage_cm) & is.finite(flags$stage_cm)
  flags
}
ana_daily_hydro_make_curve_coverage <- function(flags, curve_segments) {
  base <- data.frame(
    station_code = flags$station_code,
    date = flags$date,
    n_date_valid_rating_segments = 0L,
    first_rating_curve_date = as.Date(NA),
    last_rating_curve_date = as.Date(NA),
    has_open_ended_rating_curve = FALSE,
    before_first_rating_curve = FALSE,
    after_last_rating_curve = FALSE,
    stage_min_date_valid_cm = NA_real_,
    stage_max_date_valid_cm = NA_real_,
    stringsAsFactors = FALSE
  )
  if (is.null(curve_segments) || nrow(curve_segments) == 0) return(base)
  curve_segments <- ana_daily_hydro_prepare_curve_segments(curve_segments)
  stations <- unique(base$station_code)
  pieces <- lapply(stations, function(station) {
    out <- base[base$station_code == station, , drop = FALSE]
    curves <- curve_segments[curve_segments$station_code == station, , drop = FALSE]
    if (nrow(curves) == 0) return(out)
    first_date <- ana_daily_hydro_min_date(curves$valid_from)
    last_date <- ana_daily_hydro_max_date(curves$valid_to)
    has_open <- any(is.na(curves$valid_to))
    out$first_rating_curve_date <- first_date
    out$last_rating_curve_date <- last_date
    out$has_open_ended_rating_curve <- has_open
    out$before_first_rating_curve <- !is.na(first_date) & out$date < first_date
    out$after_last_rating_curve <- !has_open & !is.na(last_date) & out$date > last_date
    for (i in seq_len(nrow(out))) {
      d <- out$date[i]
      valid <- curves[!is.na(curves$valid_from) & curves$valid_from <= d &
        (is.na(curves$valid_to) | curves$valid_to >= d), , drop = FALSE]
      out$n_date_valid_rating_segments[i] <- nrow(valid)
      if (nrow(valid) > 0) {
        out$stage_min_date_valid_cm[i] <- min(valid$stage_min_cm, na.rm = TRUE)
        out$stage_max_date_valid_cm[i] <- max(valid$stage_max_cm, na.rm = TRUE)
      }
    }
    out
  })
  do.call(rbind, pieces)
}
ana_daily_hydro_prepare_curve_segments <- function(curve_segments) {
  out <- as.data.frame(curve_segments, stringsAsFactors = FALSE)
  if (nrow(out) == 0) return(out)
  out$station_code <- as.character(out$station_code)
  out$valid_from <- as.Date(out$valid_from)
  out$valid_to <- as.Date(out$valid_to)
  out$stage_min_cm <- suppressWarnings(as.numeric(out$stage_min_cm))
  out$stage_max_cm <- suppressWarnings(as.numeric(out$stage_max_cm))
  out$coefficient_a <- suppressWarnings(as.numeric(out$coefficient_a))
  out$coefficient_h0_m <- suppressWarnings(as.numeric(out$coefficient_h0_m))
  out$coefficient_n <- suppressWarnings(as.numeric(out$coefficient_n))
  out
}
ana_daily_hydro_match_rating_curves <- function(flags, curve_segments) {
  if (is.null(curve_segments) || nrow(curve_segments) == 0) return(data.frame())
  curve_segments <- ana_daily_hydro_prepare_curve_segments(curve_segments)
  rows <- flags[flags$has_valid_stage, , drop = FALSE]
  if (nrow(rows) == 0) return(data.frame())
  pieces <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    curves <- curve_segments[curve_segments$station_code == row$station_code, , drop = FALSE]
    if (nrow(curves) == 0) return(data.frame())
    curves <- curves[!is.na(curves$valid_from) & curves$valid_from <= row$date &
      (is.na(curves$valid_to) | curves$valid_to >= row$date) &
      row$stage_cm >= curves$stage_min_cm &
      row$stage_cm <= curves$stage_max_cm, , drop = FALSE]
    if (nrow(curves) == 0) return(data.frame())
    q_hat <- ana_diag_predict_rating_discharge(
      stage_cm = rep(row$stage_cm, nrow(curves)),
      coefficient_a = curves$coefficient_a,
      coefficient_h0_m = curves$coefficient_h0_m,
      coefficient_n = curves$coefficient_n
    )
    relative_error <- rep(NA_real_, nrow(curves))
    log_residual <- rep(NA_real_, nrow(curves))
    has_q <- !is.na(q_hat) & is.finite(q_hat) & q_hat > 0
    has_obs <- isTRUE(row$has_valid_discharge) && !is.na(row$discharge_m3s) &&
      is.finite(row$discharge_m3s) && row$discharge_m3s > 0
    if (has_obs) {
      relative_error[has_q] <- 100 * (row$discharge_m3s - q_hat[has_q]) / q_hat[has_q]
      log_residual[has_q] <- log(row$discharge_m3s) - log(q_hat[has_q])
    }
    data.frame(
      station_code = row$station_code,
      date = row$date,
      stage_cm = row$stage_cm,
      discharge_m3s = row$discharge_m3s,
      rating_curve_id = curves$rating_curve_id,
      rating_curve_segment_id = curves$rating_curve_segment_id,
      segment_number = curves$segment_number,
      valid_from = curves$valid_from,
      valid_to = curves$valid_to,
      stage_min_cm = curves$stage_min_cm,
      stage_max_cm = curves$stage_max_cm,
      coefficient_a = curves$coefficient_a,
      coefficient_h0_cm = curves$coefficient_h0_cm,
      coefficient_n = curves$coefficient_n,
      generated_discharge_m3s = q_hat,
      relative_error_pct = relative_error,
      abs_relative_error_pct = abs(relative_error),
      log_residual = log_residual,
      mathematical_error = !has_q,
      stringsAsFactors = FALSE
    )
  })
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0]
  if (length(pieces) == 0) return(data.frame())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
ana_daily_hydro_select_rating_match <- function(matches) {
  if (is.null(matches) || nrow(matches) == 0) {
    return(data.frame(
      station_code = character(),
      date = as.Date(character()),
      n_applicable_rating_segments = integer(),
      selected_rating_curve_id = character(),
      selected_rating_curve_segment_id = character(),
      selected_segment_number = integer(),
      generated_discharge_m3s = numeric(),
      relative_error_pct = numeric(),
      abs_relative_error_pct = numeric(),
      log_residual = numeric(),
      mathematical_error = logical(),
      stringsAsFactors = FALSE
    ))
  }
  keys <- unique(paste(matches$station_code, matches$date, sep = "\r"))
  pieces <- lapply(keys, function(key) {
    x <- matches[paste(matches$station_code, matches$date, sep = "\r") == key, , drop = FALSE]
    rank_value <- x$abs_relative_error_pct
    rank_value[is.na(rank_value)] <- Inf
    x <- x[order(rank_value, x$rating_curve_segment_id), , drop = FALSE]
    best <- x[1, , drop = FALSE]
    data.frame(
      station_code = best$station_code,
      date = best$date,
      n_applicable_rating_segments = nrow(x),
      selected_rating_curve_id = best$rating_curve_id,
      selected_rating_curve_segment_id = best$rating_curve_segment_id,
      selected_segment_number = best$segment_number,
      generated_discharge_m3s = best$generated_discharge_m3s,
      relative_error_pct = best$relative_error_pct,
      abs_relative_error_pct = best$abs_relative_error_pct,
      log_residual = best$log_residual,
      mathematical_error = best$mathematical_error,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
ana_daily_hydro_complete_match_columns <- function(flags) {
  int_cols <- c("n_date_valid_rating_segments", "n_applicable_rating_segments")
  for (col in int_cols) {
    if (!col %in% names(flags)) flags[[col]] <- 0L
    flags[[col]][is.na(flags[[col]])] <- 0L
    flags[[col]] <- as.integer(flags[[col]])
  }
  char_cols <- c("selected_rating_curve_id", "selected_rating_curve_segment_id")
  for (col in char_cols) {
    if (!col %in% names(flags)) flags[[col]] <- NA_character_
  }
  num_cols <- c(
    "selected_segment_number", "generated_discharge_m3s", "relative_error_pct",
    "abs_relative_error_pct", "log_residual", "stage_min_date_valid_cm",
    "stage_max_date_valid_cm"
  )
  for (col in num_cols) {
    if (!col %in% names(flags)) flags[[col]] <- NA_real_
  }
  log_cols <- c("mathematical_error", "has_open_ended_rating_curve", "before_first_rating_curve", "after_last_rating_curve")
  for (col in log_cols) {
    if (!col %in% names(flags)) flags[[col]] <- FALSE
    flags[[col]][is.na(flags[[col]])] <- FALSE
  }
  flags
}
ana_daily_hydro_add_flags <- function(flags, relative_error_threshold_pct) {
  flags$missing_discharge <- !flags$has_valid_discharge
  flags$missing_stage <- !flags$has_valid_stage
  flags$discharge_without_stage <- flags$has_valid_discharge & !flags$has_valid_stage
  flags$stage_without_discharge <- flags$has_valid_stage & !flags$has_valid_discharge
  flags$both_missing <- !flags$has_valid_discharge & !flags$has_valid_stage
  flags$non_positive_discharge <- flags$has_valid_discharge & flags$discharge_m3s <= 0
  flags$non_positive_stage <- flags$has_valid_stage & flags$stage_cm <= 0
  flags$has_rating_curve_date_coverage <- flags$n_date_valid_rating_segments > 0
  flags$has_applicable_rating_segment <- flags$n_applicable_rating_segments > 0
  flags$multiple_applicable_rating_segments <- flags$n_applicable_rating_segments > 1
  flags$has_generated_discharge <- !is.na(flags$generated_discharge_m3s) &
    is.finite(flags$generated_discharge_m3s) & flags$generated_discharge_m3s > 0
  flags$no_rating_curve_for_date <- flags$has_valid_stage & !flags$has_rating_curve_date_coverage
  flags$no_applicable_rating_segment <- flags$has_valid_stage &
    flags$has_rating_curve_date_coverage & !flags$has_applicable_rating_segment
  flags$stage_below_curve_range <- flags$has_valid_stage & flags$has_rating_curve_date_coverage &
    !is.na(flags$stage_min_date_valid_cm) & flags$stage_cm < flags$stage_min_date_valid_cm
  flags$stage_above_curve_range <- flags$has_valid_stage & flags$has_rating_curve_date_coverage &
    !is.na(flags$stage_max_date_valid_cm) & flags$stage_cm > flags$stage_max_date_valid_cm
  flags$stage_outside_curve_range <- flags$stage_below_curve_range | flags$stage_above_curve_range
  flags$relative_error_exceeds_threshold <- !is.na(flags$abs_relative_error_pct) &
    flags$abs_relative_error_pct > relative_error_threshold_pct
  flags$diagnostic_problem <- flags$discharge_without_stage |
    flags$stage_without_discharge |
    flags$non_positive_discharge |
    flags$non_positive_stage |
    flags$no_rating_curve_for_date |
    flags$no_applicable_rating_segment |
    flags$multiple_applicable_rating_segments |
    flags$mathematical_error |
    flags$relative_error_exceeds_threshold
  flags[order(flags$station_code, flags$date), , drop = FALSE]
}
ana_daily_hydro_make_summary <- function(flags, relative_error_threshold_pct) {
  stations <- unique(flags$station_code)
  pieces <- lapply(stations, function(station) {
    x <- flags[flags$station_code == station, , drop = FALSE]
    data.frame(
      station_code = station,
      start_date = min(x$date, na.rm = TRUE),
      end_date = max(x$date, na.rm = TRUE),
      expected_days = nrow(x),
      discharge_observed_days = sum(x$has_discharge_record),
      stage_observed_days = sum(x$has_stage_record),
      discharge_valid_days = sum(x$has_valid_discharge),
      stage_valid_days = sum(x$has_valid_stage),
      min_daily_discharge_m3s = ana_diag_safe_min(x$discharge_m3s),
      max_daily_discharge_m3s = ana_diag_safe_max(x$discharge_m3s),
      min_daily_stage_cm = ana_diag_safe_min(x$stage_cm),
      max_daily_stage_cm = ana_diag_safe_max(x$stage_cm),
      discharge_without_stage_days = sum(x$discharge_without_stage),
      stage_without_discharge_days = sum(x$stage_without_discharge),
      both_missing_days = sum(x$both_missing),
      non_positive_discharge_days = sum(x$non_positive_discharge),
      non_positive_stage_days = sum(x$non_positive_stage),
      days_with_rating_curve_date_coverage = sum(x$has_valid_stage & x$has_rating_curve_date_coverage),
      days_without_rating_curve_date_coverage = sum(x$no_rating_curve_for_date),
      days_without_applicable_rating_segment = sum(x$no_applicable_rating_segment),
      days_with_stage_outside_curve_range = sum(x$stage_outside_curve_range),
      days_with_multiple_applicable_segments = sum(x$multiple_applicable_rating_segments),
      days_with_generated_discharge = sum(x$has_generated_discharge),
      days_with_relative_error = sum(!is.na(x$relative_error_pct)),
      relative_error_threshold_pct = relative_error_threshold_pct,
      days_exceeding_relative_error_threshold = sum(x$relative_error_exceeds_threshold),
      mean_abs_relative_error_pct = ana_daily_hydro_mean_or_na(x$abs_relative_error_pct),
      median_abs_relative_error_pct = ana_daily_hydro_median_or_na(x$abs_relative_error_pct),
      diagnostic_problem_days = sum(x$diagnostic_problem),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
ana_daily_hydro_make_indices <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) return(data.frame())
  metric_cols <- setdiff(names(summary), c("station_code", "start_date", "end_date"))
  numeric_cols <- vapply(
    summary[, metric_cols, drop = FALSE],
    function(x) is.numeric(x) || is.integer(x) || is.logical(x),
    logical(1)
  )
  metric_cols <- metric_cols[numeric_cols]
  if (length(metric_cols) == 0) {
    return(data.frame())
  }
  pieces <- lapply(seq_len(nrow(summary)), function(i) {
    row <- summary[i, , drop = FALSE]
    data.frame(
      station_code = row$station_code,
      metric = metric_cols,
      value = as.numeric(row[1, metric_cols]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
ana_daily_hydro_make_rating_coverage_summary <- function(flags) {
  stations <- unique(flags$station_code)
  pieces <- lapply(stations, function(station) {
    x <- flags[flags$station_code == station, , drop = FALSE]
    data.frame(
      station_code = station,
      first_rating_curve_date = ana_daily_hydro_min_date(x$first_rating_curve_date),
      last_rating_curve_date = ana_daily_hydro_max_date(x$last_rating_curve_date),
      has_open_ended_rating_curve = any(x$has_open_ended_rating_curve),
      days_before_first_rating_curve = sum(x$before_first_rating_curve & x$has_valid_stage),
      days_after_last_rating_curve = sum(x$after_last_rating_curve & x$has_valid_stage),
      days_with_date_valid_rating_segments = sum(x$has_valid_stage & x$n_date_valid_rating_segments > 0),
      days_with_applicable_rating_segments = sum(x$has_valid_stage & x$n_applicable_rating_segments > 0),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
ana_daily_hydro_median_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  stats::median(x)
}
ana_daily_hydro_min_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  min(x)
}
ana_daily_hydro_max_date <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.Date(NA))
  max(x)
}
# Station-level hydrometry diagnostics.
#' Diagnosticar medições de descarga e curvas-chave
#'
#' Avalia a consistência geral entre medições de descarga e curvas-chave de uma
#' estação. A função calcula indicadores de qualidade, verifica valores nulos ou
#' negativos, identifica grupos repetidos, pareia medições com segmentos de
#' curva-chave e, quando solicitado, calcula resíduos e envelopes empíricos.
#'
#' @param measurements Tabela de medições de descarga, ou objeto agregado
#'   retornado por `get_ana_data(product = "all")` contendo o elemento
#'   `discharge_measurements`.
#' @param rating_curves Tabela de curvas-chave. Se `measurements` for um objeto
#'   agregado e este argumento estiver vazio, a função tenta usar o elemento
#'   `rating_curves` do próprio objeto.
#' @param params Lista opcional de parâmetros diagnósticos. Valores omitidos
#'   usam os padrões internos.
#' @param detailed Se `TRUE`, calcula pareamento, resíduos, envelopes e triagem
#'   temporal. Se `FALSE`, calcula apenas métricas leves.
#' @param rating_curve_summary Tabela opcional de resumo das curvas-chave.
#' @param station_code Código(s) de estação a filtrar. Também permite consultar
#'   a base interna quando `measurements` estiver vazio.
#' @param use_internal_database Se `TRUE`, usa a base hidrométrica interna do
#'   pacote quando não houver referência fornecida pelo usuário nem no objeto
#'   de aquisição.
#'
#' @details
#' Quando medições de descarga ou curvas-chave não são fornecidas pelo usuário
#' nem estão presentes em um objeto agregado de `get_ana_data(product = "all")`,
#' a função consulta a base hidrométrica interna do pacote, correspondente a um
#' retrato da ANA de junho de 2026. O objeto retornado informa a origem da
#' referência hidrométrica usada.
#'
#' Os resultados são exploratórios e servem para apoiar revisão hidrométrica.
#' Eles não substituem a avaliação técnica de consistência, nem a revisão
#' especializada de uma curva-chave.
#'
#' @return Lista com tabelas de resumo, índices, flags de medições, metadados
#'   das curvas, pareamentos e resultados de resíduos quando `detailed = TRUE`.
#' @importFrom stats ave median
#' @export
#'
#' @examples
#' measurements <- data.frame(
#'   station_code = "001",
#'   measurement_date = as.Date(c("2020-01-01", "2020-02-01")),
#'   stage_cm = c(100, 120),
#'   discharge_m3s = c(10, 15)
#' )
#'
#' curves <- data.frame(
#'   station_code = "001",
#'   rating_curve_id = "rc1",
#'   rating_curve_segment_id = "seg1",
#'   segment_number = 1,
#'   valid_from = as.Date("2019-01-01"),
#'   valid_to = as.Date("2021-12-31"),
#'   stage_min_cm = 50,
#'   stage_max_cm = 200,
#'   coefficient_a = 10,
#'   coefficient_h0_cm = 0,
#'   coefficient_n = 1
#' )
#'
#' diagnose_station_hydrometry(measurements, curves, detailed = FALSE)$summary
diagnose_station_hydrometry <- function(
    measurements = data.frame(),
    rating_curves = data.frame(),
    params = NULL,
    detailed = TRUE,
    rating_curve_summary = data.frame(),
    station_code = NULL,
    use_internal_database = TRUE) {
  params <- ana_diag_merge_params(params)
  raw_input <- measurements
  explicit_measurements <- if (is.list(measurements) && !is.data.frame(measurements)) {
    data.frame()
  } else {
    measurements
  }
  reference <- ana_hydro_ref_resolve(
    x = raw_input,
    station_code = station_code,
    measurements = explicit_measurements,
    rating_curves = rating_curves,
    rating_curve_summary = rating_curve_summary,
    use_internal_database = use_internal_database
  )
  m <- ana_diag_standardize_measurements(reference$measurements)
  rc <- ana_diag_standardize_rating_curves(reference$rating_curves)
  measurement_flags <- ana_diag_make_measurement_flags(m, params)
  repeated_group_details <- ana_diag_make_repeated_value_group_details(measurement_flags)
  curve_metadata <- ana_diag_make_curve_metadata(rc)
  curve_segments <- ana_diag_make_curve_segment_metadata(rc, curve_metadata)
  rating_curve_points <- ana_diag_make_rating_curve_points(
    curve_segments,
    n_points = params$n_stage_points_per_segment
  )
  rating_matches <- data.frame()
  best_rating_match <- data.frame()
  residual_envelopes <- data.frame()
  residual_points <- data.frame()
  temporal_regime <- NULL
  power_curve_points <- data.frame()
  if (isTRUE(detailed)) {
    rating_matches <- ana_diag_match_measurements_to_rating_curves(
      measurement_flags,
      curve_segments
    )
    best_rating_match <- ana_diag_make_best_rating_match(rating_matches)
    residual_envelopes <- ana_diag_make_residual_envelopes(best_rating_match, params)
    residual_points <- ana_diag_add_envelope_flags(best_rating_match, residual_envelopes)
    temporal_regime <- ana_diag_fit_residual_temporal_regimes(
      measurement_flags,
      station_code_value = ana_diag_safe_first(measurement_flags$station_code),
      params = params
    )
    if (!is.null(temporal_regime$power_model) &&
        nrow(temporal_regime$power_model) > 0 &&
        nrow(measurement_flags) > 0) {
      valid_stage <- measurement_flags$stage_cm[
        !is.na(measurement_flags$stage_cm) & measurement_flags$stage_cm > 0
      ]
      if (length(valid_stage) > 1) {
        power_curve_points <- ana_diag_make_power_curve_points(
          temporal_regime$power_model,
          stage_min_cm = min(valid_stage),
          stage_max_cm = max(valid_stage),
          params = params
        )
      }
    }
  }
  summary <- ana_diag_make_diagnostic_summary(
    measurement_flags = measurement_flags,
    rating_curves = rc,
    best_matches = best_rating_match,
    residual_points = residual_points,
    temporal_regime = temporal_regime,
    detailed = detailed
  )
  if (!is.null(station_code) && "station_code" %in% names(summary) && all(is.na(summary$station_code))) {
    summary$station_code <- as.character(station_code[[1]])
  }
  reference_summary <- ana_hydro_ref_summary(station_code, reference)
  summary <- ana_hydro_ref_add_summary_columns(summary, reference_summary)
  measurement_year_summary <- ana_hydro_ref_measurement_year_summary(reference$measurements)
  if (nrow(measurement_flags) > 0) {
    measurement_flags$hydrometry_reference_source <- reference$source
    measurement_flags$hydrometry_reference_snapshot <- reference$snapshot
  }
  indices <- ana_diag_make_diagnostic_indices(summary)
  list(
    summary = summary,
    indices = indices,
    measurement_flags = measurement_flags,
    repeated_group_details = repeated_group_details,
    curve_metadata = curve_metadata,
    curve_segments = curve_segments,
    rating_curve_points = rating_curve_points,
    rating_matches = rating_matches,
    best_rating_match = best_rating_match,
    residual_envelopes = residual_envelopes,
    residual_points = residual_points,
    temporal_regime = temporal_regime,
    power_curve_points = power_curve_points,
    measurement_year_summary = measurement_year_summary,
    hydrometry_reference_summary = reference_summary,
    hydrometry_reference_details = reference$details,
    hydrometry_reference_source = reference$source,
    hydrometry_reference_snapshot = reference$snapshot
  )
}
ana_diag_default_params <- function() {
  list(
    stage_zero_tolerance_cm = 0,
    discharge_zero_tolerance_m3s = 0,
    min_repeated_group_size = 5,
    stage_group_round_digits = 2,
    discharge_group_round_digits = 3,
    min_stage_spread_cm_for_repeated_discharge = 5,
    min_abs_discharge_spread_m3s_for_repeated_stage = 0.1,
    min_rel_discharge_spread_for_repeated_stage = 0.10,
    min_residual_points_per_segment = 5,
    residual_envelope_sd_multiplier = 1.96,
    min_power_model_points = 30,
    n_h0_grid = 40,
    h0_min_offset_m = 0.005,
    h0_grid_span_multiplier = 2.0,
    h0_grid_min_span_m = 1.0,
    h0_grid_max_span_m = 20.0,
    min_power_exponent = 0.05,
    max_power_exponent = 10,
    min_regime_measurements = 30,
    max_temporal_regimes = 3,
    max_break_candidates = 25,
    min_regime_fraction = 0.15,
    min_regime_span_years = 4,
    min_log_residual_shift = log(1.25),
    residual_shift_mad_fraction = 0.75,
    min_break_gain = 0.12,
    min_incremental_gain_for_second_break = 0.06,
    n_stage_points_per_segment = 80,
    n_stage_points_power_curve = 120
  )
}
ana_diag_merge_params <- function(params) {
  defaults <- ana_diag_default_params()
  if (is.null(params)) return(defaults)
  if (!is.list(params)) stop("`params` must be NULL or a list.", call. = FALSE)
  defaults[names(params)] <- params
  defaults
}
ana_diag_empty_measurements <- function() {
  data.frame(
    station_code = character(),
    measurement_date = as.Date(character()),
    measurement_datetime = as.POSIXct(character()),
    measurement_year = integer(),
    stage_cm = numeric(),
    discharge_m3s = numeric(),
    stringsAsFactors = FALSE
  )
}
ana_diag_standardize_measurements <- function(measurements) {
  if (is.null(measurements) || !is.data.frame(measurements) || nrow(measurements) == 0) {
    return(ana_diag_empty_measurements())
  }
  m <- as.data.frame(measurements, stringsAsFactors = FALSE)
  if (!"station_code" %in% names(m)) m$station_code <- NA_character_
  m$station_code <- as.character(m$station_code)
  if ("measurement_date" %in% names(m)) {
    m$measurement_date <- as.Date(m$measurement_date)
  } else if ("measurement_datetime" %in% names(m)) {
    m$measurement_date <- as.Date(m$measurement_datetime)
  } else {
    m$measurement_date <- as.Date(NA)
  }
  if ("measurement_datetime" %in% names(m)) {
    m$measurement_datetime <- as.POSIXct(m$measurement_datetime)
  } else {
    m$measurement_datetime <- as.POSIXct(m$measurement_date)
  }
  if (!"stage_cm" %in% names(m)) m$stage_cm <- NA_real_
  if (!"discharge_m3s" %in% names(m)) m$discharge_m3s <- NA_real_
  m$stage_cm <- suppressWarnings(as.numeric(m$stage_cm))
  m$discharge_m3s <- suppressWarnings(as.numeric(m$discharge_m3s))
  m$measurement_year <- as.integer(format(m$measurement_date, "%Y"))
  m
}
ana_diag_empty_rating_curves <- function() {
  data.frame(
    station_code = character(),
    rating_curve_id = character(),
    rating_curve_segment_id = character(),
    segment_number = integer(),
    valid_from = as.Date(character()),
    valid_to = as.Date(character()),
    stage_min_cm = numeric(),
    stage_max_cm = numeric(),
    coefficient_a = numeric(),
    coefficient_h0_cm = numeric(),
    coefficient_h0_m = numeric(),
    coefficient_n = numeric(),
    stringsAsFactors = FALSE
  )
}
ana_diag_standardize_rating_curves <- function(rating_curves) {
  if (is.null(rating_curves) || nrow(rating_curves) == 0) {
    return(ana_diag_empty_rating_curves())
  }
  rc <- as.data.frame(rating_curves, stringsAsFactors = FALSE)
  if (!"station_code" %in% names(rc)) rc$station_code <- NA_character_
  rc$station_code <- as.character(rc$station_code)
  if (!"valid_from" %in% names(rc)) rc$valid_from <- as.Date(NA)
  if (!"valid_to" %in% names(rc)) rc$valid_to <- as.Date(NA)
  rc$valid_from <- as.Date(rc$valid_from)
  rc$valid_to <- as.Date(rc$valid_to)
  numeric_cols <- c(
    "stage_min_cm", "stage_max_cm", "coefficient_a",
    "coefficient_h0_cm", "coefficient_h0_m", "coefficient_h0",
    "coefficient_n"
  )
  for (col in intersect(numeric_cols, names(rc))) {
    rc[[col]] <- suppressWarnings(as.numeric(rc[[col]]))
  }
  if (!"coefficient_h0_m" %in% names(rc)) {
    if ("coefficient_h0_cm" %in% names(rc)) {
      rc$coefficient_h0_m <- rc$coefficient_h0_cm / 100
    } else if ("coefficient_h0" %in% names(rc)) {
      rc$coefficient_h0_m <- rc$coefficient_h0
    } else {
      rc$coefficient_h0_m <- NA_real_
    }
  }
  if (!"coefficient_h0_cm" %in% names(rc)) {
    rc$coefficient_h0_cm <- rc$coefficient_h0_m * 100
  }
  needed <- c("stage_min_cm", "stage_max_cm", "coefficient_a", "coefficient_n")
  for (col in needed) {
    if (!col %in% names(rc)) rc[[col]] <- NA_real_
  }
  if (!"rating_curve_id" %in% names(rc)) {
    key <- paste(rc$station_code, rc$valid_from, rc$valid_to, sep = "_")
    rc$rating_curve_id <- paste0("curve_", as.integer(factor(key)))
  }
  rc$rating_curve_id <- as.character(rc$rating_curve_id)
  if (!"segment_number" %in% names(rc)) {
    rc$segment_number <- ave(
      seq_len(nrow(rc)),
      rc$rating_curve_id,
      FUN = seq_along
    )
  }
  rc$segment_number <- as.integer(rc$segment_number)
  if (!"rating_curve_segment_id" %in% names(rc)) {
    rc$rating_curve_segment_id <- paste0(rc$rating_curve_id, "_seg_", rc$segment_number)
  }
  rc$rating_curve_segment_id <- as.character(rc$rating_curve_segment_id)
  rc
}
ana_diag_safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}
ana_diag_safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}
ana_diag_safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}
ana_diag_safe_mad <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::mad(x, constant = 1, na.rm = TRUE)
}
ana_diag_safe_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}
ana_diag_safe_sd <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::sd(x)
}
ana_diag_format_coef <- function(x, digits = 3) {
  out <- formatC(x, format = "f", digits = digits)
  out <- sub("\\.?0+$", "", out)
  out[is.na(x)] <- "NA"
  out
}
ana_diag_make_equation_text <- function(a, h0_m, n) {
  sign_text <- ifelse(is.na(h0_m), "-", ifelse(h0_m < 0, "+", "-"))
  h0_abs <- abs(h0_m)
  paste0(
    "Q=", ana_diag_format_coef(a),
    "(H", sign_text, ana_diag_format_coef(h0_abs), ")^",
    ana_diag_format_coef(n)
  )
}
ana_diag_class_percent <- function(x) {
  ifelse(
    is.na(x), "not_available",
    ifelse(x == 0, "none",
      ifelse(x < 0.01, "very_low",
        ifelse(x < 0.05, "low",
          ifelse(x < 0.15, "moderate", "high")
        )
      )
    )
  )
}
ana_diag_class_residual <- function(x) {
  ifelse(
    is.na(x), "not_available",
    ifelse(x < 0.10, "low",
      ifelse(x < 0.25, "moderate",
        ifelse(x < 0.50, "high", "very_high")
      )
    )
  )
}
ana_diag_predict_rating_discharge <- function(
    stage_cm,
    coefficient_a,
    coefficient_h0_m,
    coefficient_n) {
  stage_m <- stage_cm / 100
  base_m <- stage_m - coefficient_h0_m
  out <- ifelse(
    !is.na(base_m) & base_m > 0 &
      !is.na(coefficient_a) & !is.na(coefficient_n),
    coefficient_a * (base_m ^ coefficient_n),
    NA_real_
  )
  out
}
ana_diag_make_curve_metadata <- function(curves) {
  curves <- ana_diag_standardize_rating_curves(curves)
  if (nrow(curves) == 0) return(data.frame())
  ids <- unique(curves$rating_curve_id)
  out <- lapply(ids, function(id) {
    x <- curves[curves$rating_curve_id == id, , drop = FALSE]
    data.frame(
      rating_curve_id = id,
      valid_from_first = ana_diag_safe_first(x$valid_from),
      valid_to_first = ana_diag_safe_first(x$valid_to),
      n_curve_segments = length(unique(x$rating_curve_segment_id)),
      stage_min_curve_cm = ana_diag_safe_min(x$stage_min_cm),
      stage_max_curve_cm = ana_diag_safe_max(x$stage_max_cm),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[order(out$valid_from_first, out$valid_to_first, out$rating_curve_id), , drop = FALSE]
  out$curve_short_label <- paste0("RC ", seq_len(nrow(out)))
  out$curve_label <- paste0(
    out$curve_short_label,
    " | ",
    out$n_curve_segments,
    ifelse(out$n_curve_segments == 1, " segment", " segments")
  )
  rownames(out) <- NULL
  out
}
ana_diag_make_curve_segment_metadata <- function(curves, curve_metadata = NULL) {
  curves <- ana_diag_standardize_rating_curves(curves)
  if (nrow(curves) == 0) return(data.frame())
  if (is.null(curve_metadata)) curve_metadata <- ana_diag_make_curve_metadata(curves)
  keep <- !is.na(curves$stage_min_cm) &
    !is.na(curves$stage_max_cm) &
    !is.na(curves$coefficient_a) &
    !is.na(curves$coefficient_h0_m) &
    !is.na(curves$coefficient_n) &
    curves$stage_max_cm > curves$stage_min_cm
  segments <- curves[keep, , drop = FALSE]
  if (nrow(segments) == 0) return(data.frame())
  label_cols <- curve_metadata[, c("rating_curve_id", "curve_short_label", "curve_label"), drop = FALSE]
  segments <- merge(segments, label_cols, by = "rating_curve_id", all.x = TRUE, sort = FALSE)
  segments$segment_equation <- ana_diag_make_equation_text(
    segments$coefficient_a,
    segments$coefficient_h0_m,
    segments$coefficient_n
  )
  segments$curve_segment_label <- paste0(
    segments$curve_short_label,
    " | seg ",
    segments$segment_number,
    " | H=",
    round(segments$stage_min_cm, 0),
    "-",
    round(segments$stage_max_cm, 0),
    " cm"
  )
  segments <- segments[order(segments$valid_from, segments$valid_to, segments$rating_curve_id, segments$segment_number), , drop = FALSE]
  rownames(segments) <- NULL
  segments
}
ana_diag_make_rating_curve_points <- function(curve_segments, n_points = 80) {
  if (is.null(curve_segments) || nrow(curve_segments) == 0) return(data.frame())
  out <- lapply(seq_len(nrow(curve_segments)), function(i) {
    row <- curve_segments[i, , drop = FALSE]
    stages_cm <- seq(row$stage_min_cm, row$stage_max_cm, length.out = n_points)
    discharge <- ana_diag_predict_rating_discharge(
      stage_cm = stages_cm,
      coefficient_a = row$coefficient_a,
      coefficient_h0_m = row$coefficient_h0_m,
      coefficient_n = row$coefficient_n
    )
    data.frame(
      station_code = row$station_code,
      rating_curve_id = row$rating_curve_id,
      rating_curve_segment_id = row$rating_curve_segment_id,
      segment_number = row$segment_number,
      curve_label = row$curve_label,
      curve_segment_label = row$curve_segment_label,
      valid_from = row$valid_from,
      valid_to = row$valid_to,
      stage_min_cm = row$stage_min_cm,
      stage_max_cm = row$stage_max_cm,
      stage_cm = stages_cm,
      discharge_m3s = discharge,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[!is.na(out$discharge_m3s) & is.finite(out$discharge_m3s) & out$discharge_m3s >= 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}
ana_diag_match_measurements_to_rating_curves <- function(measurements, curve_segments) {
  m <- ana_diag_standardize_measurements(measurements)
  if (nrow(m) == 0 || is.null(curve_segments) || nrow(curve_segments) == 0) {
    return(data.frame())
  }
  m$.measurement_id <- seq_len(nrow(m))
  m <- m[!is.na(m$measurement_date) &
    !is.na(m$stage_cm) &
    !is.na(m$discharge_m3s) &
    m$stage_cm > 0 &
    m$discharge_m3s > 0, , drop = FALSE]
  if (nrow(m) == 0) return(data.frame())
  out <- lapply(seq_len(nrow(curve_segments)), function(i) {
    row <- curve_segments[i, , drop = FALSE]
    valid_from_date <- as.Date(row$valid_from)
    valid_to_date <- as.Date(row$valid_to)
    if (is.na(valid_from_date)) return(data.frame())
    if (is.na(valid_to_date)) valid_to_date <- Sys.Date()
    matched <- m[
      m$measurement_date >= valid_from_date &
        m$measurement_date <= valid_to_date &
        m$stage_cm >= row$stage_min_cm &
        m$stage_cm <= row$stage_max_cm,
      , drop = FALSE
    ]
    if (nrow(matched) == 0) return(data.frame())
    q_hat <- ana_diag_predict_rating_discharge(
      stage_cm = matched$stage_cm,
      coefficient_a = row$coefficient_a,
      coefficient_h0_m = row$coefficient_h0_m,
      coefficient_n = row$coefficient_n
    )
    matched$rating_curve_id <- row$rating_curve_id
    matched$rating_curve_segment_id <- row$rating_curve_segment_id
    matched$segment_number <- row$segment_number
    matched$curve_label <- row$curve_label
    matched$curve_segment_label <- row$curve_segment_label
    matched$rating_predicted_discharge_m3s <- q_hat
    matched$rating_log_residual <- log(matched$discharge_m3s) - log(q_hat)
    matched$rating_relative_residual_pct <- 100 * (exp(matched$rating_log_residual) - 1)
    matched[!is.na(matched$rating_predicted_discharge_m3s) &
      is.finite(matched$rating_predicted_discharge_m3s) &
      matched$rating_predicted_discharge_m3s > 0 &
      !is.na(matched$rating_log_residual) &
      is.finite(matched$rating_log_residual), , drop = FALSE]
  })
  out <- out[vapply(out, nrow, integer(1)) > 0]
  if (length(out) == 0) return(data.frame())
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}
ana_diag_make_best_rating_match <- function(rating_matches) {
  if (is.null(rating_matches) || nrow(rating_matches) == 0) return(data.frame())
  ids <- unique(rating_matches$.measurement_id)
  out <- lapply(ids, function(id) {
    x <- rating_matches[rating_matches$.measurement_id == id, , drop = FALSE]
    x <- x[order(abs(x$rating_log_residual), x$rating_curve_segment_id), , drop = FALSE]
    x[1, , drop = FALSE]
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}
ana_diag_make_residual_envelopes <- function(best_matches, params) {
  if (is.null(best_matches) || nrow(best_matches) == 0) return(data.frame())
  bm <- best_matches[!is.na(best_matches$rating_log_residual) & is.finite(best_matches$rating_log_residual), , drop = FALSE]
  if (nrow(bm) == 0) return(data.frame())
  key <- paste(bm$station_code, bm$rating_curve_id, bm$rating_curve_segment_id, sep = "\r")
  groups <- split(bm, key)
  out <- lapply(groups, function(x) {
    sd_value <- ana_diag_safe_sd(x$rating_log_residual)
    mean_value <- mean(x$rating_log_residual, na.rm = TRUE)
    data.frame(
      station_code = x$station_code[1],
      rating_curve_id = x$rating_curve_id[1],
      rating_curve_segment_id = x$rating_curve_segment_id[1],
      curve_label = x$curve_label[1],
      curve_segment_label = x$curve_segment_label[1],
      n_residual_points = nrow(x),
      mean_log_residual = mean_value,
      median_log_residual = median(x$rating_log_residual, na.rm = TRUE),
      sd_log_residual = sd_value,
      median_abs_log_residual = median(abs(x$rating_log_residual), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out$envelope_lower_log_residual <- out$mean_log_residual - params$residual_envelope_sd_multiplier * out$sd_log_residual
  out$envelope_upper_log_residual <- out$mean_log_residual + params$residual_envelope_sd_multiplier * out$sd_log_residual
  out$has_residual_envelope <- out$n_residual_points >= params$min_residual_points_per_segment & !is.na(out$sd_log_residual)
  rownames(out) <- NULL
  out
}
ana_diag_add_envelope_flags <- function(best_matches, envelopes) {
  if (is.null(best_matches) || nrow(best_matches) == 0) return(data.frame())
  if (is.null(envelopes) || nrow(envelopes) == 0) {
    best_matches$outside_residual_envelope <- NA
    return(best_matches)
  }
  cols <- c(
    "rating_curve_segment_id", "envelope_lower_log_residual",
    "envelope_upper_log_residual", "has_residual_envelope"
  )
  out <- merge(best_matches, envelopes[, cols, drop = FALSE], by = "rating_curve_segment_id", all.x = TRUE, sort = FALSE)
  out$outside_residual_envelope <- ifelse(
    out$has_residual_envelope &
      !is.na(out$rating_log_residual) &
      (out$rating_log_residual < out$envelope_lower_log_residual |
         out$rating_log_residual > out$envelope_upper_log_residual),
    TRUE,
    FALSE
  )
  out
}
ana_diag_group_summary <- function(data, group_col, value_col, prefix, params) {
  df <- data[!is.na(data[[group_col]]) & !is.na(data[[value_col]]), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())
  groups <- split(df, df[[group_col]])
  out <- lapply(groups, function(x) {
    if (prefix == "stage") {
      data.frame(
        stage_group = x[[group_col]][1],
        n_repeated_stage_group = nrow(x),
        discharge_min_m3s_in_stage_group = ana_diag_safe_min(x[[value_col]]),
        discharge_max_m3s_in_stage_group = ana_diag_safe_max(x[[value_col]]),
        discharge_median_m3s_in_stage_group = median(x[[value_col]], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        discharge_group = x[[group_col]][1],
        n_repeated_discharge_group = nrow(x),
        stage_min_cm_in_discharge_group = ana_diag_safe_min(x[[value_col]]),
        stage_max_cm_in_discharge_group = ana_diag_safe_max(x[[value_col]]),
        stringsAsFactors = FALSE
      )
    }
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  if (prefix == "stage") {
    out$discharge_spread_m3s_in_stage_group <- out$discharge_max_m3s_in_stage_group - out$discharge_min_m3s_in_stage_group
    out$discharge_rel_spread_in_stage_group <- ana_diag_safe_divide(
      out$discharge_spread_m3s_in_stage_group,
      abs(out$discharge_median_m3s_in_stage_group)
    )
    out$repeated_stage_variable_discharge_group_flag <-
      out$n_repeated_stage_group >= params$min_repeated_group_size &
      out$discharge_spread_m3s_in_stage_group >= params$min_abs_discharge_spread_m3s_for_repeated_stage &
      out$discharge_rel_spread_in_stage_group >= params$min_rel_discharge_spread_for_repeated_stage
  } else {
    out$stage_spread_cm_in_discharge_group <- out$stage_max_cm_in_discharge_group - out$stage_min_cm_in_discharge_group
    out$repeated_discharge_variable_stage_group_flag <-
      out$n_repeated_discharge_group >= params$min_repeated_group_size &
      out$stage_spread_cm_in_discharge_group >= params$min_stage_spread_cm_for_repeated_discharge
  }
  out
}
ana_diag_make_measurement_flags <- function(measurements, params) {
  m <- ana_diag_standardize_measurements(measurements)
  if (nrow(m) == 0) return(data.frame())
  m$.measurement_id <- seq_len(nrow(m))
  m$stage_zero_or_negative_flag <- !is.na(m$stage_cm) & m$stage_cm <= params$stage_zero_tolerance_cm
  m$discharge_zero_or_negative_flag <- !is.na(m$discharge_m3s) & m$discharge_m3s <= params$discharge_zero_tolerance_m3s
  m$stage_group <- ifelse(!is.na(m$stage_cm), round(m$stage_cm, params$stage_group_round_digits), NA_real_)
  m$discharge_group <- ifelse(!is.na(m$discharge_m3s), round(m$discharge_m3s, params$discharge_group_round_digits), NA_real_)
  repeated_stage <- ana_diag_group_summary(m, "stage_group", "discharge_m3s", "stage", params)
  repeated_discharge <- ana_diag_group_summary(m, "discharge_group", "stage_cm", "discharge", params)
  if (nrow(repeated_stage) > 0) {
    m <- merge(m, repeated_stage, by = "stage_group", all.x = TRUE, sort = FALSE)
  } else {
    m$repeated_stage_variable_discharge_group_flag <- FALSE
  }
  if (nrow(repeated_discharge) > 0) {
    m <- merge(m, repeated_discharge, by = "discharge_group", all.x = TRUE, sort = FALSE)
  } else {
    m$repeated_discharge_variable_stage_group_flag <- FALSE
  }
  m$repeated_stage_variable_discharge_flag <- ifelse(
    is.na(m$repeated_stage_variable_discharge_group_flag),
    FALSE,
    m$repeated_stage_variable_discharge_group_flag
  )
  m$repeated_discharge_variable_stage_flag <- ifelse(
    is.na(m$repeated_discharge_variable_stage_group_flag),
    FALSE,
    m$repeated_discharge_variable_stage_group_flag
  )
  m$any_obvious_measurement_attention_flag <- m$stage_zero_or_negative_flag |
    m$discharge_zero_or_negative_flag |
    m$repeated_stage_variable_discharge_flag |
    m$repeated_discharge_variable_stage_flag
  m <- m[order(m$.measurement_id), , drop = FALSE]
  rownames(m) <- NULL
  m
}
ana_diag_make_repeated_value_group_details <- function(measurement_flags) {
  if (is.null(measurement_flags) || nrow(measurement_flags) == 0) return(data.frame())
  repeated_stage <- data.frame()
  if ("repeated_stage_variable_discharge_flag" %in% names(measurement_flags)) {
    x <- measurement_flags[measurement_flags$repeated_stage_variable_discharge_flag, , drop = FALSE]
    if (nrow(x) > 0) {
      repeated_stage <- unique(data.frame(
        station_code = x$station_code,
        group_type = "same_stage_variable_discharge",
        group_value = x$stage_group,
        n_group = x$n_repeated_stage_group,
        spread_value = x$discharge_spread_m3s_in_stage_group,
        relative_spread = x$discharge_rel_spread_in_stage_group,
        stringsAsFactors = FALSE
      ))
    }
  }
  repeated_discharge <- data.frame()
  if ("repeated_discharge_variable_stage_flag" %in% names(measurement_flags)) {
    x <- measurement_flags[measurement_flags$repeated_discharge_variable_stage_flag, , drop = FALSE]
    if (nrow(x) > 0) {
      repeated_discharge <- unique(data.frame(
        station_code = x$station_code,
        group_type = "same_discharge_variable_stage",
        group_value = x$discharge_group,
        n_group = x$n_repeated_discharge_group,
        spread_value = x$stage_spread_cm_in_discharge_group,
        relative_spread = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }
  out <- rbind(repeated_stage, repeated_discharge)
  rownames(out) <- NULL
  out
}
ana_diag_robust_centered_sse <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  med <- median(x)
  abs_dev <- abs(x - med)
  cap <- stats::quantile(abs_dev, probs = 0.90, na.rm = TRUE, names = FALSE)
  if (is.na(cap) || cap <= 0) cap <- max(abs_dev, na.rm = TRUE)
  if (is.na(cap) || cap <= 0) return(0)
  sum(pmin(abs_dev, cap) ^ 2)
}
ana_diag_make_h0_candidates <- function(stage_m, params) {
  stage_m <- stage_m[!is.na(stage_m) & is.finite(stage_m) & stage_m > 0]
  if (length(stage_m) == 0) return(numeric(0))
  h_min <- min(stage_m)
  h_max <- max(stage_m)
  h_span <- h_max - h_min
  grid_span <- max(params$h0_grid_min_span_m, params$h0_grid_span_multiplier * h_span)
  grid_span <- min(grid_span, params$h0_grid_max_span_m)
  lower <- h_min - grid_span
  upper <- h_min - params$h0_min_offset_m
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    return(numeric(0))
  }
  unique(seq(lower, upper, length.out = params$n_h0_grid))
}
ana_diag_fit_power_rating_baseline <- function(measurements, params) {
  df <- ana_diag_standardize_measurements(measurements)
  df <- df[!is.na(df$stage_cm) &
    !is.na(df$discharge_m3s) &
    !is.na(df$measurement_date) &
    df$stage_cm > 0 &
    df$discharge_m3s > 0, , drop = FALSE]
  if (nrow(df) == 0) return(list(points = data.frame(), model = data.frame()))
  df$stage_m <- df$stage_cm / 100
  df$log_observed_discharge <- log(df$discharge_m3s)
  if (nrow(df) < params$min_power_model_points || length(unique(df$stage_m)) < 3) {
    return(list(points = data.frame(), model = data.frame()))
  }
  h0_candidates <- ana_diag_make_h0_candidates(df$stage_m, params)
  if (length(h0_candidates) == 0) return(list(points = data.frame(), model = data.frame()))
  fits <- lapply(h0_candidates, function(h0) {
    tmp <- df
    tmp$effective_stage_m <- tmp$stage_m - h0
    tmp$log_effective_stage <- log(tmp$effective_stage_m)
    tmp <- tmp[!is.na(tmp$log_effective_stage) & is.finite(tmp$log_effective_stage) & tmp$effective_stage_m > 0, , drop = FALSE]
    if (nrow(tmp) < params$min_power_model_points || length(unique(tmp$log_effective_stage)) < 2) {
      return(NULL)
    }
    fit <- tryCatch(
      stats::lm(log_observed_discharge ~ log_effective_stage, data = tmp),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    coefs <- stats::coef(fit)
    intercept <- unname(coefs[1])
    exponent_b <- unname(coefs[2])
    if (is.na(intercept) || is.na(exponent_b) ||
        exponent_b <= params$min_power_exponent ||
        exponent_b > params$max_power_exponent) {
      return(NULL)
    }
    predicted_log_discharge <- as.numeric(stats::predict(fit, newdata = tmp))
    log_residual <- tmp$log_observed_discharge - predicted_log_discharge
    data.frame(
      h0_m = h0,
      coefficient_a = exp(intercept),
      coefficient_b = exponent_b,
      robust_sse = ana_diag_robust_centered_sse(log_residual),
      median_abs_log_residual = median(abs(log_residual), na.rm = TRUE),
      n_model_points = nrow(tmp),
      stringsAsFactors = FALSE
    )
  })
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) == 0) return(list(points = data.frame(), model = data.frame()))
  fit_table <- do.call(rbind, fits)
  fit_table <- fit_table[order(fit_table$robust_sse, fit_table$median_abs_log_residual, abs(fit_table$h0_m)), , drop = FALSE]
  best_model <- fit_table[1, , drop = FALSE]
  best_model$model_type <- "power_rating_baseline"
  best_model$equation <- ana_diag_make_equation_text(
    best_model$coefficient_a,
    best_model$h0_m,
    best_model$coefficient_b
  )
  df$effective_stage_m <- df$stage_m - best_model$h0_m[1]
  df$power_predicted_discharge_m3s <- ifelse(
    df$effective_stage_m > 0,
    best_model$coefficient_a[1] * (df$effective_stage_m ^ best_model$coefficient_b[1]),
    NA_real_
  )
  df$power_log_residual <- log(df$discharge_m3s) - log(df$power_predicted_discharge_m3s)
  df$power_relative_residual_pct <- 100 * (exp(df$power_log_residual) - 1)
  df <- df[!is.na(df$power_predicted_discharge_m3s) &
    is.finite(df$power_predicted_discharge_m3s) &
    df$power_predicted_discharge_m3s > 0 &
    !is.na(df$power_log_residual) &
    is.finite(df$power_log_residual), , drop = FALSE]
  rownames(df) <- NULL
  rownames(best_model) <- NULL
  list(points = df, model = best_model)
}
ana_diag_make_power_curve_points <- function(power_model, stage_min_cm, stage_max_cm, params) {
  if (is.null(power_model) || nrow(power_model) == 0 || is.na(stage_min_cm) || is.na(stage_max_cm)) {
    return(data.frame())
  }
  stages_cm <- seq(stage_min_cm, stage_max_cm, length.out = params$n_stage_points_power_curve)
  stage_m <- stages_cm / 100
  effective_stage_m <- stage_m - power_model$h0_m[1]
  discharge <- ifelse(
    effective_stage_m > 0,
    power_model$coefficient_a[1] * (effective_stage_m ^ power_model$coefficient_b[1]),
    NA_real_
  )
  out <- data.frame(stage_cm = stages_cm, discharge_m3s = discharge, stringsAsFactors = FALSE)
  out <- out[!is.na(out$discharge_m3s) & is.finite(out$discharge_m3s) & out$discharge_m3s > 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}
ana_diag_make_break_candidates <- function(points, params) {
  dates <- sort(unique(as.Date(points$measurement_date)))
  if (length(dates) < 3) return(as.Date(character(0)))
  candidate_dates <- dates[-length(dates)]
  if (length(candidate_dates) > params$max_break_candidates) {
    idx <- unique(round(seq(1, length(candidate_dates), length.out = params$max_break_candidates)))
    candidate_dates <- candidate_dates[idx]
  }
  as.Date(candidate_dates)
}
ana_diag_assign_temporal_regimes <- function(points, breaks) {
  points <- points[order(points$measurement_date, points$measurement_datetime), , drop = FALSE]
  breaks <- sort(as.Date(breaks))
  regime_number <- rep(1L, nrow(points))
  if (length(breaks) > 0) {
    for (i in seq_along(breaks)) {
      regime_number[as.Date(points$measurement_date) > breaks[i]] <- i + 1L
    }
  }
  points$regime_number <- regime_number
  points
}
ana_diag_regime_summary <- function(assigned) {
  groups <- split(assigned, assigned$regime_number)
  out <- lapply(groups, function(x) {
    date_start <- min(x$measurement_date, na.rm = TRUE)
    date_end <- max(x$measurement_date, na.rm = TRUE)
    data.frame(
      regime_number = x$regime_number[1],
      n_points = nrow(x),
      date_start = date_start,
      date_end = date_end,
      date_span_years = as.numeric(date_end - date_start) / 365.25,
      median_log_residual = median(x$power_log_residual, na.rm = TRUE),
      robust_sse = ana_diag_robust_centered_sse(x$power_log_residual),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}
ana_diag_evaluate_temporal_partition <- function(points, breaks, params) {
  points <- points[!is.na(points$power_log_residual) & is.finite(points$power_log_residual), , drop = FALSE]
  points <- points[order(points$measurement_date, points$measurement_datetime), , drop = FALSE]
  if (nrow(points) == 0) return(data.frame())
  global_dispersion <- ana_diag_robust_centered_sse(points$power_log_residual)
  if (is.na(global_dispersion) || global_dispersion <= 0) global_dispersion <- 0
  assigned <- ana_diag_assign_temporal_regimes(points, breaks)
  regime_summary <- ana_diag_regime_summary(assigned)
  regime_summary$point_fraction <- regime_summary$n_points / nrow(assigned)
  segmented_dispersion <- sum(regime_summary$robust_sse, na.rm = TRUE)
  dispersion_gain <- ifelse(global_dispersion > 0, 1 - segmented_dispersion / global_dispersion, 0)
  residual_shift_log <- if (nrow(regime_summary) >= 2) {
    max(regime_summary$median_log_residual, na.rm = TRUE) - min(regime_summary$median_log_residual, na.rm = TRUE)
  } else {
    0
  }
  global_mad <- ana_diag_safe_mad(points$power_log_residual)
  required_shift <- max(params$min_log_residual_shift, params$residual_shift_mad_fraction * global_mad, na.rm = TRUE)
  if (!is.finite(required_shift)) required_shift <- params$min_log_residual_shift
  accepted <- all(regime_summary$n_points >= params$min_regime_measurements) &&
    all(regime_summary$point_fraction >= params$min_regime_fraction) &&
    all(regime_summary$date_span_years >= params$min_regime_span_years) &&
    residual_shift_log >= required_shift &&
    dispersion_gain >= params$min_break_gain
  data.frame(
    n_regimes = nrow(regime_summary),
    break_dates = ifelse(length(breaks) == 0, NA_character_, paste(as.character(breaks), collapse = ";")),
    n_points = nrow(assigned),
    min_regime_points = min(regime_summary$n_points, na.rm = TRUE),
    min_regime_fraction = min(regime_summary$point_fraction, na.rm = TRUE),
    min_regime_span_years = min(regime_summary$date_span_years, na.rm = TRUE),
    residual_shift_log = residual_shift_log,
    residual_shift_pct = 100 * (exp(residual_shift_log) - 1),
    required_residual_shift_log = required_shift,
    dispersion_gain = dispersion_gain,
    accepted = accepted,
    stringsAsFactors = FALSE
  )
}
ana_diag_find_best_temporal_regime_model <- function(points, params) {
  points <- points[order(points$measurement_date, points$measurement_datetime), , drop = FALSE]
  base_model <- ana_diag_evaluate_temporal_partition(points, breaks = as.Date(character(0)), params = params)
  if (nrow(base_model) > 0) base_model$model_type <- "baseline"
  candidates <- ana_diag_make_break_candidates(points, params)
  one_break_scores <- data.frame()
  two_break_scores <- data.frame()
  best_one <- data.frame()
  best_two <- data.frame()
  if (length(candidates) > 0 && params$max_temporal_regimes >= 2) {
    one_break_scores <- do.call(rbind, lapply(candidates, function(b1) {
      ana_diag_evaluate_temporal_partition(points, breaks = as.Date(b1), params = params)
    }))
    if (nrow(one_break_scores) > 0) {
      one_break_scores$model_type <- "one_break"
      accepted <- one_break_scores[one_break_scores$accepted, , drop = FALSE]
      if (nrow(accepted) > 0) {
        accepted <- accepted[order(-accepted$dispersion_gain, -accepted$residual_shift_log, accepted$break_dates), , drop = FALSE]
        best_one <- accepted[1, , drop = FALSE]
      }
    }
  }
  if (length(candidates) > 1 && params$max_temporal_regimes >= 3 && nrow(best_one) > 0) {
    base_breaks <- as.Date(strsplit(best_one$break_dates[1], ";", fixed = TRUE)[[1]])
    pairs <- lapply(candidates, function(candidate) sort(unique(c(base_breaks, candidate))))
    pairs <- pairs[vapply(pairs, length, integer(1)) == 2]
    if (length(pairs) > 0) {
      pair_keys <- vapply(pairs, function(x) paste(as.character(x), collapse = ";"), character(1))
      pairs <- pairs[!duplicated(pair_keys)]
      two_break_scores <- do.call(rbind, lapply(pairs, function(bs) {
        ana_diag_evaluate_temporal_partition(points, breaks = bs, params = params)
      }))
      if (nrow(two_break_scores) > 0) {
        two_break_scores$model_type <- "two_break"
        accepted_two <- two_break_scores[
          two_break_scores$accepted &
            two_break_scores$dispersion_gain >= best_one$dispersion_gain[1] + params$min_incremental_gain_for_second_break,
          , drop = FALSE
        ]
        if (nrow(accepted_two) > 0) {
          accepted_two <- accepted_two[order(-accepted_two$dispersion_gain, -accepted_two$residual_shift_log, accepted_two$break_dates), , drop = FALSE]
          best_two <- accepted_two[1, , drop = FALSE]
        }
      }
    }
  }
  model_scores <- rbind(base_model, one_break_scores, two_break_scores)
  best_model <- if (nrow(best_two) > 0) {
    best_two
  } else if (nrow(best_one) > 0) {
    best_one
  } else {
    base_model
  }
  list(best_model = best_model, model_scores = model_scores)
}
ana_diag_fit_residual_temporal_regimes <- function(measurements, station_code_value = NA, params) {
  baseline <- ana_diag_fit_power_rating_baseline(measurements, params)
  if (nrow(baseline$points) == 0 || nrow(baseline$model) == 0) {
    return(list(
      points = data.frame(),
      power_model = baseline$model,
      model_scores = data.frame(),
      regime_summary = data.frame()
    ))
  }
  model <- ana_diag_find_best_temporal_regime_model(baseline$points, params)
  best <- model$best_model
  points <- baseline$points
  regime_summary <- data.frame()
  if (nrow(best) > 0 && !is.na(best$break_dates[1]) && isTRUE(best$accepted[1])) {
    breaks <- as.Date(strsplit(best$break_dates[1], ";", fixed = TRUE)[[1]])
    points <- ana_diag_assign_temporal_regimes(points, breaks)
    regime_summary <- ana_diag_regime_summary(points)
    regime_summary$station_code <- station_code_value
  } else {
    points$regime_number <- 1L
  }
  if (nrow(model$model_scores) > 0 && nrow(baseline$model) > 0) {
    model$model_scores$station_code <- station_code_value
    model$model_scores$baseline_equation <- baseline$model$equation[1]
    model$model_scores$baseline_h0_m <- baseline$model$h0_m[1]
    model$model_scores$baseline_coefficient_a <- baseline$model$coefficient_a[1]
    model$model_scores$baseline_coefficient_b <- baseline$model$coefficient_b[1]
  }
  list(
    points = points,
    power_model = baseline$model,
    model_scores = model$model_scores,
    regime_summary = regime_summary
  )
}
ana_diag_make_diagnostic_summary <- function(
    measurement_flags,
    rating_curves,
    best_matches,
    residual_points,
    temporal_regime,
    detailed) {
  n_measurements <- ifelse(is.null(measurement_flags), 0L, nrow(measurement_flags))
  station_code_value <- if (n_measurements > 0) {
    ana_diag_safe_first(measurement_flags$station_code)
  } else if (!is.null(rating_curves) && nrow(rating_curves) > 0 && "station_code" %in% names(rating_curves)) {
    ana_diag_safe_first(rating_curves$station_code)
  } else {
    NA_character_
  }
  n_valid_measurements <- if (n_measurements > 0) {
    sum(!is.na(measurement_flags$stage_cm) & !is.na(measurement_flags$discharge_m3s) &
      measurement_flags$stage_cm > 0 & measurement_flags$discharge_m3s > 0)
  } else 0L
  n_stage_zero <- if (n_measurements > 0) sum(measurement_flags$stage_zero_or_negative_flag, na.rm = TRUE) else 0L
  n_discharge_zero <- if (n_measurements > 0) sum(measurement_flags$discharge_zero_or_negative_flag, na.rm = TRUE) else 0L
  n_rep_stage <- if (n_measurements > 0) sum(measurement_flags$repeated_stage_variable_discharge_flag, na.rm = TRUE) else 0L
  n_rep_discharge <- if (n_measurements > 0) sum(measurement_flags$repeated_discharge_variable_stage_flag, na.rm = TRUE) else 0L
  n_rating_curves <- if (is.null(rating_curves) || nrow(rating_curves) == 0) 0L else length(unique(rating_curves$rating_curve_id))
  n_rating_curve_segments <- if (is.null(rating_curves) || nrow(rating_curves) == 0) 0L else length(unique(rating_curves$rating_curve_segment_id))
  n_best <- ifelse(is.null(best_matches), 0L, nrow(best_matches))
  rating_match_fraction <- ana_diag_safe_divide(n_best, n_valid_measurements)
  median_abs_rating_log_residual <- if (n_best > 0) median(abs(best_matches$rating_log_residual), na.rm = TRUE) else NA_real_
  outside_envelope_fraction <- if (!is.null(residual_points) && nrow(residual_points) > 0 && "outside_residual_envelope" %in% names(residual_points)) {
    ana_diag_safe_divide(sum(residual_points$outside_residual_envelope, na.rm = TRUE), sum(!is.na(residual_points$outside_residual_envelope)))
  } else {
    NA_real_
  }
  n_temporal_regimes <- NA_integer_
  regime_evidence_class <- "not_available"
  baseline_power_equation <- NA_character_
  baseline_power_h0_m <- NA_real_
  baseline_power_a <- NA_real_
  baseline_power_b <- NA_real_
  if (!is.null(temporal_regime) && !is.null(temporal_regime$model_scores) && nrow(temporal_regime$model_scores) > 0) {
    scores <- temporal_regime$model_scores
    accepted_scores <- scores[scores$accepted & scores$n_regimes > 1, , drop = FALSE]
    if (nrow(accepted_scores) > 0) {
      accepted_scores <- accepted_scores[order(-accepted_scores$n_regimes, -accepted_scores$dispersion_gain), , drop = FALSE]
      n_temporal_regimes <- accepted_scores$n_regimes[1]
      regime_evidence_class <- ifelse(n_temporal_regimes >= 3, "strong", "moderate")
    } else {
      n_temporal_regimes <- 1L
      regime_evidence_class <- "low"
    }
    baseline_power_equation <- scores$baseline_equation[1]
    baseline_power_h0_m <- scores$baseline_h0_m[1]
    baseline_power_a <- scores$baseline_coefficient_a[1]
    baseline_power_b <- scores$baseline_coefficient_b[1]
  }
  score <- 0
  score <- score + ifelse(!is.na(ana_diag_safe_divide(n_stage_zero, n_measurements)) && ana_diag_safe_divide(n_stage_zero, n_measurements) >= 0.05, 1, 0)
  score <- score + ifelse(!is.na(ana_diag_safe_divide(n_discharge_zero, n_measurements)) && ana_diag_safe_divide(n_discharge_zero, n_measurements) >= 0.05, 1, 0)
  score <- score + ifelse(!is.na(ana_diag_safe_divide(n_rep_stage, n_measurements)) && ana_diag_safe_divide(n_rep_stage, n_measurements) >= 0.15, 1, 0)
  score <- score + ifelse(!is.na(ana_diag_safe_divide(n_rep_discharge, n_measurements)) && ana_diag_safe_divide(n_rep_discharge, n_measurements) >= 0.15, 1, 0)
  score <- score + ifelse(!is.na(rating_match_fraction) && rating_match_fraction < 0.60, 1, 0)
  score <- score + ifelse(!is.na(median_abs_rating_log_residual) && median_abs_rating_log_residual >= 0.25, 1, 0)
  score <- score + ifelse(!is.na(outside_envelope_fraction) && outside_envelope_fraction >= 0.15, 1, 0)
  diagnostic_attention_class <- ifelse(score <= 1, "low_attention", ifelse(score <= 3, "moderate_attention", "high_attention"))
  data.frame(
    station_code = station_code_value,
    n_measurements = n_measurements,
    n_valid_measurements = n_valid_measurements,
    n_stage_zero_or_negative = n_stage_zero,
    pct_stage_zero_or_negative = ana_diag_safe_divide(n_stage_zero, n_measurements),
    n_discharge_zero_or_negative = n_discharge_zero,
    pct_discharge_zero_or_negative = ana_diag_safe_divide(n_discharge_zero, n_measurements),
    n_repeated_stage_variable_discharge_points = n_rep_stage,
    pct_repeated_stage_variable_discharge_points = ana_diag_safe_divide(n_rep_stage, n_measurements),
    n_repeated_discharge_variable_stage_points = n_rep_discharge,
    pct_repeated_discharge_variable_stage_points = ana_diag_safe_divide(n_rep_discharge, n_measurements),
    n_rating_curves = n_rating_curves,
    n_rating_curve_segments = n_rating_curve_segments,
    rating_match_fraction = rating_match_fraction,
    median_abs_rating_log_residual = median_abs_rating_log_residual,
    outside_residual_envelope_fraction = outside_envelope_fraction,
    n_temporal_regimes = n_temporal_regimes,
    temporal_regime_evidence_class = regime_evidence_class,
    baseline_power_equation = baseline_power_equation,
    baseline_power_h0_m = baseline_power_h0_m,
    baseline_power_a = baseline_power_a,
    baseline_power_b = baseline_power_b,
    diagnostic_attention_score = score,
    diagnostic_attention_class = diagnostic_attention_class,
    diagnostic_detail_level = ifelse(detailed, "detailed_station_level", "light_station_summary"),
    stringsAsFactors = FALSE
  )
}
ana_diag_make_diagnostic_indices <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) return(data.frame())
  out <- rbind(
    data.frame(
      station_code = summary$station_code,
      index_group = "Sinais nas medicoes",
      index_name = "Fracao de cotas <= 0",
      index_value = summary$pct_stage_zero_or_negative,
      index_unit = "fracao",
      index_class = ana_diag_class_percent(summary$pct_stage_zero_or_negative),
      index_description = "Fracao de medicoes de descarga com cota <= 0.",
      display_order = 10,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Sinais nas medicoes",
      index_name = "Fracao de vazoes <= 0",
      index_value = summary$pct_discharge_zero_or_negative,
      index_unit = "fracao",
      index_class = ana_diag_class_percent(summary$pct_discharge_zero_or_negative),
      index_description = "Fracao de medicoes de descarga com vazao <= 0.",
      display_order = 20,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Valores repetidos",
      index_name = "Fracao de cotas repetidas com vazao variavel",
      index_value = summary$pct_repeated_stage_variable_discharge_points,
      index_unit = "fracao",
      index_class = ana_diag_class_percent(summary$pct_repeated_stage_variable_discharge_points),
      index_description = "Fracao de medicoes em grupos de cota repetida com vazao variavel.",
      display_order = 30,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Valores repetidos",
      index_name = "Fracao de vazoes repetidas com cota variavel",
      index_value = summary$pct_repeated_discharge_variable_stage_points,
      index_unit = "fracao",
      index_class = ana_diag_class_percent(summary$pct_repeated_discharge_variable_stage_points),
      index_description = "Fracao de medicoes em grupos de vazao repetida com cota variavel.",
      display_order = 40,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Residuos da curva-chave",
      index_name = "Fracao pareada com curva-chave",
      index_value = summary$rating_match_fraction,
      index_unit = "fracao",
      index_class = ifelse(
        is.na(summary$rating_match_fraction),
        "not_available",
        ifelse(summary$rating_match_fraction >= 0.90, "high_coverage",
          ifelse(summary$rating_match_fraction >= 0.60, "moderate_coverage", "low_coverage")
        )
      ),
      index_description = "Fracao de medicoes validas pareadas a um segmento de curva-chave por data e cota.",
      display_order = 50,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Residuos da curva-chave",
      index_name = "Mediana do residuo log absoluto",
      index_value = summary$median_abs_rating_log_residual,
      index_unit = "razao logaritmica",
      index_class = ana_diag_class_residual(summary$median_abs_rating_log_residual),
      index_description = "Mediana do residuo logaritmico absoluto entre a vazao medida e a vazao estimada pela curva-chave.",
      display_order = 60,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Regimes temporais",
      index_name = "Evidencia de regimes temporais nos residuos",
      index_value = summary$n_temporal_regimes,
      index_unit = "numero de regimes",
      index_class = ifelse(is.na(summary$temporal_regime_evidence_class), "not_available", summary$temporal_regime_evidence_class),
      index_description = "Classe de evidencia da triagem de regimes temporais nos residuos, baseada em Q = a(H - h0)^b.",
      display_order = 70,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Resumo diagnostico",
      index_name = "Escore de atencao diagnostica",
      index_value = summary$diagnostic_attention_score,
      index_unit = "escore",
      index_class = summary$diagnostic_attention_class,
      index_description = "Escore preliminar de atencao para revisao visual. Nao e uma nota oficial de qualidade hidrologica.",
      display_order = 80,
      stringsAsFactors = FALSE
    )
  )
  out <- out[order(out$station_code, out$display_order), , drop = FALSE]
  rownames(out) <- NULL
  out
}
