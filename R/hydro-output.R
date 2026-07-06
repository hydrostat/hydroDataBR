# User-facing plotting and table dispatchers for hydroDataBR.
# These functions avoid source-specific names so future sources can reuse them.
utils::globalVariables(c("station_code", "variable", ".data"))
hydrodatabr_normalize_choice <- function(x, choices, aliases, arg = "value") {
  if (missing(x) || is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    stop("`", arg, "` must be supplied.", call. = FALSE)
  }
  x <- tolower(gsub("-", "_", as.character(x[[1]]), fixed = TRUE))
  if (x %in% names(aliases)) {
    x <- aliases[[x]]
  }
  if (!x %in% choices) {
    stop(
      "Unsupported `", arg, "`: ", x, ". Supported values are: ",
      paste(choices, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}
hydrodatabr_split_hydrometry_dots <- function(dots) {
  diagnostic_names <- c(
    "rating_curves", "measurements", "rating_curve_summary",
    "relative_error_threshold_pct", "use_internal_database"
  )
  if (length(dots) == 0 || is.null(names(dots))) {
    return(list(diagnostic = list(), output = dots))
  }
  named <- nzchar(names(dots))
  diagnostic <- dots[named & names(dots) %in% diagnostic_names]
  output <- dots[!named | !names(dots) %in% diagnostic_names]
  list(diagnostic = diagnostic, output = output)
}

hydrodatabr_is_data_frame <- function(x) {
  is.data.frame(x)
}
hydrodatabr_is_daily_frame <- function(x) {
  hydrodatabr_is_data_frame(x) && all(c("station_code", "date", "variable", "value") %in% names(x))
}
hydrodatabr_bind_rows_base <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  x <- x[vapply(x, is.data.frame, logical(1))]
  if (length(x) == 0) {
    return(data.frame())
  }
  x <- lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE))
  x <- x[vapply(x, nrow, integer(1)) > 0]
  if (length(x) == 0) {
    return(data.frame())
  }
  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(z) {
    missing_names <- setdiff(all_names, names(z))
    for (nm in missing_names) {
      z[[nm]] <- NA
    }
    z[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, x)
  row.names(out) <- NULL
  out
}
hydrodatabr_add_station_if_missing <- function(x, station_code) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(x)
  }
  if (!"station_code" %in% names(x) && !is.null(station_code) && nzchar(station_code)) {
    x$station_code <- as.character(station_code)
  }
  x
}
hydrodatabr_get_component <- function(x, names) {
  if (!is.list(x)) {
    return(NULL)
  }
  hit <- names[names %in% names(x)]
  if (length(hit) == 0) {
    return(NULL)
  }
  x[[hit[[1]]]]
}
hydrodatabr_extract_daily_data <- function(x) {
  if (hydrodatabr_is_daily_frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (!is.list(x)) {
    return(data.frame())
  }
  if ("results" %in% names(x) && is.list(x$results)) {
    pieces <- vector("list", length(x$results))
    result_names <- names(x$results)
    for (i in seq_along(x$results)) {
      piece <- hydrodatabr_extract_daily_data(x$results[[i]])
      if (length(result_names) >= i && !is.na(result_names[[i]]) && nzchar(result_names[[i]])) {
        piece <- hydrodatabr_add_station_if_missing(piece, result_names[[i]])
      }
      pieces[[i]] <- piece
    }
    return(hydrodatabr_bind_rows_base(pieces))
  }
  if (hydrodatabr_is_daily_frame(x$daily_data)) {
    return(as.data.frame(x$daily_data, stringsAsFactors = FALSE))
  }
  if (hydrodatabr_is_daily_frame(x$data)) {
    return(as.data.frame(x$data, stringsAsFactors = FALSE))
  }
  daily_names <- c("daily_discharge", "daily_stage", "daily_rainfall")
  pieces <- lapply(daily_names[daily_names %in% names(x)], function(nm) x[[nm]])
  pieces <- pieces[vapply(pieces, hydrodatabr_is_daily_frame, logical(1))]
  hydrodatabr_bind_rows_base(pieces)
}
hydrodatabr_extract_component_data <- function(x, component, fallback_data = TRUE) {
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (!is.list(x)) {
    return(data.frame())
  }
  if ("results" %in% names(x) && is.list(x$results)) {
    result_names <- names(x$results)
    pieces <- vector("list", length(x$results))
    for (i in seq_along(x$results)) {
      piece <- hydrodatabr_extract_component_data(x$results[[i]], component, fallback_data = FALSE)
      if (length(result_names) >= i && !is.na(result_names[[i]]) && nzchar(result_names[[i]])) {
        piece <- hydrodatabr_add_station_if_missing(piece, result_names[[i]])
      }
      pieces[[i]] <- piece
    }
    return(hydrodatabr_bind_rows_base(pieces))
  }
  component_names <- switch(
    component,
    daily = c("daily_data", "data"),
    measurements = c("discharge_measurements", "measurements"),
    rating_curves = c("rating_curves", "rating_curve_summary", "curves"),
    diagnostics = c("hydrometry_diagnostics", "daily_hydrometry_diagnostics", "diagnostics", "daily_flags"),
    stations = c("stations", "station_data", "station_inventory", "data"),
    character(0)
  )
  value <- hydrodatabr_get_component(x, component_names)
  if (is.data.frame(value)) {
    return(as.data.frame(value, stringsAsFactors = FALSE))
  }
  if (is.list(value) && !is.data.frame(value)) {
    data_value <- hydrodatabr_get_component(value, c("data", "daily", "diagnostics", "summary"))
    if (is.data.frame(data_value)) {
      return(as.data.frame(data_value, stringsAsFactors = FALSE))
    }
  }
  if (fallback_data && is.data.frame(x$data)) {
    return(as.data.frame(x$data, stringsAsFactors = FALSE))
  }
  data.frame()
}
hydrodatabr_filter_station <- function(x, station_code = NULL) {
  if (is.null(station_code) || !is.data.frame(x) || !"station_code" %in% names(x)) {
    return(x)
  }
  x[x$station_code %in% as.character(station_code), , drop = FALSE]
}
hydrodatabr_station_values <- function(x) {
  if (!is.data.frame(x) || !"station_code" %in% names(x)) {
    return(character(0))
  }
  sort(unique(as.character(x$station_code[!is.na(x$station_code) & nzchar(x$station_code)])))
}
hydrodatabr_check_multistation <- function(x, station_code = NULL, multi_station = NULL,
                                           default = "facet", max_facets = 12,
                                           plot_name = "plot") {
  x <- hydrodatabr_filter_station(x, station_code)
  stations <- hydrodatabr_station_values(x)
  if (length(stations) <= 1) {
    return(list(data = x, stations = stations, mode = "single"))
  }
  if (is.null(multi_station)) {
    multi_station <- default
  }
  multi_station <- match.arg(multi_station, c("facet", "list", "overlay", "error"))
  if (identical(multi_station, "error")) {
    stop(
      "`", plot_name, "` received data for multiple stations. Use `station_code` ",
      "to select one station or set `multi_station = \"list\"`.",
      call. = FALSE
    )
  }
  if (identical(multi_station, "facet") && length(stations) > max_facets) {
    stop(
      "Too many stations for faceted plotting (", length(stations), "). ",
      "Use `station_code` or set `multi_station = \"list\"`.",
      call. = FALSE
    )
  }
  list(data = x, stations = stations, mode = multi_station)
}
hydrodatabr_station_plot_list <- function(data, stations, fun, station_arg = "station_code", ...) {
  out <- vector("list", length(stations))
  names(out) <- stations
  dots <- list(...)
  for (st in stations) {
    dots[[station_arg]] <- st
    out[[st]] <- do.call(fun, c(list(data), dots))
  }
  out
}
hydrodatabr_facet_by_station <- function(p, data, variable = NULL) {
  if (!"station_code" %in% names(data)) {
    return(p)
  }
  vars <- if ("variable" %in% names(data)) {
    unique(as.character(data$variable[!is.na(data$variable)]))
  } else {
    character(0)
  }
  if (is.null(variable) && length(vars) > 1) {
    stop(
      "For faceted multi-station plots with multiple variables, use `variable` ",
      "to select one variable or set `multi_station = \"list\"`.",
      call. = FALSE
    )
  }
  p + ggplot2::facet_wrap(ggplot2::vars(.data[["station_code"]]), scales = "free_y")
}
hydrodatabr_dispatch_daily_plot <- function(data, plot, variable = NULL,
                                            station_code = NULL,
                                            multi_station = NULL,
                                            max_facets = 12,
                                            ...) {
  default_mode <- if (identical(plot, "daily_series")) "facet" else "facet"
  checked <- hydrodatabr_check_multistation(
    data,
    station_code = station_code,
    multi_station = multi_station,
    default = default_mode,
    max_facets = max_facets,
    plot_name = plot
  )
  data <- checked$data
  fun <- switch(
    plot,
    daily_series = plot_daily_series,
    daily_availability = plot_daily_availability,
    monthly_summary = plot_monthly_summary,
    annual_summary = plot_annual_summary,
    flow_duration = plot_flow_duration_curve
  )
  if (identical(checked$mode, "list")) {
    return(hydrodatabr_station_plot_list(
      data,
      checked$stations,
      fun,
      variable = variable,
      ...
    ))
  }
  if (identical(checked$mode, "overlay")) {
    if (!identical(plot, "daily_series")) {
      stop("`multi_station = \"overlay\"` is currently supported only for daily-series plots.", call. = FALSE)
    }
    return(plot_daily_series(
      data,
      variable = variable,
      station_code = NULL,
      color_by = "station_code",
      ...
    ))
  }
  p <- fun(data, variable = variable, station_code = station_code, ...)
  if (identical(checked$mode, "facet")) {
    p <- hydrodatabr_facet_by_station(p, data, variable = variable)
  }
  p
}
hydrodatabr_dispatch_single_station_plot <- function(data, plot, station_code = NULL,
                                                     type = NULL,
                                                     multi_station = NULL,
                                                     max_facets = 12,
                                                     ...) {
  checked <- hydrodatabr_check_multistation(
    data,
    station_code = station_code,
    multi_station = multi_station,
    default = "error",
    max_facets = max_facets,
    plot_name = plot
  )
  data <- checked$data
  fun <- switch(
    plot,
    measurements = plot_discharge_measurements,
    hydrometry_diagnostics = plot_daily_hydrometry_diagnostics
  )
  if (identical(checked$mode, "list")) {
    out <- vector("list", length(checked$stations))
    names(out) <- checked$stations
    for (st in checked$stations) {
      st_data <- data[data$station_code %in% st, , drop = FALSE]
      if (identical(plot, "measurements")) {
        out[[st]] <- fun(st_data, type = if (is.null(type)) "rating" else type, ...)
      } else {
        diagnostic_type <- type
        if (is.null(diagnostic_type)) {
          relative_error <- if ("relative_error_pct" %in% names(st_data)) {
            suppressWarnings(as.numeric(st_data$relative_error_pct))
          } else {
            numeric()
          }
          diagnostic_type <- if (any(!is.na(relative_error) & is.finite(relative_error))) {
            "relative_error"
          } else {
            "flags"
          }
        }
        out[[st]] <- fun(st_data, type = diagnostic_type, ...)
      }
    }
    return(out)
  }
  if (identical(plot, "measurement_diagnostics")) {
    result <- if (is.data.frame(data) && any(grepl("^flag_", names(data)))) {
      hydrodatabr_filter_station(data, station_code)
    } else {
      analyze_hydro_data(data, analysis = "measurement_diagnostics", station_code = station_code, ...)
    }
    return(plot_diagnostic_counts(result, ...))
  }
  if (identical(plot, "rating_diagnostics")) {
    result <- if (is.data.frame(data) && "relative_error_pct" %in% names(data)) {
      hydrodatabr_filter_station(data, station_code)
    } else {
      analyze_hydro_data(data, analysis = "rating_diagnostics", station_code = station_code, ...)
    }
    return(plot_rating_diagnostics_table(result, type = if (is.null(type)) "residual_discharge" else type, ...))
  }
  if (identical(plot, "measurements")) {
    return(fun(data, type = if (is.null(type)) "rating" else type, ...))
  }
  diagnostic_type <- type
  if (is.null(diagnostic_type)) {
    relative_error <- if ("relative_error_pct" %in% names(data)) {
      suppressWarnings(as.numeric(data$relative_error_pct))
    } else {
      numeric()
    }
    diagnostic_type <- if (any(!is.na(relative_error) & is.finite(relative_error))) {
      "relative_error"
    } else {
      "flags"
    }
  }
  fun(data, type = diagnostic_type, ...)
}
#' Gerar gráficos hidrológicos
#'
#' Gera gráficos a partir de séries diárias padronizadas, objetos de aquisição
#' do hydroDataBR ou resultados de análises. A função reúne os principais
#' gráficos usados na inspeção de séries, disponibilidade, regime, permanência,
#' extremos, chuva, medições de descarga, curvas-chave e seções transversais.
#' Para `plot = "hydrometry_diagnostics"`, séries diárias e objetos agregados
#' podem ser diagnosticados automaticamente com apoio da base hidrométrica
#' interna quando referências explícitas não forem fornecidas.
#'
#' @param data Objeto de dados. Pode ser uma série diária padronizada, um objeto
#'   retornado por `get_ana_data()`, um lote retornado por `get_ana_data_batch()`
#'   ou uma tabela/lista de resultados de análise.
#' @param plot Tipo de gráfico. Valores aceitos incluem `"daily_series"`,
#'   `"availability"`, `"monthly_summary"`, `"annual_summary"`,
#'   `"flow_duration"`, `"flow_indices"`, `"annual_maxima"`,
#'   `"low_flows"`, `"rainfall_indices"`,
#'   `"rainfall_annual_maxima"`, `"rainfall_monthly_boxplot"`,
#'   `"rainfall_diagnostics"`, `"measurements"`,
#'   `"measurement_diagnostics"`, `"rating_curves"`,
#'   `"rating_validity"`, `"rating_diagnostics"`,
#'   `"hydrometry_diagnostics"`, `"cross_section_profile"`,
#'   `"cross_section_overlay"` e `"cross_section_timeline"`.
#' @param variable Variável diária a filtrar, como `"discharge"`, `"stage"`
#'   ou `"rainfall"`.
#' @param station_code Código(s) de estação a filtrar.
#' @param type Subtipo usado por alguns gráficos, por exemplo em medições de
#'   descarga ou diagnósticos.
#' @param multi_station Estratégia para múltiplas estações: `"facet"`,
#'   `"list"`, `"overlay"` ou `"error"`. Quando `NULL`, a função escolhe
#'   um comportamento adequado ao gráfico.
#' @param max_facets Número máximo de estações em gráficos facetados.
#' @param ... Argumentos adicionais do gráfico escolhido, como `title`,
#'   `subtitle`, `log_y`, `section_id`, `section_date` ou `base_size`.
#'
#' @return Objeto `ggplot`. Em alguns casos de múltiplas estações, pode retornar
#'   uma lista nomeada de objetos `ggplot`.
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
#' plot_hydro_data(daily, plot = "daily_series")
#' plot_hydro_data(daily, plot = "flow_duration")
plot_hydro_data <- function(data,
                            plot = c(
                              "daily_series", "availability", "monthly_summary",
                              "annual_summary", "flow_duration", "rating_curves",
                              "rating_validity", "measurements",
                              "hydrometry_diagnostics",
                              "cross_section_profile",
                              "cross_section_overlay",
                              "cross_section_timeline",
                              "annual_maxima", "low_flows", "rainfall_indices",
                              "rainfall_annual_maxima", "rainfall_monthly_boxplot",
                              "rainfall_diagnostics", "measurement_diagnostics",
                              "rating_diagnostics"
                            ),
                            variable = NULL,
                            station_code = NULL,
                            type = NULL,
                            multi_station = NULL,
                            max_facets = 12,
                            ...) {
  # Stage 09F daily consistency preference - plot_hydro_data
  data <- hydro_prefer_daily_observations(data)
  plot_choices <- c(
    "daily_series", "daily_availability", "monthly_summary", "annual_summary",
    "flow_duration", "rating_curves", "rating_validity", "measurements",
    "hydrometry_diagnostics", "cross_section_profile", "cross_section_overlay",
    "cross_section_timeline",
    "annual_maxima", "low_flows", "rainfall_indices",
    "rainfall_annual_maxima", "rainfall_monthly_boxplot",
    "rainfall_diagnostics", "measurement_diagnostics", "rating_diagnostics"
  )
  plot_aliases <- c(
    daily = "daily_series",
    series = "daily_series",
    availability = "daily_availability",
    daily_availability = "daily_availability",
    monthly = "monthly_summary",
    annual = "annual_summary",
    fdc = "flow_duration",
    permanence = "flow_duration",
    rating = "rating_curves",
    rating_curve = "rating_curves",
    rating_curve_validity = "rating_validity",
    validity = "rating_validity",
    discharge_measurements = "measurements",
    measurement = "measurements",
    diagnostics = "hydrometry_diagnostics",
    daily_hydrometry_diagnostics = "hydrometry_diagnostics",
    cross_sections = "cross_section_profile",
    cross_section = "cross_section_profile",
    section_profile = "cross_section_profile",
    profile = "cross_section_profile",
    cross_section_overlay = "cross_section_overlay",
    section_overlay = "cross_section_overlay",
    overlay_sections = "cross_section_overlay",
    cross_section_timeline = "cross_section_timeline",
    section_timeline = "cross_section_timeline",
    timeline_sections = "cross_section_timeline",
    maxima = "annual_maxima",
    annual_max = "annual_maxima",
    low_flow = "low_flows",
    minimum_flows = "low_flows",
    rain_indices = "rainfall_indices",
    precipitation_indices = "rainfall_indices",
    rainfall_maxima = "rainfall_annual_maxima",
    rainfall_annual_max = "rainfall_annual_maxima",
    rainfall_boxplot = "rainfall_monthly_boxplot",
    rain_boxplot = "rainfall_monthly_boxplot",
    rain_diagnostics = "rainfall_diagnostics",
    precipitation_diagnostics = "rainfall_diagnostics",
    measurement_diagnostics = "measurement_diagnostics",
    measurement_flags = "measurement_diagnostics",
    rating_diagnostics = "rating_diagnostics",
    rating_curve_diagnostics = "rating_diagnostics"
  )
  plot <- hydrodatabr_normalize_choice(plot[[1]], plot_choices, plot_aliases, arg = "plot")
  if (plot %in% c("daily_series", "daily_availability", "monthly_summary", "annual_summary", "flow_duration")) {
    daily <- hydrodatabr_extract_daily_data(data)
    if (nrow(daily) == 0) {
      stop("No standardized daily data found in `data`.", call. = FALSE)
    }
    return(hydrodatabr_dispatch_daily_plot(
      daily,
      plot = plot,
      variable = variable,
      station_code = station_code,
      multi_station = multi_station,
      max_facets = max_facets,
      ...
    ))
  }
  if (plot %in% c("annual_maxima", "rainfall_annual_maxima", "low_flows", "rainfall_indices", "rainfall_monthly_boxplot", "rainfall_diagnostics")) {
    if (identical(plot, "rainfall_monthly_boxplot")) {
      daily <- hydrodatabr_extract_daily_data(data)
      return(plot_rainfall_monthly_boxplot(
        hydrodatabr_filter_station(daily, station_code),
        ...
      ))
    }
    analysis <- switch(
      plot,
      annual_maxima = "annual_maxima",
      rainfall_annual_maxima = "rainfall_annual_maxima",
      low_flows = "low_flows",
      rainfall_indices = "rainfall_indices",
      rainfall_diagnostics = "rainfall_diagnostics"
    )
    result <- if (is.data.frame(data) && (
      (plot %in% c("annual_maxima", "rainfall_annual_maxima") && "max_value" %in% names(data)) ||
        (identical(plot, "low_flows") && "min_value" %in% names(data)) ||
        (identical(plot, "rainfall_indices") && "rx1day" %in% names(data)) ||
        (identical(plot, "rainfall_diagnostics") && any(grepl("^flag_", names(data))))
    )) {
      hydrodatabr_filter_station(data, station_code)
    } else {
      analyze_hydro_data(data, analysis = analysis, variable = variable, station_code = station_code, ...)
    }
    dots <- list(...)
    analysis_arg_names <- c(
      "year_start_month", "complete_years_only", "valid_fraction_threshold",
      "valid_window_fraction_threshold", "gap_window_days", "repeated_digits",
      "durations", "very_high_threshold", "zero_run_length",
      "repeated_value_min_count", "robust_mad_multiplier", "flagged_only"
    )
    plot_dots <- dots
    if (length(plot_dots) > 0 && !is.null(names(plot_dots))) {
      plot_dots <- plot_dots[!names(plot_dots) %in% analysis_arg_names]
    }
    if (identical(plot, "low_flows")) {
      return(do.call(plot_extreme_low_flows, c(list(data = result), plot_dots)))
    }
    if (plot %in% c("annual_maxima", "rainfall_annual_maxima")) {
      return(do.call(plot_extreme_annual_maxima, c(list(data = result), plot_dots)))
    }
    if (identical(plot, "rainfall_indices")) {
      return(do.call(plot_rainfall_indices_table, c(list(data = result, type = if (is.null(type)) "depth" else type), plot_dots)))
    }
    return(do.call(plot_diagnostic_counts, c(list(data = result), plot_dots)))
  }
  if (identical(plot, "rating_curves")) {
    curves <- hydrodatabr_filter_station(
      hydrodatabr_extract_component_data(data, "rating_curves"),
      station_code
    )
    if (nrow(curves) == 0) {
      stop("No rating-curve data found in `data`.", call. = FALSE)
    }
    checked <- hydrodatabr_check_multistation(
      curves,
      station_code = NULL,
      multi_station = multi_station,
      default = "error",
      max_facets = max_facets,
      plot_name = plot
    )
    curves <- checked$data
    measurements <- hydrodatabr_filter_station(
      hydrodatabr_extract_component_data(data, "measurements"),
      station_code
    )
    if (identical(checked$mode, "list")) {
      out <- vector("list", length(checked$stations))
      names(out) <- checked$stations
      for (st in checked$stations) {
        out[[st]] <- plot_rating_curves(
          curves[curves$station_code %in% st, , drop = FALSE],
          measurements = if ("station_code" %in% names(measurements)) measurements[measurements$station_code %in% st, , drop = FALSE] else measurements,
          ...
        )
      }
      return(out)
    }
    return(plot_rating_curves(curves, measurements = measurements, ...))
  }
  if (identical(plot, "rating_validity")) {
    curves <- hydrodatabr_extract_component_data(data, "rating_curves")
    if (nrow(curves) == 0) {
      stop("No rating-curve data found in `data`.", call. = FALSE)
    }
    curves <- hydrodatabr_filter_station(curves, station_code)
    checked <- hydrodatabr_check_multistation(
      curves,
      station_code = NULL,
      multi_station = multi_station,
      default = "error",
      max_facets = max_facets,
      plot_name = plot
    )
    curves <- checked$data
    if (identical(checked$mode, "list")) {
      out <- vector("list", length(checked$stations))
      names(out) <- checked$stations
      for (st in checked$stations) {
        out[[st]] <- plot_rating_curve_validity(curves[curves$station_code %in% st, , drop = FALSE], ...)
      }
      return(out)
    }
    return(plot_rating_curve_validity(curves, ...))
  }
  if (plot %in% c("cross_section_profile", "cross_section_overlay", "cross_section_timeline")) {
    cross_sections <- hydrodatabr_extract_cross_sections(data)
    cross_sections <- hydrodatabr_filter_cross_sections(cross_sections, station_code)
    vertices <- hydrodatabr_prepare_cross_section_vertices(cross_sections)
    sections <- hydrodatabr_prepare_cross_sections_table(cross_sections)
    check_data <- if (nrow(vertices) > 0) vertices else sections
    if (nrow(check_data) == 0) {
      stop("No cross-section data found in `data`.", call. = FALSE)
    }
    checked <- hydrodatabr_check_multistation(
      check_data,
      station_code = NULL,
      multi_station = multi_station,
      default = "error",
      max_facets = max_facets,
      plot_name = plot
    )
    plot_type <- switch(
      plot,
      cross_section_profile = "profile",
      cross_section_overlay = "overlay",
      cross_section_timeline = "timeline"
    )
    if (identical(checked$mode, "list")) {
      out <- vector("list", length(checked$stations))
      names(out) <- checked$stations
      for (st in checked$stations) {
        out[[st]] <- plot_cross_sections(
          hydrodatabr_filter_cross_sections(cross_sections, st),
          type = plot_type,
          ...
        )
      }
      return(out)
    }
    return(plot_cross_sections(cross_sections, type = plot_type, ...))
  }
  if (identical(plot, "measurement_diagnostics")) {
    result <- if (is.data.frame(data) && any(grepl("^flag_", names(data)))) {
      hydrodatabr_filter_station(data, station_code)
    } else {
      analyze_hydro_data(data, analysis = "measurement_diagnostics", station_code = station_code, ...)
    }
    return(plot_diagnostic_counts(result, ...))
  }
  if (identical(plot, "rating_diagnostics")) {
    result <- if (is.data.frame(data) && "relative_error_pct" %in% names(data)) {
      hydrodatabr_filter_station(data, station_code)
    } else {
      analyze_hydro_data(data, analysis = "rating_diagnostics", station_code = station_code, ...)
    }
    return(plot_rating_diagnostics_table(result, type = if (is.null(type)) "residual_discharge" else type, ...))
  }
  if (identical(plot, "measurements")) {
    measurements <- hydrodatabr_extract_component_data(data, "measurements")
    if (nrow(measurements) == 0) {
      stop("No discharge-measurement data found in `data`.", call. = FALSE)
    }
    return(hydrodatabr_dispatch_single_station_plot(
      measurements,
      plot = plot,
      station_code = station_code,
      type = type,
      multi_station = multi_station,
      max_facets = max_facets,
      ...
    ))
  }
  dots <- hydrodatabr_split_hydrometry_dots(list(...))
  diagnostics <- hydrodatabr_extract_component_data(data, "diagnostics")
  if (nrow(diagnostics) > 0 && nrow(table_daily_hydrometry_diagnostics(diagnostics)) == 0) {
    diagnostics <- data.frame()
  }
  if (nrow(diagnostics) == 0 && is.list(data) && is.data.frame(data$daily_flags)) {
    diagnostics <- data$daily_flags
  }
  if (nrow(diagnostics) == 0) {
    daily_candidate <- ana_hydro_ref_extract_daily_data(data)
    if (nrow(daily_candidate) > 0) {
      diagnostic_result <- do.call(
        diagnose_daily_hydrometry,
        c(list(daily_data = data, station_code = station_code), dots$diagnostic)
      )
      diagnostics <- diagnostic_result$daily_flags
    }
  }
  if (nrow(diagnostics) == 0 && is.data.frame(data)) {
    diagnostics <- data
  }
  if (nrow(diagnostics) == 0 || nrow(table_daily_hydrometry_diagnostics(diagnostics)) == 0) {
    stop("No hydrometry-diagnostic data found in `data`.", call. = FALSE)
  }
  do.call(
    hydrodatabr_dispatch_single_station_plot,
    c(
      list(
        data = diagnostics,
        plot = "hydrometry_diagnostics",
        station_code = station_code,
        type = type,
        multi_station = multi_station,
        max_facets = max_facets
      ),
      dots$output
    )
  )
}
#' Gerar tabelas hidrológicas
#'
#' Organiza séries, análises, diagnósticos e relatórios do hydroDataBR em
#' tabelas prontas para inspeção ou exportação. A função aceita tanto séries
#' diárias padronizadas quanto objetos retornados por `get_ana_data()` e
#' `get_ana_data_batch()`. Para `table = "hydrometry_diagnostics"`, séries
#' diárias e objetos agregados podem ser diagnosticados automaticamente com
#' apoio da base hidrométrica interna quando referências explícitas não forem
#' fornecidas.
#'
#' @param data Objeto de dados. Pode ser série diária padronizada, objeto de
#'   aquisição, lote de aquisição, diagnóstico ou relatório.
#' @param table Tipo de tabela. Valores aceitos incluem
#'   `"daily_availability"`, `"daily_statistics"`, `"flow_duration"`,
#'   `"flow_indices"`, `"monthly_flow_indices"`, `"annual_maxima"`,
#'   `"low_flows"`, `"rainfall_indices"`,
#'   `"rainfall_annual_maxima"`, `"rainfall_diagnostics"`,
#'   `"daily_gap_summary"`, `"station_products"`, `"request_report"`,
#'   `"hydrometry_diagnostics"`, `"measurement_diagnostics"`,
#'   `"rating_diagnostics"` e `"cross_sections"`.
#' @param variable Variável diária a filtrar.
#' @param station_code Código(s) de estação a filtrar.
#' @param period Período usado em tabelas diárias, como `"annual"`,
#'   `"monthly"` ou `"monthly_regime"`, conforme a tabela solicitada.
#' @param ... Argumentos adicionais usados pela tabela escolhida.
#'
#' @return `data.frame` com a tabela solicitada.
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
#' table_hydro_data(daily, table = "daily_statistics")
#' table_hydro_data(daily, table = "daily_availability")
table_hydro_data <- function(data,
                             table = c(
                               "daily_availability", "daily_statistics",
                               "flow_duration", "flow_indices",
                               "rainfall_indices", "annual_maxima",
                               "low_flows", "station_products",
                               "request_report", "hydrometry_diagnostics",
                               "cross_sections", "monthly_flow_indices",
                               "rainfall_annual_maxima", "rainfall_diagnostics",
                               "daily_gap_summary", "measurement_diagnostics",
                               "rating_diagnostics"
                             ),
                             variable = NULL,
                             station_code = NULL,
                             period = NULL,
                             ...) {
  # Stage 09F daily consistency preference - table_hydro_data
  data <- hydro_prefer_daily_observations(data)
  table_choices <- c(
    "daily_availability", "daily_statistics", "flow_duration",
    "flow_indices", "rainfall_indices", "annual_maxima", "low_flows",
    "station_products", "request_report", "hydrometry_diagnostics",
    "cross_sections", "monthly_flow_indices",
    "rainfall_annual_maxima", "rainfall_diagnostics",
    "daily_gap_summary", "measurement_diagnostics", "rating_diagnostics"
  )
  table_aliases <- c(
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
    products = "station_products",
    station_products = "station_products",
    report = "request_report",
    diagnostics = "hydrometry_diagnostics",
    daily_hydrometry_diagnostics = "hydrometry_diagnostics",
    cross_section = "cross_sections",
    cross_section_summary = "cross_sections",
    sections = "cross_sections",
    rain_diagnostics = "rainfall_diagnostics",
    precipitation_diagnostics = "rainfall_diagnostics",
    gaps = "daily_gap_summary",
    gap_summary = "daily_gap_summary",
    measurement_flags = "measurement_diagnostics",
    rating = "rating_diagnostics",
    rating_curve_diagnostics = "rating_diagnostics"
  )
  table <- hydrodatabr_normalize_choice(table[[1]], table_choices, table_aliases, arg = "table")
  if (identical(table, "request_report")) {
    return(table_request_report(data))
  }
  if (identical(table, "station_products")) {
    stations <- if (is.data.frame(data)) {
      data
    } else {
      hydrodatabr_extract_component_data(data, "stations")
    }
    if (nrow(stations) == 0) {
      return(table_station_products(station_code = station_code, ...))
    }
    return(table_station_products(stations = stations, station_code = station_code, ...))
  }
  if (identical(table, "hydrometry_diagnostics")) {
    dots <- hydrodatabr_split_hydrometry_dots(list(...))
    diagnostics <- hydrodatabr_extract_component_data(data, "diagnostics")
    if (nrow(diagnostics) > 0 && nrow(table_daily_hydrometry_diagnostics(diagnostics)) == 0) {
      diagnostics <- data.frame()
    }
    if (nrow(diagnostics) == 0 && is.list(data) && is.data.frame(data$daily_flags)) {
      diagnostics <- data$daily_flags
    }
    if (nrow(diagnostics) == 0) {
      daily_candidate <- ana_hydro_ref_extract_daily_data(data)
      if (nrow(daily_candidate) > 0) {
        diagnostics <- do.call(
          diagnose_daily_hydrometry,
          c(list(daily_data = data, station_code = station_code), dots$diagnostic)
        )
      }
    }
    if (is.data.frame(diagnostics)) {
      diagnostics <- hydrodatabr_filter_station(diagnostics, station_code)
    }
    return(table_daily_hydrometry_diagnostics(diagnostics))
  }
  if (identical(table, "cross_sections")) {
    cross_sections <- hydrodatabr_extract_cross_sections(data)
    return(table_cross_sections(cross_sections, station_code = station_code, ...))
  }
  analysis_tables <- c(
    "flow_duration", "flow_indices", "monthly_flow_indices", "rainfall_indices",
    "annual_maxima", "rainfall_annual_maxima", "low_flows", "rainfall_diagnostics",
    "daily_gap_summary", "measurement_diagnostics", "rating_diagnostics"
  )
  if (table %in% analysis_tables) {
    return(analyze_hydro_data(
      data,
      analysis = table,
      variable = variable,
      station_code = station_code,
      ...
    ))
  }
  daily <- hydrodatabr_extract_daily_data(data)
  if (nrow(daily) == 0) {
    stop("No standardized daily data found in `data`.", call. = FALSE)
  }
  if (identical(table, "daily_availability")) {
    if (is.null(period)) {
      period <- "annual"
    }
    return(table_daily_availability(
      daily,
      period = period,
      variable = variable,
      station_code = station_code,
      ...
    ))
  }
  if (is.null(period)) {
    period <- "annual"
  }
  table_daily_statistics(
    daily,
    period = period,
    variable = variable,
    station_code = station_code,
    ...
  )
}
