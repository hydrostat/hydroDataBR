# Plotting helpers for hydroDataBR.
# Functions in this file are deterministic and do not call ANA services.
utils::globalVariables(c(
  ".data", "date", "value", "variable", "year", "month", "month_label",
  "mean_value", "total_value", "permanence_pct", "discharge_m3s",
  "stage_cm", "curve_label", "curve_segment_label", "diagnostic_flag",
  "relative_error_pct", "n_flagged", "issue", ".plot_group",
  "failure_class", "flagged", "availability_pct", "missing_pct",
  "year_factor", "valid_from", "valid_to_plot", "validity_label",
  "q_lower_m3s", "q_upper_m3s", "interval_label"
))
hydrodatabr_month_labels <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)
hydrodatabr_first_existing_name <- function(data, candidates) {
  candidates <- as.character(candidates)
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0) {
    return(NA_character_)
  }
  hit[[1]]
}
hydrodatabr_as_data_frame <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  }
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  stop("`data` must be a data frame or tibble.", call. = FALSE)
}
hydrodatabr_as_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  as.Date(x)
}
hydrodatabr_empty_plot_data <- function() {
  data.frame()
}
hydrodatabr_is_daily_contract <- function(data) {
  all(c("station_code", "date", "variable", "value") %in% names(data))
}
hydrodatabr_prepare_daily <- function(data, variable = NULL, station_code = NULL,
                                      start_date = NULL, end_date = NULL) {
  data <- hydrodatabr_as_data_frame(data)
  required <- c("station_code", "date", "variable", "value")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "`data` must contain columns: ", paste(required, collapse = ", "),
      call. = FALSE
    )
  }
  data$station_code <- as.character(data$station_code)
  data$variable <- as.character(data$variable)
  data$date <- as.Date(data$date)
  data$value <- suppressWarnings(as.numeric(data$value))
  if (!"unit" %in% names(data)) {
    data$unit <- NA_character_
  } else {
    data$unit <- as.character(data$unit)
  }
  data <- data[!is.na(data$date), , drop = FALSE]
  if (!is.null(variable)) {
    data <- data[data$variable %in% as.character(variable), , drop = FALSE]
  }
  if (!is.null(station_code)) {
    data <- data[data$station_code %in% as.character(station_code), , drop = FALSE]
  }
  if (!is.null(start_date)) {
    data <- data[data$date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date)) {
    data <- data[data$date <= as.Date(end_date), , drop = FALSE]
  }
  data[order(data$station_code, data$variable, data$date), , drop = FALSE]
}
hydrodatabr_unit_label <- function(unit) {
  unit <- as.character(unit)
  unit <- if (length(unit) == 0) NA_character_ else unit[[1]]
  if (is.na(unit) || !nzchar(unit)) {
    return(NA_character_)
  }
  switch(
    unit,
    "m3/s" = "m\u00b3/s",
    "m^3/s" = "m\u00b3/s",
    "m3.s-1" = "m\u00b3/s",
    "m/s" = "m/s",
    "m2" = "m\u00b2",
    "m2/s" = "m\u00b2/s",
    unit
  )
}
hydrodatabr_variable_name_label <- function(variable) {
  variable <- as.character(variable)
  variable <- if (length(variable) == 0) NA_character_ else variable[[1]]
  switch(
    variable,
    discharge = "Vaz\u00e3o",
    stage = "Cota",
    rainfall = "Precipita\u00e7\u00e3o",
    variable
  )
}
hydrodatabr_variable_labels <- function(values) {
  labels <- vapply(as.character(values), hydrodatabr_variable_name_label, character(1))
  unname(labels)
}
hydrodatabr_variable_label <- function(variable, unit = NA_character_) {
  label <- hydrodatabr_variable_name_label(variable)
  unit_label <- hydrodatabr_unit_label(unit)
  if (!is.na(unit_label) && nzchar(unit_label)) {
    label <- paste0(label, " (", unit_label, ")")
  }
  label
}
hydrodatabr_summary_axis_label <- function(data, statistic, value_col, variable = NULL) {
  if (is.null(variable) && "variable" %in% names(data)) {
    values <- unique(as.character(data$variable[!is.na(data$variable) & nzchar(data$variable)]))
    if (length(values) == 1) {
      variable <- values[[1]]
    }
  }
  if (is.null(variable)) {
    variable <- NA_character_
  }
  unit <- NA_character_
  if ("unit" %in% names(data)) {
    units <- unique(as.character(data$unit[!is.na(data$unit) & nzchar(data$unit)]))
    if (length(units) == 1) {
      unit <- units[[1]]
    }
  }
  unit_label <- hydrodatabr_unit_label(unit)
  value_label <- switch(
    value_col,
    mean_value = "m\u00e9dia",
    total_value = "total",
    mean_total_value = "total m\u00e9dia",
    min_value = "m\u00ednima",
    max_value = "m\u00e1xima",
    statistic
  )
  if (!is.na(variable) && identical(variable, "discharge")) {
    label <- switch(
      value_col,
      mean_value = "Vaz\u00e3o m\u00e9dia",
      total_value = "Vaz\u00e3o total",
      min_value = "Vaz\u00e3o m\u00ednima",
      max_value = "Vaz\u00e3o m\u00e1xima",
      paste("Vaz\u00e3o", value_label)
    )
  } else if (!is.na(variable) && identical(variable, "rainfall")) {
    label <- switch(
      value_col,
      mean_value = "Precipita\u00e7\u00e3o m\u00e9dia di\u00e1ria",
      total_value = "Precipita\u00e7\u00e3o total",
      mean_total_value = "Precipita\u00e7\u00e3o total mensal m\u00e9dia",
      min_value = "Precipita\u00e7\u00e3o m\u00ednima",
      max_value = "Precipita\u00e7\u00e3o m\u00e1xima",
      paste("Precipita\u00e7\u00e3o", value_label)
    )
  } else if (!is.na(variable) && identical(variable, "stage")) {
    label <- switch(
      value_col,
      mean_value = "Cota m\u00e9dia",
      total_value = "Cota total",
      min_value = "Cota m\u00ednima",
      max_value = "Cota m\u00e1xima",
      paste("Cota", value_label)
    )
  } else {
    label <- switch(
      value_col,
      mean_value = "Valor m\u00e9dio",
      total_value = "Total",
      mean_total_value = "Total m\u00e9dio",
      min_value = "Valor m\u00ednimo",
      max_value = "Valor m\u00e1ximo",
      value_col
    )
  }
  if (!is.na(unit_label) && nzchar(unit_label)) {
    label <- paste0(label, " (", unit_label, ")")
  }
  label
}
hydrodatabr_diagnostic_issue_label <- function(issue) {
  issue <- as.character(issue)
  issue <- if (length(issue) == 0) NA_character_ else issue[[1]]
  switch(
    issue,
    outside_validity_flag = "Fora da validade da curva",
    outside_curve_flag = "Fora da faixa da curva",
    stage_below_curve_flag = "Cota abaixo da curva",
    stage_above_curve_flag = "Cota acima da curva",
    missing_stage_flag = "Cota ausente",
    missing_discharge_flag = "Vaz\u00e3o ausente",
    generated_discharge_missing_flag = "Vaz\u00e3o gerada ausente",
    relative_error_flag = "Erro relativo elevado",
    zero_stage_flag = "Cota igual a zero",
    zero_discharge_flag = "Vaz\u00e3o igual a zero",
    issue
  )
}
hydrodatabr_diagnostic_issue_labels <- function(values) {
  labels <- vapply(as.character(values), hydrodatabr_diagnostic_issue_label, character(1))
  unname(labels)
}
hydrodatabr_number_labels <- function() {
  scales::label_number(decimal.mark = ",", big.mark = ".")
}
hydrodatabr_year_breaks <- function(years, max_breaks = 12) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]
  if (length(years) == 0) {
    return(integer(0))
  }
  if (length(years) <= max_breaks) {
    return(years)
  }
  step <- ceiling(length(years) / max_breaks)
  breaks <- years[seq(1, length(years), by = step)]
  last_year <- years[[length(years)]]
  if (!last_year %in% breaks) {
    breaks <- c(breaks, last_year)
  }
  breaks
}
hydrodatabr_plot_palette <- function(labels, palette = "Dark 3") {
  labels <- unique(as.character(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0) {
    return(character(0))
  }
  colors <- grDevices::hcl.colors(length(labels), palette = palette)
  names(colors) <- labels
  colors
}
#' Tema grafico interno do hydroDataBR
#'
#' Tema base usado internamente pelos graficos produzidos pelo pacote.
#' A funcao nao faz parte da interface publica; usuarios podem customizar
#' os objetos `ggplot` retornados por [plot_hydro_data()] com camadas
#' adicionais do `ggplot2`.
#'
#' @param base_size Tamanho base da fonte.
#' @param base_family Familia tipografica base.
#' @return Um objeto de tema do `ggplot2`.
#' @noRd
theme_hydrodatabr <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(size = base_size),
      plot.caption = ggplot2::element_text(size = base_size - 2, color = "grey35"),
      axis.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_line(linewidth = 0.25),
      panel.grid.major = ggplot2::element_line(linewidth = 0.45),
      axis.line = ggplot2::element_line(linewidth = 0.35, color = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.25, color = "black")
    )
}
#' Grafico de serie diaria
#'
#' Gera um grafico de serie temporal a partir do contrato diario padronizado do `hydroDataBR`.
#'
#' @param data Tabela no contrato diario padronizado.
#' @param variable Variavel a filtrar. Use `NULL` para manter todas.
#' @param station_code Codigo da estacao a filtrar. Use `NULL` para manter todas.
#' @param start_date Data inicial opcional.
#' @param end_date Data final opcional.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param color_by Coluna usada para colorir linhas ou pontos.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:9,
#'   variable = "discharge",
#'   value = c(1:5, NA, 7:10),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' plot_daily_series(daily, variable = "discharge")
#' @noRd
plot_daily_series <- function(data, variable = NULL, station_code = NULL,
                              start_date = NULL, end_date = NULL,
                              title = NULL, subtitle = NULL,
                              color_by = NULL, base_size = 11) {
  plot_data <- hydrodatabr_prepare_daily(
    data = data,
    variable = variable,
    station_code = station_code,
    start_date = start_date,
    end_date = end_date
  )
  if (nrow(plot_data) == 0) {
    stop("No daily data available for plotting.", call. = FALSE)
  }
  variables <- unique(plot_data$variable)
  units <- unique(plot_data$unit[!is.na(plot_data$unit) & nzchar(plot_data$unit)])
  y_label <- if (length(variables) == 1) {
    unit <- if (length(units) == 1) units[[1]] else NA_character_
    hydrodatabr_variable_label(variables[[1]], unit)
  } else {
    "Valor"
  }
  if (is.null(color_by) && length(variables) > 1) {
    color_by <- "variable"
  }
  if (!is.null(color_by) && !color_by %in% names(plot_data)) {
    stop("`color_by` must be a column in `data`.", call. = FALSE)
  }
  if (is.null(color_by)) {
    plot_data$.plot_group <- paste(plot_data$station_code, plot_data$variable, sep = "-")
  } else {
    plot_data$.plot_group <- paste(plot_data$station_code, plot_data$variable, plot_data[[color_by]], sep = "-")
  }
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["date"]], y = .data[["value"]]))
  rainfall_only <- length(variables) == 1 && identical(variables[[1]], "rainfall")
  if (rainfall_only && is.null(color_by)) {
    p <- p + ggplot2::geom_col(width = 1)
  } else if (rainfall_only) {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data[[color_by]]), width = 1)
  } else if (is.null(color_by)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(group = .data[[".plot_group"]]),
      linewidth = 0.45, lineend = "round", linejoin = "round"
    )
  } else {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(color = .data[[color_by]], group = .data[[".plot_group"]]),
      linewidth = 0.45, lineend = "round", linejoin = "round"
    )
  }
  if (length(variables) > 1) {
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(.data[["variable"]]),
      scales = "free_y",
      ncol = 1,
      labeller = ggplot2::as_labeller(function(x) hydrodatabr_variable_labels(x))
    )
  }
  if (identical(color_by, "variable")) {
    color_labels <- sort(unique(as.character(plot_data$variable)))
    color_palette <- hydrodatabr_plot_palette(color_labels)
    if (rainfall_only) {
      p <- p + ggplot2::scale_fill_manual(
        values = color_palette,
        breaks = color_labels,
        labels = hydrodatabr_variable_labels(color_labels),
        drop = FALSE
      )
    } else {
      p <- p + ggplot2::scale_color_manual(
        values = color_palette,
        breaks = color_labels,
        labels = hydrodatabr_variable_labels(color_labels),
        drop = FALSE
      )
    }
  }
  p +
    ggplot2::scale_x_date(name = "Data") +
    ggplot2::scale_y_continuous(name = y_label, labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle, color = NULL, fill = NULL) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
}
hydrodatabr_summary_value_column <- function(data, statistic = c("mean", "total", "min", "max"),
                                             value_col = NULL) {
  if (!is.null(value_col)) {
    if (!value_col %in% names(data)) {
      stop("`value_col` must be a column in `data`.", call. = FALSE)
    }
    return(value_col)
  }
  statistic <- match.arg(statistic)
  candidates <- switch(
    statistic,
    mean = c("mean_value", "q_mean_m3s", "mean_discharge_m3s", "mean_stage_cm"),
    total = c("total_value", "total_mm", "rainfall_total_mm"),
    min = c("min_value", "q_min_m3s"),
    max = c("max_value", "q_max_m3s", "rx1day_mm")
  )
  column <- hydrodatabr_first_existing_name(data, candidates)
  if (is.na(column)) {
    stop("Could not identify a summary value column.", call. = FALSE)
  }
  column
}
hydrodatabr_failure_classes <- function(missing_pct) {
  missing_pct <- suppressWarnings(as.numeric(missing_pct))
  out <- ifelse(
    missing_pct == 100, "100%",
    ifelse(
      missing_pct >= 75 & missing_pct < 100, "75-<100%",
      ifelse(
        missing_pct >= 50 & missing_pct < 75, "50-<75%",
        ifelse(
          missing_pct >= 25 & missing_pct < 50, "25-<50%",
          ifelse(missing_pct > 0 & missing_pct < 25, "0-<25%", "0%")
        )
      )
    )
  )
  out[is.na(missing_pct)] <- NA_character_
  factor(out, levels = c("100%", "75-<100%", "50-<75%", "25-<50%", "0-<25%", "0%"))
}
hydrodatabr_failure_colors <- function() {
  c(
    "100%" = "#d53e4f",
    "75-<100%" = "#fc8d59",
    "50-<75%" = "#fee08b",
    "25-<50%" = "#e6f598",
    "0-<25%" = "#99d594",
    "0%" = "#3288bd"
  )
}
hydrodatabr_failure_labels <- function() {
  c(
    "100%" = "100%",
    "75-<100%" = "75% a <100%",
    "50-<75%" = "50% a <75%",
    "25-<50%" = "25% a <50%",
    "0-<25%" = ">0% a <25%",
    "0%" = "0%"
  )
}
#' Grafico de disponibilidade diaria
#'
#' Gera um grafico mensal em blocos para avaliar a disponibilidade de dados diarios. O eixo x mostra os anos, o eixo y mostra os meses e as cores indicam o percentual de falhas em cada mes.
#'
#' @param data Tabela no contrato diario padronizado.
#' @param variable Variavel a filtrar. Use `NULL` para manter todas.
#' @param station_code Codigo da estacao a filtrar. Use `NULL` para manter todas.
#' @param start_date Data inicial opcional.
#' @param end_date Data final opcional.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:9,
#'   variable = "discharge",
#'   value = c(1:5, NA, 7:10),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' plot_daily_availability(daily, variable = "discharge")
#' @noRd
plot_daily_availability <- function(data, variable = NULL, station_code = NULL,
                                    start_date = NULL, end_date = NULL,
                                    title = NULL, subtitle = NULL,
                                    base_size = 11) {
  plot_data <- table_daily_availability(
    data = data,
    period = "monthly",
    variable = variable,
    station_code = station_code,
    start_date = start_date,
    end_date = end_date
  )
  if (nrow(plot_data) == 0) {
    stop("No daily availability data available for plotting.", call. = FALSE)
  }
  plot_data$year <- as.integer(plot_data$year)
  plot_data$month <- as.integer(plot_data$month)
  plot_data$failure_class <- hydrodatabr_failure_classes(plot_data$missing_pct)
  plot_data <- plot_data[!is.na(plot_data$year) & !is.na(plot_data$month), , drop = FALSE]
  plot_data <- plot_data[plot_data$month >= 1 & plot_data$month <= 12, , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("No finite monthly availability values available for plotting.", call. = FALSE)
  }
  year_values <- sort(unique(plot_data$year))
  year_breaks <- hydrodatabr_year_breaks(year_values)
  year_levels <- as.character(year_values)
  plot_data$year_factor <- factor(as.character(plot_data$year), levels = year_levels)
  plot_data$month_label <- factor(
    hydrodatabr_month_labels[plot_data$month],
    levels = hydrodatabr_month_labels
  )
  if ("variable" %in% names(plot_data) && length(unique(plot_data$variable)) > 1) {
    plot_data$variable_label <- hydrodatabr_variable_labels(plot_data$variable)
    facet_layer <- ggplot2::facet_wrap(ggplot2::vars(.data[["variable_label"]]), ncol = 1)
  } else {
    facet_layer <- NULL
  }
  legend_levels <- names(hydrodatabr_failure_colors())
  legend_dummy <- data.frame(
    year_factor = factor(rep(year_levels[[1]], length(legend_levels)), levels = year_levels),
    month_label = factor(rep(hydrodatabr_month_labels[[1]], length(legend_levels)), levels = hydrodatabr_month_labels),
    failure_class = factor(legend_levels, levels = legend_levels),
    stringsAsFactors = FALSE
  )
  if ("variable_label" %in% names(plot_data)) {
    legend_dummy$variable_label <- plot_data$variable_label[[1]]
  }
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["year_factor"]],
      y = .data[["month_label"]],
      fill = .data[["failure_class"]]
    )
  ) +
    ggplot2::geom_tile(
      data = legend_dummy,
      ggplot2::aes(
        x = .data[["year_factor"]],
        y = .data[["month_label"]],
        fill = .data[["failure_class"]]
      ),
      alpha = 0,
      show.legend = TRUE,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_tile(width = 0.95, height = 0.95, color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(
      name = "Falhas",
      values = hydrodatabr_failure_colors(),
      limits = legend_levels,
      breaks = legend_levels,
      labels = hydrodatabr_failure_labels(),
      drop = FALSE,
      na.translate = FALSE,
      guide = ggplot2::guide_legend(
        override.aes = list(alpha = 1, color = NA)
      )
    ) +
    ggplot2::scale_x_discrete(
      name = "Ano",
      breaks = as.character(year_breaks),
      drop = FALSE,
      expand = ggplot2::expansion(add = 0.1)
    ) +
    ggplot2::scale_y_discrete(
      name = "M\u00eas",
      drop = FALSE,
      expand = ggplot2::expansion(add = 0.05)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      legend.position = "right",
      plot.margin = ggplot2::margin(6, 8, 6, 8)
    )
  if (!is.null(facet_layer)) {
    p <- p + facet_layer
  }
  attr(p, "hydrodatabr_data") <- plot_data
  p
}
#' Grafico de resumo mensal
#'
#' Gera um grafico de regime mensal. A entrada pode ser uma serie diaria padronizada ou uma tabela de resumo mensal ja calculada.
#'
#' @param data Serie diaria padronizada ou tabela de resumo ja calculada.
#' @param variable Variavel a filtrar quando `data` e uma serie diaria.
#' @param station_code Codigo da estacao a filtrar.
#' @param statistic Estatistica usada quando a coluna de valor nao e informada.
#' @param value_col Coluna de valor opcional.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:400,
#'   variable = "discharge",
#'   value = 10 + sin(seq(0, 8, length.out = 401)),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' plot_monthly_summary(daily, variable = "discharge")
#' @noRd
plot_monthly_summary <- function(data, variable = NULL, station_code = NULL,
                                 statistic = c("mean", "total", "min", "max"),
                                 value_col = NULL, title = NULL,
                                 subtitle = NULL, base_size = 11) {
  statistic <- match.arg(statistic)
  input <- hydrodatabr_as_data_frame(data)
  if (hydrodatabr_is_daily_contract(input)) {
    plot_data <- table_daily_statistics(
      input,
      period = "monthly_regime",
      variable = variable,
      station_code = station_code
    )
  } else {
    plot_data <- input
  }
  if (nrow(plot_data) == 0) {
    stop("No monthly summary data available for plotting.", call. = FALSE)
  }
  if (!"month" %in% names(plot_data)) {
    stop("Monthly summary data must contain a `month` column.", call. = FALSE)
  }
  if (!"month_label" %in% names(plot_data)) {
    plot_data$month_label <- hydrodatabr_month_labels[as.integer(plot_data$month)]
  }
  y_col <- hydrodatabr_summary_value_column(plot_data, statistic, value_col)
  plot_data[[y_col]] <- suppressWarnings(as.numeric(plot_data[[y_col]]))
  plot_data$month <- as.integer(plot_data$month)
  plot_data$month_label <- factor(plot_data$month_label, levels = hydrodatabr_month_labels)
  plot_data <- plot_data[is.finite(plot_data[[y_col]]) & !is.na(plot_data$month), , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("No finite monthly summary values available for plotting.", call. = FALSE)
  }
  color_col <- if ("variable" %in% names(plot_data) && length(unique(plot_data$variable)) > 1) "variable" else NULL
  variable_values <- if ("variable" %in% names(plot_data)) {
    unique(as.character(plot_data$variable[!is.na(plot_data$variable) & nzchar(plot_data$variable)]))
  } else {
    character(0)
  }
  rainfall_only <- length(variable_values) == 1 && identical(variable_values[[1]], "rainfall")
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["month_label"]], y = .data[[y_col]]))
  if (rainfall_only && is.null(color_col)) {
    p <- p + ggplot2::geom_col(width = 0.72, fill = "grey35")
  } else if (rainfall_only) {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data[[color_col]]), width = 0.72)
  } else if (is.null(color_col)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.45, lineend = "round", linejoin = "round") +
      ggplot2::geom_point(size = 1.5)
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[color_col]], group = .data[[color_col]]), linewidth = 0.45) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[color_col]]), size = 1.5)
  }
  y_label <- hydrodatabr_summary_axis_label(plot_data, statistic, y_col, variable)
  p +
    ggplot2::scale_x_discrete(name = "M\u00eas") +
    ggplot2::scale_y_continuous(name = y_label, labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle, color = NULL, fill = NULL) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
}
#' Grafico de resumo anual
#'
#' Gera um grafico anual. A entrada pode ser uma serie diaria padronizada ou uma tabela de resumo anual ja calculada.
#'
#' @param data Serie diaria padronizada ou tabela de resumo ja calculada.
#' @param variable Variavel a filtrar quando `data` e uma serie diaria.
#' @param station_code Codigo da estacao a filtrar.
#' @param statistic Estatistica usada quando a coluna de valor nao e informada.
#' @param value_col Coluna de valor opcional.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param max_year_labels Numero maximo aproximado de rotulos no eixo dos anos,
#'   quando aplicavel.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' daily <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:400,
#'   variable = "discharge",
#'   value = 10 + sin(seq(0, 8, length.out = 401)),
#'   unit = "m3/s",
#'   consistency_level = NA_integer_,
#'   source_status = NA_character_,
#'   source = "example"
#' )
#' plot_annual_summary(daily, variable = "discharge")
#' @noRd
plot_annual_summary <- function(data, variable = NULL, station_code = NULL,
                                statistic = c("mean", "total", "min", "max"),
                                value_col = NULL, title = NULL,
                                subtitle = NULL, max_year_labels = 12,
                                base_size = 11) {
  statistic <- match.arg(statistic)
  input <- hydrodatabr_as_data_frame(data)
  if (hydrodatabr_is_daily_contract(input)) {
    plot_data <- table_daily_statistics(
      input,
      period = "annual",
      variable = variable,
      station_code = station_code
    )
  } else {
    plot_data <- input
  }
  if (nrow(plot_data) == 0) {
    stop("No annual summary data available for plotting.", call. = FALSE)
  }
  if (!"year" %in% names(plot_data)) {
    stop("Annual summary data must contain a `year` column.", call. = FALSE)
  }
  y_col <- hydrodatabr_summary_value_column(plot_data, statistic, value_col)
  plot_data[[y_col]] <- suppressWarnings(as.numeric(plot_data[[y_col]]))
  plot_data$year <- as.integer(plot_data$year)
  plot_data <- plot_data[is.finite(plot_data[[y_col]]) & !is.na(plot_data$year), , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("No finite annual summary values available for plotting.", call. = FALSE)
  }
  color_col <- if ("variable" %in% names(plot_data) && length(unique(plot_data$variable)) > 1) "variable" else NULL
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["year"]], y = .data[[y_col]]))
  if (is.null(color_col)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.45, lineend = "round", linejoin = "round") +
      ggplot2::geom_point(size = 1.5)
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = .data[[color_col]], group = .data[[color_col]]), linewidth = 0.45) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[color_col]]), size = 1.5)
  }
  year_breaks <- hydrodatabr_year_breaks(plot_data$year, max_breaks = max_year_labels)
  y_label <- hydrodatabr_summary_axis_label(plot_data, statistic, y_col, variable)
  p +
    ggplot2::scale_x_continuous(
      name = "Ano",
      breaks = year_breaks,
      minor_breaks = NULL,
      labels = function(x) format(as.integer(x), scientific = FALSE, trim = TRUE)
    ) +
    ggplot2::scale_y_continuous(name = y_label, labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle, color = NULL) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
}
hydrodatabr_fdc_probabilities <- function() {
  unique(round(c(
    seq(0.1, 1, by = 0.1),
    seq(1.25, 5, by = 0.25),
    seq(6, 94, by = 1),
    seq(95, 99, by = 0.25),
    seq(99.1, 99.9, by = 0.1)
  ), 2))
}
hydrodatabr_normal_probability_trans <- function() {
  scales::trans_new(
    name = "normal_probability_percent",
    transform = function(x) stats::qnorm(pmin(pmax(x, 0.1), 99.9) / 100),
    inverse = function(x) 100 * stats::pnorm(x),
    domain = c(0.1, 99.9)
  )
}
hydrodatabr_flow_duration_table <- function(values, target_probabilities = NULL) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(data.frame())
  }
  if (is.null(target_probabilities)) {
    target_probabilities <- hydrodatabr_fdc_probabilities()
  }
  sorted_values <- sort(values, decreasing = TRUE)
  n_values <- length(sorted_values)
  permanence <- seq_len(n_values) / n_values * 100
  if (n_values == 1) {
    discharge <- rep(sorted_values[[1]], length(target_probabilities))
  } else {
    discharge <- as.numeric(stats::approx(
      x = permanence,
      y = sorted_values,
      xout = target_probabilities,
      rule = 2
    )$y)
  }
  data.frame(
    permanence_pct = target_probabilities,
    discharge_m3s = discharge,
    stringsAsFactors = FALSE
  )
}
#' Grafico de curva de permanencia
#'
#' Gera a curva de permanencia para vazoes diarias padronizadas ou para um vetor
#' numerico de valores de vazao.
#'
#' @param data Tabela no contrato diario padronizado ou vetor numerico.
#' @param variable Variavel a filtrar. O padrao e `"discharge"`.
#' @param station_code Codigo da estacao a filtrar.
#' @param target_probabilities Probabilidades de permanencia em porcentagem. Se
#'   `NULL`, usa a grade padrao da funcao auxiliar.
#' @param log_y Se `TRUE`, usa escala logaritmica no eixo y.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' plot_flow_duration_curve(c(10, 8, 6, 4, 2), target_probabilities = c(10, 50, 90))
#' @noRd
plot_flow_duration_curve <- function(data, variable = "discharge", station_code = NULL,
                                     target_probabilities = NULL, log_y = FALSE,
                                     title = NULL, subtitle = NULL,
                                     base_size = 11) {
  if (is.data.frame(data)) {
    daily <- hydrodatabr_prepare_daily(data, variable = variable, station_code = station_code)
    values <- daily$value
  } else {
    values <- data
  }
  duration_data <- hydrodatabr_flow_duration_table(values, target_probabilities)
  if (nrow(duration_data) == 0) {
    stop("No finite values available for the flow-duration curve.", call. = FALSE)
  }
  if (isTRUE(log_y)) {
    duration_data <- duration_data[duration_data$discharge_m3s > 0, , drop = FALSE]
  }
  if (nrow(duration_data) == 0) {
    stop("No positive values available for a log-scale flow-duration curve.", call. = FALSE)
  }
  p <- ggplot2::ggplot(duration_data, ggplot2::aes(x = .data[["permanence_pct"]], y = .data[["discharge_m3s"]])) +
    ggplot2::geom_line(linewidth = 0.45, lineend = "round", linejoin = "round") +
    ggplot2::scale_x_continuous(
      name = "Perman\u00eancia (%)",
      trans = hydrodatabr_normal_probability_trans(),
      breaks = c(1, 2, 5, 10, 20, 50, 80, 90, 95, 99),
      labels = c("1", "2", "5", "10", "20", "50", "80", "90", "95", "99"),
      limits = c(0.5, 99)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(6, 8, 6, 8))
  if (isTRUE(log_y)) {
    p <- p + ggplot2::scale_y_log10(
      name = "Vaz\u00e3o (m\u00b3/s)",
      labels = hydrodatabr_number_labels()
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(
      name = "Vaz\u00e3o (m\u00b3/s)",
      labels = hydrodatabr_number_labels()
    )
  }
  attr(p, "hydrodatabr_data") <- duration_data
  p
}
hydrodatabr_prepare_measurements <- function(measurements) {
  data <- hydrodatabr_as_data_frame(measurements)
  if (nrow(data) == 0) {
    return(data.frame())
  }
  stage_col <- hydrodatabr_first_existing_name(data, c("stage_cm", "stage", "cota_cm", "cota", "water_level_cm", "h_cm"))
  discharge_col <- hydrodatabr_first_existing_name(data, c("discharge_m3s", "discharge", "vazao", "flow_m3s", "q_m3s"))
  date_col <- hydrodatabr_first_existing_name(data, c("measurement_date", "measurement_datetime", "date", "datetime"))
  area_col <- hydrodatabr_first_existing_name(data, c("wetted_area_m2", "area_molhada_m2", "area_molhada", "area"))
  width_col <- hydrodatabr_first_existing_name(data, c("width_m", "largura_m", "largura"))
  depth_col <- hydrodatabr_first_existing_name(data, c("mean_depth_m", "profundidade_media_m", "profundidade_media"))
  velocity_col <- hydrodatabr_first_existing_name(data, c("mean_velocity_ms", "mean_velocity_m_s", "mean_velocity", "velocity_ms", "velocity_m_s", "velocidade_media_ms", "velocidade_media"))
  n <- nrow(data)
  out <- data.frame(.row_id = seq_len(n), stringsAsFactors = FALSE)
  out$stage_cm <- if (!is.na(stage_col)) suppressWarnings(as.numeric(data[[stage_col]])) else rep(NA_real_, n)
  out$discharge_m3s <- if (!is.na(discharge_col)) suppressWarnings(as.numeric(data[[discharge_col]])) else rep(NA_real_, n)
  out$wetted_area_m2 <- if (!is.na(area_col)) suppressWarnings(as.numeric(data[[area_col]])) else rep(NA_real_, n)
  out$width_m <- if (!is.na(width_col)) suppressWarnings(as.numeric(data[[width_col]])) else rep(NA_real_, n)
  out$mean_depth_m <- if (!is.na(depth_col)) suppressWarnings(as.numeric(data[[depth_col]])) else rep(NA_real_, n)
  out$mean_velocity_ms <- if (!is.na(velocity_col)) suppressWarnings(as.numeric(data[[velocity_col]])) else rep(NA_real_, n)
  out$measurement_date <- if (!is.na(date_col)) as.Date(data[[date_col]]) else rep(as.Date(NA), n)
  out$measurement_year <- as.integer(format(out$measurement_date, "%Y"))
  out$approx_wetted_perimeter_m <- ifelse(
    is.finite(out$width_m) & is.finite(out$mean_depth_m),
    out$width_m + 2 * out$mean_depth_m,
    NA_real_
  )
  out$approx_hydraulic_radius_m <- ifelse(
    is.finite(out$wetted_area_m2) & is.finite(out$approx_wetted_perimeter_m) & out$approx_wetted_perimeter_m > 0,
    out$wetted_area_m2 / out$approx_wetted_perimeter_m,
    NA_real_
  )
  out$area_rh_two_thirds <- ifelse(
    is.finite(out$wetted_area_m2) & is.finite(out$approx_hydraulic_radius_m) & out$approx_hydraulic_radius_m > 0,
    out$wetted_area_m2 * (out$approx_hydraulic_radius_m ^ (2 / 3)),
    NA_real_
  )
  out$.row_id <- NULL
  out
}
#' Grafico de medicoes de descarga
#'
#' Gera graficos simples para medicoes de descarga, seguindo a estrutura visual
#' usada no HydroStat.
#'
#' @param measurements Tabela de medicoes de descarga.
#' @param type Tipo de grafico: `"rating"`, `"year"`, `"area_rh"` ou
#'   `"velocity"`.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param max_year_labels Numero maximo aproximado de rotulos no eixo dos anos,
#'   usado em `type = "year"`.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' measurements <- data.frame(
#'   station_code = "001",
#'   measurement_date = as.Date(c("2020-01-01", "2020-02-01")),
#'   stage_cm = c(100, 120),
#'   discharge_m3s = c(10, 15)
#' )
#' plot_discharge_measurements(measurements, type = "rating")
#' @noRd
plot_discharge_measurements <- function(measurements,
                                        type = c("rating", "year", "area_rh", "velocity"),
                                        title = NULL, subtitle = NULL,
                                        max_year_labels = 12,
                                        base_size = 11) {
  type <- match.arg(type)
  plot_data <- hydrodatabr_prepare_measurements(measurements)
  if (nrow(plot_data) == 0) {
    stop("No discharge measurements available for plotting.", call. = FALSE)
  }
  if (identical(type, "year")) {
    plot_data <- plot_data[!is.na(plot_data$measurement_year), , drop = FALSE]
    if (nrow(plot_data) == 0) stop("No measurement dates available for plotting.", call. = FALSE)
    year_values <- seq(min(plot_data$measurement_year), max(plot_data$measurement_year), by = 1L)
    year_counts <- tabulate(match(plot_data$measurement_year, year_values), nbins = length(year_values))
    by_year <- data.frame(
      year = factor(as.character(year_values), levels = as.character(year_values)),
      n_measurements = as.integer(year_counts),
      stringsAsFactors = FALSE
    )
    year_breaks <- hydrodatabr_year_breaks(year_values, max_breaks = max_year_labels)
    return(
      ggplot2::ggplot(by_year, ggplot2::aes(x = .data[["year"]], y = .data[["n_measurements"]])) +
        ggplot2::geom_col(width = 0.65, fill = "grey85", color = "grey35", linewidth = 0.25) +
        ggplot2::scale_x_discrete(
          name = "Ano",
          breaks = as.character(year_breaks),
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(
          name = "N\u00famero de medi\u00e7\u00f5es",
          breaks = scales::breaks_width(1),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(title = title, subtitle = subtitle) +
        theme_hydrodatabr(base_size = base_size)
    )
  }
  if (identical(type, "rating")) {
    plot_data <- plot_data[is.finite(plot_data$stage_cm) & is.finite(plot_data$discharge_m3s), , drop = FALSE]
    x_col <- "discharge_m3s"
    y_col <- "stage_cm"
    x_lab <- "Vaz\u00e3o (m\u00b3/s)"
    y_lab <- "Cota (cm)"
  } else if (identical(type, "area_rh")) {
    plot_data <- plot_data[is.finite(plot_data$area_rh_two_thirds) & is.finite(plot_data$stage_cm), , drop = FALSE]
    x_col <- "area_rh_two_thirds"
    y_col <- "stage_cm"
    x_lab <- expression(A~R[h]^{2/3}~(m^{8/3}))
    y_lab <- "Cota (cm)"
  } else {
    plot_data <- plot_data[is.finite(plot_data$mean_velocity_ms) & is.finite(plot_data$stage_cm), , drop = FALSE]
    x_col <- "mean_velocity_ms"
    y_col <- "stage_cm"
    x_lab <- "Velocidade m\u00e9dia (m/s)"
    y_lab <- "Cota (cm)"
  }
  if (nrow(plot_data) == 0) {
    stop("No finite measurement values available for plotting.", call. = FALSE)
  }
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_point(alpha = 0.6, size = 1.6, color = "gray15") +
    ggplot2::scale_x_continuous(name = x_lab, labels = hydrodatabr_number_labels()) +
    ggplot2::scale_y_continuous(name = y_lab, labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
}
hydrodatabr_rating_curve_points <- function(rating_curves, n_points = 80) {
  curves <- hydrodatabr_as_data_frame(rating_curves)
  if (nrow(curves) == 0) {
    return(data.frame())
  }
  a_col <- hydrodatabr_first_existing_name(curves, c("coefficient_a", "a"))
  h0_col <- hydrodatabr_first_existing_name(curves, c("coefficient_h0_cm", "h0_cm", "coefficient_h0"))
  b_col <- hydrodatabr_first_existing_name(curves, c("coefficient_b", "coefficient_n", "b", "n"))
  stage_min_col <- hydrodatabr_first_existing_name(curves, c("stage_min_cm", "h_min_cm", "stage_min"))
  stage_max_col <- hydrodatabr_first_existing_name(curves, c("stage_max_cm", "h_max_cm", "stage_max"))
  required <- c(a_col, h0_col, b_col, stage_min_col, stage_max_col)
  if (any(is.na(required))) {
    stop(
      "Rating curves must contain coefficient and stage-range columns.",
      call. = FALSE
    )
  }
  curve_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_id", "curve_id"))
  segment_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_segment_id", "segment_id"))
  pieces <- vector("list", nrow(curves))
  for (i in seq_len(nrow(curves))) {
    a <- suppressWarnings(as.numeric(curves[[a_col]][[i]]))
    h0 <- suppressWarnings(as.numeric(curves[[h0_col]][[i]]))
    b <- suppressWarnings(as.numeric(curves[[b_col]][[i]]))
    stage_min <- suppressWarnings(as.numeric(curves[[stage_min_col]][[i]]))
    stage_max <- suppressWarnings(as.numeric(curves[[stage_max_col]][[i]]))
    if (!all(is.finite(c(a, h0, b, stage_min, stage_max))) || stage_max <= stage_min) {
      next
    }
    stage <- seq(stage_min, stage_max, length.out = n_points)
    discharge <- ifelse(stage > h0, a * ((stage - h0) ^ b), NA_real_)
    curve_id <- if (!is.na(curve_id_col)) as.character(curves[[curve_id_col]][[i]]) else as.character(i)
    segment_id <- if (!is.na(segment_id_col)) as.character(curves[[segment_id_col]][[i]]) else as.character(i)
    curve_label <- paste0("CC ", curve_id)
    segment_label <- paste0(curve_label, " - seg. ", segment_id)
    pieces[[i]] <- data.frame(
      stage_cm = stage,
      discharge_m3s = discharge,
      curve_label = curve_label,
      curve_segment_label = segment_label,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, pieces)
  if (is.null(out)) {
    return(data.frame())
  }
  out <- out[is.finite(out$stage_cm) & is.finite(out$discharge_m3s), , drop = FALSE]
  rownames(out) <- NULL
  out
}
hydrodatabr_rating_curve_confidence_factor <- function(rating_curves, measurements = NULL,
                                                       confidence_level = 0.95) {
  if (is.null(measurements)) {
    return(NA_real_)
  }
  measurements <- hydrodatabr_as_data_frame(measurements)
  if (nrow(measurements) == 0) {
    return(NA_real_)
  }
  confidence_level <- suppressWarnings(as.numeric(confidence_level[[1]]))
  if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    confidence_level <- 0.95
  }
  diag <- NULL
  if (exists("hydrodatabr_rating_diagnostics_analysis", mode = "function")) {
    diag <- try(
      hydrodatabr_rating_diagnostics_analysis(
        rating_curves = rating_curves,
        measurements = measurements,
        envelope_quantile = confidence_level
      ),
      silent = TRUE
    )
  }
  if (!inherits(diag, "try-error") && is.data.frame(diag) && "log_residual" %in% names(diag)) {
    abs_res <- abs(suppressWarnings(as.numeric(diag$log_residual)))
    abs_res <- abs_res[is.finite(abs_res)]
    if (length(abs_res) >= 3) {
      return(as.numeric(stats::quantile(abs_res, probs = confidence_level, na.rm = TRUE, type = 8)))
    }
  }
  # Conservative fallback when detailed matching is not available.
  # It fits only a simple empirical spread in log space between measured
  # discharge and the nearest curve value at the measured stage.
  if (!exists("hydrodatabr_rating_curve_value", mode = "function") ||
      !exists("hydrodatabr_rating_match_curve", mode = "function")) {
    return(NA_real_)
  }
  curves <- hydrodatabr_as_data_frame(rating_curves)
  meas <- try(hydrodatabr_prepare_measurements(measurements), silent = TRUE)
  if (inherits(meas, "try-error") || !is.data.frame(meas) || nrow(curves) == 0 || nrow(meas) == 0) {
    return(NA_real_)
  }
  if ("station_code" %in% names(measurements)) meas$station_code <- as.character(measurements$station_code) else meas$station_code <- NA_character_
  if ("station_code" %in% names(curves)) curves$station_code <- as.character(curves$station_code) else curves$station_code <- NA_character_
  log_res <- numeric(0)
  for (i in seq_len(nrow(meas))) {
    st <- meas$station_code[[i]]
    st_curves <- if (!is.na(st) && "station_code" %in% names(curves)) curves[curves$station_code %in% st, , drop = FALSE] else curves
    idx <- hydrodatabr_rating_match_curve(st_curves, meas$measurement_date[[i]], meas$stage_cm[[i]])
    predicted <- if (length(idx) == 1) hydrodatabr_rating_curve_value(st_curves[idx, , drop = FALSE], meas$stage_cm[[i]]) else NA_real_
    observed <- meas$discharge_m3s[[i]]
    if (is.finite(observed) && observed > 0 && is.finite(predicted) && predicted > 0) {
      log_res <- c(log_res, log(observed) - log(predicted))
    }
  }
  abs_res <- abs(log_res[is.finite(log_res)])
  if (length(abs_res) < 3) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(abs_res, probs = confidence_level, na.rm = TRUE, type = 8))
}
hydrodatabr_rating_curve_confidence_band <- function(rating_curves, measurements = NULL,
                                                     n_points = 80,
                                                     confidence_level = 0.95) {
  curve_points <- hydrodatabr_rating_curve_points(rating_curves, n_points = n_points)
  if (nrow(curve_points) == 0) {
    return(data.frame())
  }
  limit <- hydrodatabr_rating_curve_confidence_factor(
    rating_curves = rating_curves,
    measurements = measurements,
    confidence_level = confidence_level
  )
  if (!is.finite(limit) || limit <= 0) {
    return(data.frame())
  }
  curve_points$q_lower_m3s <- pmax(0, curve_points$discharge_m3s * exp(-limit))
  curve_points$q_upper_m3s <- curve_points$discharge_m3s * exp(limit)
  curve_points$interval_label <- paste0(round(100 * confidence_level), "%")
  groups <- split(curve_points, curve_points$curve_segment_label, drop = TRUE)
  pieces <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    g <- g[order(g$stage_cm), , drop = FALSE]
    if (nrow(g) < 2) {
      next
    }
    pieces[[i]] <- data.frame(
      stage_cm = c(g$stage_cm, rev(g$stage_cm)),
      discharge_m3s = c(g$q_lower_m3s, rev(g$q_upper_m3s)),
      curve_label = g$curve_label[[1]],
      curve_segment_label = g$curve_segment_label[[1]],
      interval_label = g$interval_label[[1]],
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, pieces)
  if (is.null(out)) {
    return(data.frame())
  }
  row.names(out) <- NULL
  out
}
#' Grafico de curvas-chave
#'
#' Gera um grafico de curvas-chave e, opcionalmente, sobrepoe medicoes de
#' descarga. As curvas devem conter coeficientes e faixas de cota.
#'
#' @details
#' Por padrao, todas as curvas fornecidas na entrada sao plotadas. Para mostrar
#' apenas uma curva, filtre `rating_curves` antes de chamar a funcao. As faixas
#' empiricas sao calculadas quando ha medicoes suficientes; caso contrario, sao
#' omitidas sem alterar a curva principal. Este grafico nao depende de selecao
#' de secoes transversais.
#'
#' @param rating_curves Tabela de curvas-chave.
#' @param measurements Tabela opcional de medicoes de descarga.
#' @param n_points Numero de pontos por segmento de curva.
#' @param show_confidence Se `TRUE`, adiciona uma faixa empirica de confianca
#'   calculada a partir dos residuos logaritmicos das medicoes disponiveis.
#' @param confidence_level Nivel usado para a faixa empirica, entre 0 e 1.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
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
#'   coefficient_h0_m = 0,
#'   coefficient_n = 1
#' )
#' plot_rating_curves(curves, show_confidence = FALSE)
#' @noRd
plot_rating_curves <- function(rating_curves, measurements = NULL, n_points = 80,
                               show_confidence = TRUE, confidence_level = 0.95,
                               title = NULL, subtitle = NULL, base_size = 11) {
  curve_points <- hydrodatabr_rating_curve_points(rating_curves, n_points = n_points)
  confidence_band <- if (isTRUE(show_confidence)) {
    hydrodatabr_rating_curve_confidence_band(
      rating_curves = rating_curves,
      measurements = measurements,
      n_points = n_points,
      confidence_level = confidence_level
    )
  } else {
    data.frame()
  }
  measurement_points <- if (!is.null(measurements)) hydrodatabr_prepare_measurements(measurements) else data.frame()
  if (nrow(measurement_points) > 0) {
    measurement_points <- measurement_points[is.finite(measurement_points$stage_cm) & is.finite(measurement_points$discharge_m3s), , drop = FALSE]
  }
  if (nrow(curve_points) == 0 && nrow(measurement_points) == 0) {
    stop("No rating curve or measurement data available for plotting.", call. = FALSE)
  }
  p <- ggplot2::ggplot()
  if (nrow(confidence_band) > 0) {
    p <- p + ggplot2::geom_polygon(
      data = confidence_band,
      ggplot2::aes(
        x = .data[["discharge_m3s"]], y = .data[["stage_cm"]],
        group = .data[["curve_segment_label"]]
      ),
      inherit.aes = FALSE,
      fill = "#9ecae1", color = "#6baed6", linewidth = 0.15, alpha = 0.38
    )
  }
  if (nrow(measurement_points) > 0) {
    p <- p + ggplot2::geom_point(
      data = measurement_points,
      ggplot2::aes(x = .data[["discharge_m3s"]], y = .data[["stage_cm"]]),
      color = "grey45", alpha = 0.6, size = 1.6
    )
  }
  if (nrow(curve_points) > 0) {
    curve_palette <- hydrodatabr_plot_palette(curve_points$curve_label)
    p <- p + ggplot2::geom_line(
      data = curve_points,
      ggplot2::aes(
        x = .data[["discharge_m3s"]], y = .data[["stage_cm"]],
        group = .data[["curve_segment_label"]], color = .data[["curve_label"]]
      ),
      alpha = 0.85, linewidth = 0.6
    ) +
      ggplot2::scale_color_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE)
  }
  confidence_caption <- if (nrow(confidence_band) > 0) {
    paste0("Faixa azul: intervalo empirico de ", round(100 * confidence_level), "% baseado nos residuos logaritmicos das medicoes.")
  } else {
    NULL
  }
  p +
    ggplot2::scale_x_continuous(name = "Vaz\u00e3o (m\u00b3/s)", labels = hydrodatabr_number_labels()) +
    ggplot2::scale_y_continuous(name = "Cota (cm)", labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle, color = NULL, caption = confidence_caption) +
    theme_hydrodatabr(base_size = base_size)
}
hydrodatabr_prepare_rating_curve_validity <- function(rating_curves, open_end_date = NULL) {
  curves <- hydrodatabr_as_data_frame(rating_curves)
  if (nrow(curves) == 0) {
    return(data.frame())
  }
  curve_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_id", "curve_id", "id_curve", "id_curva"))
  segment_id_col <- hydrodatabr_first_existing_name(curves, c("rating_curve_segment_id", "segment_id", "id_segment", "id_segmento"))
  curve_label_col <- hydrodatabr_first_existing_name(curves, c("curve_label", "curva_label", "rating_curve_label", "label"))
  valid_from_col <- hydrodatabr_first_existing_name(curves, c(
    "valid_from", "valid_from_date", "validity_start_date", "start_date",
    "date_start", "inicio_validade", "data_inicio_validade"
  ))
  valid_to_col <- hydrodatabr_first_existing_name(curves, c(
    "valid_to", "valid_to_date", "validity_end_date", "end_date",
    "date_end", "fim_validade", "data_fim_validade"
  ))
  stage_min_col <- hydrodatabr_first_existing_name(curves, c("stage_min_cm", "h_min_cm", "stage_min", "cota_min_cm"))
  stage_max_col <- hydrodatabr_first_existing_name(curves, c("stage_max_cm", "h_max_cm", "stage_max", "cota_max_cm"))
  if (any(is.na(c(valid_from_col, stage_min_col, stage_max_col)))) {
    stop(
      "Rating curves must contain validity-start and stage-range columns.",
      call. = FALSE
    )
  }
  valid_from <- hydrodatabr_as_date_safe(curves[[valid_from_col]])
  valid_to <- if (!is.na(valid_to_col)) hydrodatabr_as_date_safe(curves[[valid_to_col]]) else rep(as.Date(NA), nrow(curves))
  stage_min <- suppressWarnings(as.numeric(curves[[stage_min_col]]))
  stage_max <- suppressWarnings(as.numeric(curves[[stage_max_col]]))
  if (is.null(open_end_date)) {
    finite_dates <- c(valid_from[!is.na(valid_from)], valid_to[!is.na(valid_to)])
    open_end_date <- if (length(finite_dates) > 0) max(finite_dates, na.rm = TRUE) else Sys.Date()
  }
  open_end_date <- hydrodatabr_as_date_safe(open_end_date)[[1]]
  valid_to_plot <- valid_to
  valid_to_plot[is.na(valid_to_plot)] <- open_end_date
  curve_id <- if (!is.na(curve_id_col)) as.character(curves[[curve_id_col]]) else as.character(seq_len(nrow(curves)))
  segment_id <- if (!is.na(segment_id_col)) as.character(curves[[segment_id_col]]) else as.character(seq_len(nrow(curves)))
  curve_label <- if (!is.na(curve_label_col)) {
    as.character(curves[[curve_label_col]])
  } else {
    paste0("CC ", curve_id)
  }
  curve_label[is.na(curve_label) | !nzchar(curve_label)] <- paste0("CC ", curve_id[is.na(curve_label) | !nzchar(curve_label)])
  out <- data.frame(
    curve_id = curve_id,
    segment_id = segment_id,
    curve_label = curve_label,
    curve_segment_label = paste0(curve_label, " - seg. ", segment_id),
    valid_from = valid_from,
    valid_to = valid_to,
    valid_to_plot = valid_to_plot,
    stage_min_cm = stage_min,
    stage_max_cm = stage_max,
    stringsAsFactors = FALSE
  )
  out <- out[
    !is.na(out$valid_from) &
      !is.na(out$valid_to_plot) &
      is.finite(out$stage_min_cm) &
      is.finite(out$stage_max_cm) &
      out$stage_max_cm > out$stage_min_cm &
      out$valid_to_plot >= out$valid_from,
    , drop = FALSE
  ]
  out <- out[order(out$valid_from, out$valid_to_plot, out$stage_min_cm, out$stage_max_cm), , drop = FALSE]
  rownames(out) <- NULL
  out
}
#' Grafico de validade das curvas-chave
#'
#' Gera um grafico temporal das janelas de validade das curvas-chave. A largura
#' de cada retangulo representa o periodo de validade e a altura representa a
#' faixa valida de cota.
#'
#' @param rating_curves Tabela de curvas-chave ou resumo de curvas-chave.
#' @param open_end_date Data usada para representar curvas sem data final de
#'   validade. Use `NULL` para usar a maior data encontrada na tabela.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param max_year_labels Numero maximo aproximado de rotulos no eixo dos anos.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
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
#'   coefficient_h0_m = 0,
#'   coefficient_n = 1
#' )
#' plot_rating_curve_validity(curves)
#' @noRd
plot_rating_curve_validity <- function(rating_curves, open_end_date = NULL,
                                       title = NULL, subtitle = NULL,
                                       max_year_labels = 12,
                                       base_size = 11) {
  timeline <- hydrodatabr_prepare_rating_curve_validity(
    rating_curves = rating_curves,
    open_end_date = open_end_date
  )
  if (nrow(timeline) == 0) {
    stop("No rating-curve validity windows available for plotting.", call. = FALSE)
  }
  curve_palette <- hydrodatabr_plot_palette(timeline$curve_label)
  year_sequence <- seq(
    as.integer(format(min(timeline$valid_from), "%Y")),
    as.integer(format(max(timeline$valid_to_plot), "%Y")),
    by = 1L
  )
  year_breaks <- hydrodatabr_year_breaks(year_sequence, max_breaks = max_year_labels)
  date_breaks <- as.Date(paste0(year_breaks, "-01-01"))
  p <- ggplot2::ggplot(timeline) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data[["valid_from"]],
        xmax = .data[["valid_to_plot"]],
        ymin = .data[["stage_min_cm"]],
        ymax = .data[["stage_max_cm"]],
        fill = .data[["curve_label"]],
        color = .data[["curve_label"]]
      ),
      alpha = 0.20,
      linewidth = 0.45
    ) +
    ggplot2::scale_x_date(
      name = "Data",
      breaks = date_breaks,
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      name = "Faixa v\u00e1lida de cota (cm)",
      labels = hydrodatabr_number_labels()
    ) +
    ggplot2::scale_color_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE) +
    ggplot2::scale_fill_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(title = title, subtitle = subtitle, fill = NULL) +
    theme_hydrodatabr(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
  attr(p, "hydrodatabr_data") <- timeline
  p
}
#' Grafico de diagnosticos diarios de hidrometria
#'
#' Gera graficos simples para saidas de diagnostico diario de hidrometria.
#'
#' @details
#' A entrada pode ser a lista retornada por [diagnose_daily_hydrometry()] ou a
#' tabela `daily_flags`. O tipo `"relative_error"` usa `relative_error_pct`
#' quando ha valores finitos. O tipo `"flags"` resume colunas logicas de
#' diagnostico, incluindo colunas nomeadas como `diagnostic_problem`,
#' `missing_discharge`, `missing_stage`, `no_rating_curve_for_date` e
#' `stage_outside_curve_range`. O tipo `"flag_timeline"` mostra a ocorrencia
#' temporal dessas flags.
#'
#' @param diagnostics Tabela ou lista com resultados de diagnostico.
#' @param type Tipo de grafico: `"relative_error"`, `"flags"` ou
#'   `"flag_timeline"`.
#' @param title Titulo opcional.
#' @param subtitle Subtitulo opcional.
#' @param relative_error_limits Limites de referencia para erro relativo, em
#'   porcentagem.
#' @param base_size Tamanho base da fonte.
#'
#' @return Um objeto `ggplot`.
#'
#' @examples
#' diagnostics <- data.frame(
#'   station_code = "001",
#'   date = as.Date("2020-01-01") + 0:2,
#'   missing_stage = c(TRUE, FALSE, FALSE),
#'   no_rating_curve_for_date = c(FALSE, TRUE, FALSE)
#' )
#' plot_daily_hydrometry_diagnostics(diagnostics, type = "flags")
#' @noRd
plot_daily_hydrometry_diagnostics <- function(diagnostics,
                                              type = c("relative_error", "flags", "flag_timeline"),
                                              title = NULL, subtitle = NULL,
                                              relative_error_limits = c(-10, 10),
                                              base_size = 11) {
  type <- match.arg(type)
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
    stop("No diagnostic data available for plotting.", call. = FALSE)
  }
  if (identical(type, "relative_error")) {
    if (!all(c("date", "relative_error_pct") %in% names(data))) {
      stop("Relative-error diagnostics require `date` and `relative_error_pct` columns.", call. = FALSE)
    }
    data$date <- as.Date(data$date)
    data$relative_error_pct <- suppressWarnings(as.numeric(data$relative_error_pct))
    data <- data[!is.na(data$date) & is.finite(data$relative_error_pct), , drop = FALSE]
    if (nrow(data) == 0) stop("No finite relative-error values available.", call. = FALSE)
    return(
      ggplot2::ggplot(data, ggplot2::aes(x = .data[["date"]], y = .data[["relative_error_pct"]])) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.7) +
        ggplot2::geom_hline(yintercept = relative_error_limits, linetype = "dashed", linewidth = 0.3, alpha = 0.7) +
        ggplot2::geom_line(linewidth = 0.45, lineend = "round", linejoin = "round") +
        ggplot2::scale_x_date(name = "Data") +
        ggplot2::scale_y_continuous(name = "Erro relativo (%)", labels = hydrodatabr_number_labels()) +
        ggplot2::labs(title = title, subtitle = subtitle) +
        theme_hydrodatabr(base_size = base_size) +
        ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
    )
  }
  table_data <- table_daily_hydrometry_diagnostics(data)
  if (nrow(table_data) == 0 || !all(c("issue", "n_flagged") %in% names(table_data))) {
    stop("No diagnostic flags available for plotting.", call. = FALSE)
  }
  table_data <- table_data[table_data$n_flagged > 0, , drop = FALSE]
  if (nrow(table_data) == 0) {
    stop("No flagged records available for plotting.", call. = FALSE)
  }
  table_data$issue_label <- hydrodatabr_diagnostic_issue_labels(table_data$issue)
  if (identical(type, "flag_timeline")) {
    flag_cols <- hydrodatabr_hydrometry_flag_columns(data)
    if (length(flag_cols) == 0 || !"date" %in% names(data)) {
      stop("Flag-timeline diagnostics require `date` and logical `_flag` columns.", call. = FALSE)
    }
    data$date <- as.Date(data$date)
    pieces <- vector("list", length(flag_cols))
    for (i in seq_along(flag_cols)) {
      flag <- flag_cols[[i]]
      pieces[[i]] <- data.frame(
        date = data$date,
        issue = flag,
        issue_label = hydrodatabr_diagnostic_issue_label(flag),
        flagged = hydrodatabr_as_logical_flag(data[[flag]]),
        stringsAsFactors = FALSE
      )
    }
    timeline <- do.call(rbind, pieces)
    timeline <- timeline[!is.na(timeline$date) & isTRUE(nrow(timeline) > 0), , drop = FALSE]
    timeline <- timeline[!is.na(timeline$flagged), , drop = FALSE]
    if (nrow(timeline) == 0) {
      stop("No diagnostic flags available for plotting.", call. = FALSE)
    }
    timeline$flagged <- factor(ifelse(timeline$flagged, "Sinalizado", "N\u00e3o sinalizado"), levels = c("Sinalizado", "N\u00e3o sinalizado"))
    return(
      ggplot2::ggplot(timeline, ggplot2::aes(x = .data[["date"]], y = .data[["issue_label"]], fill = .data[["flagged"]])) +
        ggplot2::geom_tile(height = 0.85, linewidth = 0) +
        ggplot2::scale_x_date(name = "Data") +
        ggplot2::scale_y_discrete(name = NULL) +
        ggplot2::scale_fill_manual(values = c("Sinalizado" = "#d53e4f", "N\u00e3o sinalizado" = "#f2f2f2"), name = NULL, drop = FALSE) +
        ggplot2::labs(title = title, subtitle = subtitle) +
        theme_hydrodatabr(base_size = base_size) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")
    )
  }
  ggplot2::ggplot(table_data, ggplot2::aes(x = stats::reorder(.data[["issue_label"]], .data[["n_flagged"]]), y = .data[["n_flagged"]])) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_y_continuous(name = "Registros sinalizados", labels = hydrodatabr_number_labels()) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    theme_hydrodatabr(base_size = base_size)
}
