#' Exportar dados, tabelas e gráficos hidrológicos
#'
#' Exporta produtos gerados pelo hydroDataBR para arquivos locais. A função pode
#' gravar séries padronizadas, tabelas finais, relatórios de requisição,
#' resultados de análise e gráficos `ggplot`, dependendo do objeto informado e
#' do formato escolhido.
#'
#' @param x Objeto a exportar. Pode ser série diária padronizada, objeto de
#'   aquisição, lote, tabela, lista de tabelas ou gráfico.
#' @param path Caminho de saída. Para múltiplos componentes, informe um
#'   diretório. Para um único arquivo, informe o caminho completo com extensão.
#' @param format Formato de exportação: `"csv"`, `"rds"`, `"png"` ou
#'   `"pdf"`. Se omitido, a extensão de `path` é usada quando reconhecida;
#'   caso contrário, o padrão é `"csv"`.
#' @param components Componentes a exportar. Use `"all"` para gravar todos os
#'   componentes disponíveis, ou informe nomes/aliases como `Tab01`, `Tab02` e
#'   outros componentes definidos pelo pacote.
#' @param overwrite Se `TRUE`, sobrescreve arquivos existentes.
#' @param manifest Se `TRUE` e `path` for um diretório, grava também um arquivo
#'   `hydro_export_manifest.csv` com os arquivos produzidos.
#' @param hydrological_year_start Mês inicial do ano hidrológico usado em
#'   tabelas de máximos anuais. O padrão é 10, representando outubro.
#' @param low_flow_durations Durações, em dias, usadas em tabelas de mínimas
#'   móveis anuais.
#' @param ... Argumentos adicionais. Para gráficos, são repassados a
#'   `ggplot2::ggsave()` quando aplicável.
#'
#' @details
#' A função procura componentes exportáveis dentro do objeto informado. Em séries
#' diárias, aplica a regra usual do pacote: dado consistido tem prioridade; na
#' ausência dele, é usado o dado não consistido disponível. Para objetos de
#' aquisição agregada, a função pode exportar séries, tabelas derivadas e
#' relatórios. Para gráficos, use formatos como `png` ou `pdf`.
#'
#' @return Invisivelmente, um `data.frame` com os arquivos gravados.
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
#' # Exemplo de exportacao local.
#' if (FALSE) {
#'   write_hydro_data(
#'     daily,
#'     path = "saida_hydrodatabr",
#'     components = "all",
#'     overwrite = TRUE,
#'     manifest = TRUE
#'   )
#' }
write_hydro_data <- function(x,
                             path,
                             format = c("csv", "rds", "png", "pdf"),
                             components = "all",
                             overwrite = FALSE,
                             manifest = FALSE,
                             hydrological_year_start = 10L,
                             low_flow_durations = c(3L, 7L, 15L, 30L),
                             ...) {
  format <- hydro_export_resolve_format(path, format, missing(format))
  hydro_export_validate(path, overwrite, manifest, components)
  collected <- if (format %in% c("png", "pdf")) {
    hydro_export_collect_plots(x)
  } else {
    hydro_export_collect_tables(x, hydrological_year_start, low_flow_durations)
  }
  if (!length(collected)) stop("No exportable components were found in `x`.", call. = FALSE)
  collected <- hydro_export_filter_components(collected, components)
  if (!length(collected)) stop("No components matched `components`.", call. = FALSE)
  out <- hydro_export_write_components(collected, path, format, overwrite, ...)
  hydro_export_write_manifest(out, path, overwrite, manifest)
  invisible(out)
}
hydro_export_resolve_format <- function(path, format, missing_format) {
  choices <- c("csv", "rds", "png", "pdf")
  if (missing_format) {
    ext <- tolower(sub("^.*[.]", "", basename(path)))
    if (!grepl("[.]", basename(path), fixed = TRUE)) return("csv")
    if (ext %in% choices) return(ext)
    stop("Cannot infer export format from file extension.", call. = FALSE)
  }
  match.arg(format, choices)
}
hydro_export_validate <- function(path, overwrite, manifest, components) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) stop("`path` must be a non-empty character scalar.", call. = FALSE)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(manifest) || length(manifest) != 1L || is.na(manifest)) stop("`manifest` must be TRUE or FALSE.", call. = FALSE)
  if (!is.character(components) || length(components) < 1L || anyNA(components)) stop("`components` must be a character vector.", call. = FALSE)
  invisible(TRUE)
}
# Internal parsers and standardizers used by export tables.
hydrostat_empty_chr <- c("", "NA", "NaN", "NULL", "null", "None", "none")
hydrostat_parse_number <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- trimws(as.character(x))
  x[x %in% hydrostat_empty_chr] <- NA_character_
  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot <- grepl(".", x, fixed = TRUE)
  both <- has_comma & has_dot
  x[both] <- gsub(".", "", x[both], fixed = TRUE)
  x[both] <- gsub(",", ".", x[both], fixed = TRUE)
  comma_only <- has_comma & !has_dot
  x[comma_only] <- gsub(",", ".", x[comma_only], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
hydrostat_parse_integer <- function(x) {
  suppressWarnings(as.integer(hydrostat_parse_number(x)))
}
hydrostat_parse_date <- function(x) {
  if (is.null(x)) {
    return(as.Date(character()))
  }
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  x <- trimws(as.character(x))
  x[x %in% hydrostat_empty_chr] <- NA_character_
  x <- sub("T", " ", x, fixed = TRUE)
  x <- sub("[.][0-9]+Z?$", "", x)
  x <- sub("Z$", "", x)
  out <- rep(as.Date(NA), length(x))
  formats <- c(
    "%d/%m/%Y",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%d-%m-%Y",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%Y/%m/%d %H:%M:%S"
  )
  for (fmt in formats) {
    need <- is.na(out) & !is.na(x)
    if (!any(need)) {
      break
    }
    parsed <- suppressWarnings(as.Date(x[need], format = fmt))
    out[need] <- parsed
  }
  out
}
hydrostat_parse_datetime <- function(x) {
  if (is.null(x)) {
    return(as.POSIXct(character()))
  }
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  x <- trimws(as.character(x))
  x[x %in% hydrostat_empty_chr] <- NA_character_
  x <- sub("T", " ", x, fixed = TRUE)
  x <- sub("[.][0-9]+Z?$", "", x)
  x <- sub("Z$", "", x)
  out <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%Y/%m/%d",
    "%d-%m-%Y"
  )
  for (fmt in formats) {
    need <- is.na(out) & !is.na(x)
    if (!any(need)) {
      break
    }
    parsed <- suppressWarnings(as.POSIXct(strptime(x[need], format = fmt, tz = "UTC")))
    out[need] <- parsed
  }
  out
}
hydrostat_normalize_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}
hydrostat_find_column <- function(data, candidates = character(), patterns = character()) {
  if (is.null(data) || !length(names(data))) {
    return(NA_character_)
  }
  nm <- names(data)
  idx <- match(tolower(candidates), tolower(nm))
  idx <- idx[!is.na(idx)]
  if (length(idx)) {
    return(nm[idx[1]])
  }
  nm_norm <- hydrostat_normalize_name(nm)
  cand_norm <- hydrostat_normalize_name(candidates)
  idx <- match(cand_norm, nm_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx)) {
    return(nm[idx[1]])
  }
  if (length(patterns)) {
    for (pat in patterns) {
      hit <- grep(pat, nm, ignore.case = TRUE, perl = TRUE)
      if (length(hit)) {
        return(nm[hit[1]])
      }
      hit <- grep(pat, nm_norm, ignore.case = TRUE, perl = TRUE)
      if (length(hit)) {
        return(nm[hit[1]])
      }
    }
  }
  NA_character_
}
hydrostat_pick_column <- function(data, candidates = character(), patterns = character(), default = NA_character_) {
  col <- hydrostat_find_column(data, candidates, patterns)
  if (is.na(col)) {
    return(rep(default, nrow(data)))
  }
  data[[col]]
}
hydrostat_standardize_station_code <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("[.]0$", "", x)
  x[x %in% hydrostat_empty_chr] <- NA_character_
  x
}
hydrostat_standardize_rating_curves <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.list(x) && !is.data.frame(x) && "rating_curves" %in% names(x)) x <- x$rating_curves
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) return(data.frame())
  out <- data.frame(
    rating_curve_id = as.character(hydrostat_pick_column(x, c("rating_curve_id", "curve_id"))),
    rating_curve_segment_id = as.character(hydrostat_pick_column(x, c("rating_curve_segment_id", "segment_id"))),
    station_code = hydrostat_standardize_station_code(hydrostat_pick_column(x, c("station_code", "codigoestacao", "CodigoEstacao", "Codigo_Estacao", "CodEstacao"))),
    valid_from = hydrostat_parse_date(hydrostat_pick_column(x, c("valid_from", "Periodo_Validade_Inicio", "PeriodoValidadeInicio", "Data_Validade_Inicio", "DataInicio", "Data_Inicio", "Data_Hora_Dado", "DataHoraDado"))),
    valid_to = hydrostat_parse_date(hydrostat_pick_column(x, c("valid_to", "Periodo_Validade_Fim", "PeriodoValidadeFim", "Data_Validade_Fim", "DataFim", "Data_Fim", "Data_Final"))),
    consistency_level = hydrostat_parse_integer(hydrostat_pick_column(x, c("consistency_level", "Nivel_Consistencia", "NivelConsistencia", "Consistencia"))),
    segment_number_raw = as.character(hydrostat_pick_column(x, c("segment_number_raw", "Numero_Curva", "NumeroCurva", "Num_Curva", "Curva_Numero", "Curva"))),
    segment_number = hydrostat_parse_integer(hydrostat_pick_column(x, c("segment_number", "NumeroSegmento", "Segmento"))),
    curve_type = as.character(hydrostat_pick_column(x, c("curve_type", "Tipo_Curva", "TipoCurva", "Tipo"))),
    equation_type = as.character(hydrostat_pick_column(x, c("equation_type", "Tipo_Equacao", "TipoEquacao", "Equacao_Tipo", "Tipo_Formula", "Formula"))),
    stage_min_cm = hydrostat_parse_number(hydrostat_pick_column(x, c("stage_min_cm", "Cota_Minima", "CotaMinima", "Limite_Inferior", "LimiteInferior", "Cota_Inferior", "CotaInicial", "Cota_Inicial", "Cota_De"))),
    stage_max_cm = hydrostat_parse_number(hydrostat_pick_column(x, c("stage_max_cm", "Cota_Maxima", "CotaMaxima", "Limite_Superior", "LimiteSuperior", "Cota_Superior", "CotaFinal", "Cota_Final", "Cota_Ate"))),
    coefficient_a = hydrostat_parse_number(hydrostat_pick_column(x, c("coefficient_a", "rating_coefficient_a", "Coeficiente_Ajuste_A", "CoeficienteAjusteA", "Coeficiente_A", "CoeficienteA", "Coef_A", "CoefA", "Parametro_A", "ParametroA", "A", "a"))),
    coefficient_h0 = hydrostat_parse_number(hydrostat_pick_column(x, c("coefficient_h0", "rating_h0", "Coeficiente_Ajuste_H0", "CoeficienteAjusteH0", "Coeficiente_H0", "CoeficienteH0", "Coef_H0", "CoefH0", "Parametro_H0", "ParametroH0", "H0", "h0"))),
    coefficient_n = hydrostat_parse_number(hydrostat_pick_column(x, c("coefficient_n", "rating_exponent", "Coeficiente_Ajuste_N", "CoeficienteAjusteN", "Coeficiente_N", "CoeficienteN", "Coef_N", "CoefN", "Parametro_N", "ParametroN", "N", "n", "b"))),
    stringsAsFactors = FALSE
  )
  if (all(is.na(out$rating_curve_id))) {
    out$rating_curve_id <- paste(out$station_code, out$valid_from, out$valid_to, out$consistency_level, sep = "__")
  }
  if (all(is.na(out$rating_curve_segment_id))) {
    out$rating_curve_segment_id <- paste(out$rating_curve_id, out$segment_number_raw, out$stage_min_cm, out$stage_max_cm, sep = "__")
  }
  out <- out[!(is.na(out$valid_from) & is.na(out$valid_to) & is.na(out$stage_min_cm) & is.na(out$stage_max_cm) & is.na(out$coefficient_a) & is.na(out$coefficient_n)), , drop = FALSE]
  row.names(out) <- NULL
  out
}
hydrostat_standardize_discharge_measurements <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.list(x) && !is.data.frame(x) && "discharge_measurements" %in% names(x)) x <- x$discharge_measurements
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) return(data.frame())
  data.frame(
    station_code = hydrostat_standardize_station_code(hydrostat_pick_column(x, c("station_code", "codigoestacao", "CodigoEstacao", "Codigo_Estacao", "CodEstacao"))),
    measurement_datetime = hydrostat_parse_datetime(hydrostat_pick_column(x, c("measurement_datetime", "Data_Hora_Medicao", "DataHoraMedicao", "DataMedicao", "Data_Hora_Dado", "DataHoraDado"))),
    stage_cm = hydrostat_parse_number(hydrostat_pick_column(x, c("stage_cm", "Cota", "NivelAgua", "Nivel_Agua", "water_level"))),
    area_m2 = hydrostat_parse_number(hydrostat_pick_column(x, c("area_m2", "Area", "AreaMolhada", "Area_Molhada"))),
    discharge_m3s = hydrostat_parse_number(hydrostat_pick_column(x, c("discharge_m3s", "Vazao", "DescargaLiquida", "Descarga_Liquida"))),
    mean_velocity_ms = hydrostat_parse_number(hydrostat_pick_column(x, c("mean_velocity_ms", "VelocidadeMedia", "Velocidade_Media"))),
    width_m = hydrostat_parse_number(hydrostat_pick_column(x, c("width_m", "Largura"))),
    consistency_level = hydrostat_parse_integer(hydrostat_pick_column(x, c("consistency_level", "NivelConsistencia", "Nivel_Consistencia"))),
    stringsAsFactors = FALSE
  )
}
hydrostat_standardize_cross_section_vertices <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.list(x) && !is.data.frame(x)) {
    if ("vertices" %in% names(x)) x <- x$vertices else if ("cross_section_vertices" %in% names(x)) x <- x$cross_section_vertices
  }
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) return(data.frame())
  data.frame(
    station_code = hydrostat_standardize_station_code(hydrostat_pick_column(x, c("station_code", "codigoestacao", "CodigoEstacao", "Codigo_Estacao"))),
    section_id = as.character(hydrostat_pick_column(x, c("section_id", "cross_section_id", "profile_id", "perfil_id"))),
    section_date = hydrostat_parse_date(hydrostat_pick_column(x, c("section_date", "measurement_datetime", "Data", "DataMedicao", "Data_Hora_Medicao"))),
    distance_m = hydrostat_parse_number(hydrostat_pick_column(x, c("distance_m", "distance", "Distancia", "distancia"))),
    elevation_cm = hydrostat_parse_number(hydrostat_pick_column(x, c("elevation_cm", "stage_cm", "Cota", "cota", "elevation"))),
    stringsAsFactors = FALSE
  )
}
if (exists("ana_all_get_api_rating_curves", mode = "function")) {
  .hydrostat_original_ana_all_get_api_rating_curves <- ana_all_get_api_rating_curves
  ana_all_get_api_rating_curves <- function(...) {
    hydrostat_standardize_rating_curves(.hydrostat_original_ana_all_get_api_rating_curves(...))
  }
}
# Calendar and request-report helpers used by export diagnostics.
hydro_export_complete_daily_calendar <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(data)
  data <- hydrostat_prefer_daily_rows(data)
  data$date <- hydrostat_parse_date(data$date)
  data <- data[!is.na(data$date), , drop = FALSE]
  if (!nrow(data)) return(data)
  if (!"station_code" %in% names(data)) data$station_code <- NA_character_
  if (!"variable" %in% names(data)) data$variable <- NA_character_
  keys <- unique(data.frame(
    station_code = as.character(data$station_code),
    variable = as.character(data$variable),
    stringsAsFactors = FALSE
  ))
  pieces <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    st <- keys$station_code[i]
    var <- keys$variable[i]
    d <- data[as.character(data$station_code) == st & as.character(data$variable) == var, , drop = FALSE]
    if (!nrow(d)) next
    d <- d[order(d$date), , drop = FALSE]
    unit_value <- if ("unit" %in% names(d)) d$unit[which(!is.na(d$unit) & nzchar(as.character(d$unit)))[1]] else NA_character_
    source_value <- if ("source" %in% names(d)) d$source[which(!is.na(d$source) & nzchar(as.character(d$source)))[1]] else NA_character_
    cal <- data.frame(
      station_code = st,
      variable = var,
      date = seq.Date(min(d$date), max(d$date), by = "day"),
      stringsAsFactors = FALSE
    )
    merged <- merge(cal, d, by = c("station_code", "variable", "date"), all.x = TRUE, sort = FALSE)
    merged <- merged[order(merged$date), , drop = FALSE]
    if ("unit" %in% names(merged)) {
      missing_unit <- is.na(merged$unit) | !nzchar(as.character(merged$unit))
      merged$unit[missing_unit] <- unit_value
    }
    if ("source" %in% names(merged)) {
      missing_source <- is.na(merged$source) | !nzchar(as.character(merged$source))
      merged$source[missing_source] <- source_value
    }
    if ("source_status" %in% names(merged)) {
      missing_status <- is.na(merged$source_status) | !nzchar(as.character(merged$source_status))
      merged$source_status[missing_status] <- "missing"
    }
    pieces[[i]] <- merged
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(data)
  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out
}
hydro_export_report_subset <- function(request_report, patterns) {
  if (!is.data.frame(request_report) || !nrow(request_report)) return(data.frame())
  txt <- apply(request_report, 1L, function(z) paste(z, collapse = " "))
  keep <- rep(FALSE, length(txt))
  for (pat in patterns) keep <- keep | grepl(pat, txt, ignore.case = TRUE)
  request_report[keep, , drop = FALSE]
}
hydro_export_status_count <- function(x, status) {
  if (!is.data.frame(x) || !nrow(x) || !"status" %in% names(x)) return(NA_integer_)
  sum(tolower(as.character(x$status)) == status, na.rm = TRUE)
}
hydro_export_report_n_rows <- function(x) {
  if (!is.data.frame(x) || !nrow(x)) return(numeric())
  if ("n_rows" %in% names(x)) return(hydrostat_parse_number(x$n_rows))
  if ("n_records" %in% names(x)) return(hydrostat_parse_number(x$n_records))
  numeric()
}
hydro_export_request_daily_diagnostics <- function(request_report, variable) {
  product <- switch(variable, discharge = "daily_discharge", rainfall = "daily_rainfall", stage = "daily_stage", variable)
  rr <- hydro_export_report_subset(request_report, c(product, variable))
  if (!nrow(rr)) return(data.frame())
  if ("status" %in% names(rr)) {
    st <- tolower(as.character(rr$status))
    if (length(st) && all(st %in% c("skipped"))) return(data.frame())
  }
  n_rows <- hydro_export_report_n_rows(rr)
  n_success <- hydro_export_status_count(rr, "success")
  n_empty <- hydro_export_status_count(rr, "empty")
  n_error <- hydro_export_status_count(rr, "error")
  n_skipped <- hydro_export_status_count(rr, "skipped")
  median_rows <- if (length(n_rows) && any(!is.na(n_rows))) stats::median(n_rows, na.rm = TRUE) else NA_real_
  suspicious <- if (length(n_rows)) sum(!is.na(n_rows) & n_rows > 0 & n_rows <= 31) else NA_integer_
  message <- character()
  if (!is.na(n_empty) && n_empty > 0L) message <- c(message, paste0("API returned empty ", product, " windows (n = ", n_empty, ")"))
  if (!is.na(n_error) && n_error > 0L) message <- c(message, paste0("request errors (n = ", n_error, ")"))
  if (!is.na(suspicious) && suspicious > 0L) message <- c(message, paste0("sparse API windows (n = ", suspicious, ")"))
  data.frame(
    station_code = hydro_export_first_existing_value(rr, c("station_code", "CodigoEstacao", "codEstacao", "station")),
    variable = variable,
    first_date = NA_character_,
    last_date = NA_character_,
    expected_days = NA_integer_,
    exported_days = 0L,
    non_missing_days = 0L,
    missing_days = NA_integer_,
    coverage_pct = NA_real_,
    request_success_windows = n_success,
    request_empty_windows = n_empty,
    request_error_windows = n_error,
    request_skipped_windows = n_skipped,
    request_success_rows_median = median_rows,
    suspicious_request_windows = suspicious,
    status = if (length(message)) "warning" else "ok",
    message = if (length(message)) paste(message, collapse = "; ") else "ok",
    stringsAsFactors = FALSE
  )
}
hydro_export_rating_request_diagnostics <- function(request_report) {
  rr <- hydro_export_report_subset(request_report, c("rating_curves", "curva"))
  if (!nrow(rr)) return(data.frame())
  if ("status" %in% names(rr)) {
    st <- tolower(as.character(rr$status))
    if (length(st) && all(st %in% c("skipped"))) return(data.frame())
  }
  n_rows <- hydro_export_report_n_rows(rr)
  n_success <- hydro_export_status_count(rr, "success")
  n_empty <- hydro_export_status_count(rr, "empty")
  n_error <- hydro_export_status_count(rr, "error")
  message <- character()
  if (!is.na(n_empty) && n_empty > 0L) message <- c(message, paste0("API returned empty rating_curves windows (n = ", n_empty, ")"))
  if (!is.na(n_error) && n_error > 0L) message <- c(message, paste0("request errors (n = ", n_error, ")"))
  data.frame(
    station_code = hydro_export_first_existing_value(rr, c("station_code", "CodigoEstacao", "codEstacao", "station")),
    product = "rating_curves",
    request_success_windows = n_success,
    request_empty_windows = n_empty,
    request_error_windows = n_error,
    returned_rows = if (length(n_rows) && any(!is.na(n_rows))) sum(n_rows, na.rm = TRUE) else NA_real_,
    status = if (length(message)) "warning" else "ok",
    message = if (length(message)) paste(message, collapse = "; ") else "ok",
    stringsAsFactors = FALSE
  )
}
hydro_export_first_existing_value <- function(x, candidates) {
  if (!is.data.frame(x) || !nrow(x)) return(NA_character_)
  for (nm in candidates) {
    if (nm %in% names(x)) {
      vals <- as.character(x[[nm]])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals)) return(vals[1])
    }
  }
  NA_character_
}
hydro_export_collect_tables <- function(x, hydrological_year_start, low_flow_durations) {
  out <- list()
  request_report <- hydro_export_extract_request_report(x)
  daily <- hydro_export_extract_daily(x)
  discharge <- daily[daily$variable == "discharge", , drop = FALSE]
  stage <- daily[daily$variable == "stage", , drop = FALSE]
  rainfall <- daily[daily$variable == "rainfall", , drop = FALSE]
  if (nrow(discharge)) {
    out$discharge_monthly_mean_wide <- hydro_export_monthly_wide(discharge, fun = mean, value_name = "Media Anual")
    out$discharge_monthly_mean_long <- hydro_export_monthly_long(discharge, fun = mean, label = "Vazao media")
    out$discharge_missing_counts <- hydro_export_missing_wide(discharge)
    out$discharge_daily_wide <- hydro_export_daily_wide(discharge)
    out$discharge_daily_long <- hydro_export_daily_long(discharge, "Vazao")
    out$discharge_annual_maxima <- hydro_export_annual_maxima(discharge, "Vazao", hydrological_year_start)
    out$discharge_annual_low_flows <- hydro_export_annual_low_flows(discharge, low_flow_durations)
    out$discharge_flow_duration <- hydro_export_flow_duration(discharge)
    out$discharge_diagnostics <- hydro_export_daily_diagnostics(discharge, request_report, "discharge")
  } else {
    diag <- hydro_export_request_daily_diagnostics(request_report, "discharge")
    if (nrow(diag)) out$discharge_diagnostics <- diag
  }
  if (nrow(rainfall)) {
    out$rainfall_monthly_total_wide <- hydro_export_monthly_wide(rainfall, fun = sum, value_name = "Total Anual")
    out$rainfall_monthly_total_long <- hydro_export_monthly_long(rainfall, fun = sum, label = "Precipitacao total")
    out$rainfall_missing_counts <- hydro_export_missing_wide(rainfall)
    out$rainfall_daily_wide <- hydro_export_daily_wide(rainfall)
    out$rainfall_daily_long <- hydro_export_daily_long(rainfall, "Precipitacao")
    out$rainfall_annual_maxima <- hydro_export_annual_maxima(rainfall, "Precipitacao", hydrological_year_start)
    out$rainfall_indices <- hydro_export_rainfall_indices(rainfall, hydrological_year_start)
  } else {
    diag <- hydro_export_request_daily_diagnostics(request_report, "rainfall")
    if (nrow(diag)) out$rainfall_diagnostics <- diag
  }
  if (nrow(request_report)) out$request_report <- hydro_export_safe_table(request_report)
  rating <- hydro_export_extract_named_table(x, "rating_curves")
  rating <- hydrostat_standardize_rating_curves(rating)
  if (nrow(rating)) {
    out$rating_curves <- hydro_export_rating_curves(rating)
  } else {
    diag <- hydro_export_rating_request_diagnostics(request_report)
    if (nrow(diag)) out$rating_diagnostics <- diag
  }
  measurements <- hydro_export_extract_named_table(x, "discharge_measurements")
  measurements <- hydrostat_standardize_discharge_measurements(measurements)
  if (nrow(measurements)) out$discharge_measurements <- hydro_export_measurements(measurements)
  cross <- hydro_export_extract_cross_sections(x)
  if (nrow(cross)) out$cross_sections <- hydro_export_cross_sections(cross)
  for (nm in c("rainfall_diagnostics", "measurement_diagnostics", "rating_diagnostics")) {
    if (nm %in% names(out)) next
    tbl <- hydro_export_extract_named_table(x, nm)
    if (is.data.frame(tbl) && nrow(tbl)) out[[nm]] <- hydro_export_safe_table(tbl)
  }
  out <- out[vapply(out, function(z) is.data.frame(z) && nrow(z) > 0L, logical(1))]
  out
}
hydro_export_extract_daily <- function(x) {
  pieces <- list()
  add <- function(z) {
    if (is.null(z)) return(NULL)
    if (is.data.frame(z)) return(hydrostat_standardize_ana_daily_table(z, "hydroDataBR"))
    if (is.list(z)) {
      out <- list()
      for (nm in names(z)) {
        if (nm %in% c("daily_data", "data", "daily_discharge", "daily_stage", "daily_rainfall", "discharge", "stage", "rainfall")) out[[length(out) + 1L]] <- add(z[[nm]])
        if (nm == "results" && is.list(z[[nm]])) out[[length(out) + 1L]] <- add(z[[nm]])
      }
      if (length(out)) return(do.call(rbind, out))
    }
    NULL
  }
  pieces[[1]] <- add(x)
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(hydrostat_empty_daily_series())
  hydrostat_prefer_daily_rows(do.call(rbind, pieces))
}
hydro_export_extract_named_table <- function(x, name) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(data.frame())
  if (is.list(x) && name %in% names(x) && is.data.frame(x[[name]])) return(x[[name]])
  if (is.list(x) && "results" %in% names(x) && is.list(x$results)) {
    pieces <- lapply(x$results, hydro_export_extract_named_table, name = name)
    pieces <- pieces[vapply(pieces, function(z) is.data.frame(z) && nrow(z) > 0L, logical(1))]
    if (length(pieces)) return(do.call(rbind, pieces))
  }
  data.frame()
}
hydro_export_extract_request_report <- function(x) hydro_export_extract_named_table(x, "request_report")
hydro_export_extract_cross_sections <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.list(x) && "cross_sections" %in% names(x)) return(hydrostat_standardize_cross_section_vertices(x$cross_sections))
  if (is.list(x) && "vertices" %in% names(x)) return(hydrostat_standardize_cross_section_vertices(x$vertices))
  if (is.list(x) && "results" %in% names(x) && is.list(x$results)) {
    pieces <- lapply(x$results, hydro_export_extract_cross_sections)
    pieces <- pieces[vapply(pieces, function(z) is.data.frame(z) && nrow(z) > 0L, logical(1))]
    if (length(pieces)) return(do.call(rbind, pieces))
  }
  data.frame()
}
hydro_export_station_groups <- function(data) split(data, data$station_code, drop = TRUE)
hydro_export_month_labels <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
hydro_export_monthly_wide <- function(data, fun, value_name) {
  data <- hydro_export_complete_daily_calendar(data)
  groups <- hydro_export_station_groups(data)
  ans <- lapply(names(groups), function(st) {
    d <- groups[[st]]
    d$year <- as.integer(format(d$date, "%Y"))
    d$month <- as.integer(format(d$date, "%m"))
    mat <- data.frame(station_code = st, ANO = sort(unique(d$year)), stringsAsFactors = FALSE)
    for (m in seq_len(12)) {
      vals <- tapply(d$value[d$month == m], d$year[d$month == m], function(v) {
        if (all(is.na(v))) NA_real_ else fun(v, na.rm = TRUE)
      })
      mat[[hydro_export_month_labels[m]]] <- as.numeric(vals[as.character(mat$ANO)])
    }
    mat[[value_name]] <- apply(mat[hydro_export_month_labels], 1L, function(v) {
      if (all(is.na(v))) NA_real_ else fun(v, na.rm = TRUE)
    })
    mat
  })
  out <- do.call(rbind, ans)
  row.names(out) <- NULL
  out
}
hydro_export_monthly_long <- function(data, fun, label) {
  data <- hydro_export_complete_daily_calendar(data)
  groups <- split(data, paste(data$station_code, format(data$date, "%Y-%m"), sep = "\r"), drop = TRUE)
  rows <- lapply(groups, function(d) {
    value <- if (all(is.na(d$value))) NA_real_ else fun(d$value, na.rm = TRUE)
    data.frame(station_code = d$station_code[1], Data = format(min(d$date), "%m/%Y"), value = value, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out)[names(out) == "value"] <- label
  out <- out[order(out$station_code, hydrostat_parse_date(paste0("01/", out$Data))), , drop = FALSE]
  row.names(out) <- NULL
  out
}
hydro_export_daily_long <- function(data, label) {
  data <- hydro_export_complete_daily_calendar(data)
  out <- data.frame(station_code = data$station_code, Data = format(data$date, "%d/%m/%Y"), value = data$value, stringsAsFactors = FALSE)
  names(out)[3] <- label
  out <- out[order(out$station_code, hydrostat_parse_date(out$Data)), , drop = FALSE]
  row.names(out) <- NULL
  out
}
hydro_export_daily_wide <- function(data) {
  data <- hydro_export_complete_daily_calendar(data)
  groups <- split(data, paste(data$station_code, format(data$date, "%Y-%m"), sep = "\r"), drop = TRUE)
  rows <- lapply(groups, function(d) {
    row <- data.frame(station_code = d$station_code[1], Data = format(min(d$date), "%b/%Y"), stringsAsFactors = FALSE)
    for (i in seq_len(31)) row[[sprintf("%02d", i)]] <- NA_real_
    day <- as.integer(format(d$date, "%d"))
    for (i in seq_along(day)) row[[sprintf("%02d", day[i])]] <- d$value[i]
    row
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
hydro_export_month_calendar <- function(station, dates) {
  first <- as.Date(format(min(dates), "%Y-%m-01"))
  last <- as.Date(format(max(dates), "%Y-%m-01"))
  months <- seq.Date(first, last, by = "month")
  lapply(months, function(m) {
    next_m <- seq.Date(m, by = "month", length.out = 2L)[2]
    data.frame(station_code = station, date = seq.Date(m, next_m - 1L, by = "day"), stringsAsFactors = FALSE)
  })
}
hydro_export_missing_wide <- function(data) {
  data <- hydro_export_complete_daily_calendar(data)
  out <- list()
  for (st in names(hydro_export_station_groups(data))) {
    d <- data[data$station_code == st, , drop = FALSE]
    d$year <- as.integer(format(d$date, "%Y"))
    d$month <- as.integer(format(d$date, "%m"))
    yrs <- sort(unique(d$year))
    mat <- data.frame(station_code = st, ANO = yrs, stringsAsFactors = FALSE)
    for (m in seq_len(12)) {
      vals <- tapply(is.na(d$value[d$month == m]), d$year[d$month == m], sum)
      mat[[hydro_export_month_labels[m]]] <- as.integer(vals[as.character(yrs)])
    }
    mat$`Total Anual` <- rowSums(mat[hydro_export_month_labels], na.rm = TRUE)
    out[[length(out) + 1L]] <- mat
  }
  ans <- do.call(rbind, out)
  row.names(ans) <- NULL
  ans
}
hydro_export_hydro_year <- function(date, start_month) {
  y <- as.integer(format(date, "%Y")); m <- as.integer(format(date, "%m"))
  y0 <- ifelse(m >= start_month, y, y - 1L)
  paste0(y0, "/", y0 + 1L)
}
hydro_export_annual_maxima <- function(data, label, start_month) {
  data <- hydro_export_complete_daily_calendar(data)
  data$hydro_year <- hydro_export_hydro_year(data$date, start_month)
  split_data <- split(data, paste(data$station_code, data$hydro_year, sep = "\r"), drop = TRUE)
  rows <- lapply(split_data, function(d) {
    vals <- d$value
    max_value <- if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
    dates <- if (is.na(max_value)) as.Date(character()) else d$date[!is.na(vals) & vals == max_value]
    missing_n <- sum(is.na(vals))
    flag <- character()
    if (missing_n > 0L) flag <- c(flag, paste0("falhas no ano (n = ", missing_n, ")"))
    if (length(dates) > 1L) {
      shown <- format(dates[seq_len(min(5L, length(dates)))], "%d/%m/%Y")
      extra <- length(dates) - length(shown)
      txt <- paste(shown, collapse = ", ")
      if (extra > 0L) txt <- paste0(txt, "; +", extra, " datas")
      flag <- c(flag, paste0("maximo repetido (datas: ", txt, ")"))
    }
    data.frame(
      station_code = d$station_code[1],
      `Ano hidrologico` = d$hydro_year[1],
      Data = if (length(dates)) format(dates[1], "%d/%m/%Y") else NA_character_,
      value = max_value,
      Flag = if (length(flag)) paste(flag, collapse = "; ") else "sem flag",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  names(out)[names(out) == "value"] <- label
  row.names(out) <- NULL
  out
}
hydro_export_annual_low_flows <- function(data, durations) {
  data <- hydro_export_complete_daily_calendar(data)
  data$year <- as.integer(format(data$date, "%Y"))
  split_data <- split(data, paste(data$station_code, data$year, sep = "\r"), drop = TRUE)
  rows <- list()
  for (key in names(split_data)) {
    d <- split_data[[key]]
    row <- data.frame(station_code = d$station_code[1], Ano = d$year[1], stringsAsFactors = FALSE)
    d <- d[order(d$date), , drop = FALSE]
    for (dur in durations) {
      if (sum(!is.na(d$value)) < dur) {
        row[[paste0("Q", dur)]] <- NA_real_
      } else {
        roll <- as.numeric(stats::filter(d$value, rep(1 / dur, dur), sides = 1))
        row[[paste0("Q", dur)]] <- if (all(is.na(roll))) NA_real_ else min(roll, na.rm = TRUE)
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
hydro_export_flow_duration <- function(data) {
  rows <- list()
  for (st in names(hydro_export_station_groups(data))) {
    v <- sort(data$value[data$station_code == st & !is.na(data$value)], decreasing = TRUE)
    if (!length(v)) next
    p <- seq_along(v) / length(v) * 100
    q <- stats::approx(x = p, y = v, xout = 1:99, rule = 2, ties = "ordered")$y
    rows[[length(rows) + 1L]] <- data.frame(station_code = st, `Permanencia (%)` = 1:99, Qp = q, check.names = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}
hydro_export_rainfall_indices <- function(data, start_month) {
  data <- hydro_export_complete_daily_calendar(data)
  data$hydro_year <- hydro_export_hydro_year(data$date, start_month)
  rows <- lapply(split(data, paste(data$station_code, data$hydro_year, sep = "\r"), drop = TRUE), function(d) {
    v <- d$value
    missing_n <- sum(is.na(v))
    observed_n <- sum(!is.na(v))
    all_missing <- observed_n == 0L
    wet <- v[!is.na(v) & v >= 1]
    if (length(v) < 5L) {
      rx5 <- NA_real_
    } else {
      rx5 <- as.numeric(stats::filter(v, rep(1, 5), sides = 1))
    }
    data.frame(
      station_code = d$station_code[1],
      `Ano hidrologico` = d$hydro_year[1],
      total_mm = if (all_missing) NA_real_ else sum(v, na.rm = TRUE),
      Rx1day = if (all_missing) NA_real_ else max(v, na.rm = TRUE),
      Rx5day = if (observed_n < 5L || all(is.na(rx5))) NA_real_ else max(rx5, na.rm = TRUE),
      R10 = if (all_missing) NA_integer_ else sum(v >= 10, na.rm = TRUE),
      R20 = if (all_missing) NA_integer_ else sum(v >= 20, na.rm = TRUE),
      R50 = if (all_missing) NA_integer_ else sum(v >= 50, na.rm = TRUE),
      SDII = if (length(wet)) mean(wet) else NA_real_,
      CDD = if (all_missing) NA_integer_ else hydro_export_max_run(ifelse(is.na(v), FALSE, v < 1)),
      CWD = if (all_missing) NA_integer_ else hydro_export_max_run(ifelse(is.na(v), FALSE, v >= 1)),
      dias_falhos = missing_n,
      Flag = if (all_missing) paste0("ano sem dados (n = ", missing_n, ")") else if (missing_n > 0L) paste0("falhas no ano (n = ", missing_n, ")") else "sem flag",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
hydro_export_max_run <- function(x) {
  if (!length(x)) return(0L)
  r <- rle(as.logical(x)); if (!any(r$values)) return(0L); max(r$lengths[r$values])
}
hydro_export_range <- function(a, b, date = FALSE) {
  if (date) {
    a <- ifelse(is.na(a), NA_character_, format(as.Date(a), "%d/%m/%Y")); b <- ifelse(is.na(b), NA_character_, format(as.Date(b), "%d/%m/%Y"))
  }
  ifelse(is.na(a) & is.na(b), NA_character_, paste0(ifelse(is.na(a), "", a), " - ", ifelse(is.na(b), "", b)))
}
hydro_export_rating_curves <- function(rating) {
  data.frame(
    station_code = rating$station_code,
    `Validade de data` = hydro_export_range(rating$valid_from, rating$valid_to, TRUE),
    `Validade de cota` = hydro_export_range(rating$stage_min_cm, rating$stage_max_cm, FALSE),
    a = rating$coefficient_a,
    b = rating$coefficient_n,
    h0 = rating$coefficient_h0,
    `Numero curva` = rating$segment_number_raw,
    `Nivel consistencia` = rating$consistency_level,
    `Tipo curva` = rating$curve_type,
    `Tipo equacao` = rating$equation_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
hydro_export_measurements <- function(x) {
  data.frame(station_code = x$station_code, Data = format(as.Date(x$measurement_datetime), "%d/%m/%Y"), Cota = x$stage_cm, Area = x$area_m2, Vazao = x$discharge_m3s, Velocidade = x$mean_velocity_ms, Largura = x$width_m, `Nivel consistencia` = x$consistency_level, check.names = FALSE, stringsAsFactors = FALSE)
}
hydro_export_cross_sections <- function(x) {
  data.frame(station_code = x$station_code, Data = format(x$section_date, "%d/%m/%Y"), Distancia = x$distance_m, Cota = x$elevation_cm, section_id = x$section_id, stringsAsFactors = FALSE)
}
hydro_export_daily_diagnostics <- function(data, request_report, variable) {
  data <- hydro_export_complete_daily_calendar(data)
  rows <- list()
  for (st in names(hydro_export_station_groups(data))) {
    d <- data[data$station_code == st, , drop = FALSE]
    expected <- as.integer(max(d$date) - min(d$date) + 1L)
    non_missing <- sum(!is.na(d$value))
    rr <- hydro_export_report_subset(request_report, c(switch(variable, discharge = "daily_discharge", rainfall = "daily_rainfall", stage = "daily_stage", variable), variable))
    n_success <- hydro_export_status_count(rr, "success")
    n_error <- hydro_export_status_count(rr, "error")
    n_empty <- hydro_export_status_count(rr, "empty")
    n_skipped <- hydro_export_status_count(rr, "skipped")
    n_rows <- hydro_export_report_n_rows(rr)
    median_rows <- if (length(n_rows) && any(!is.na(n_rows))) stats::median(n_rows, na.rm = TRUE) else NA_real_
    suspicious <- if (length(n_rows)) sum(!is.na(n_rows) & n_rows > 0 & n_rows <= 31) else NA_integer_
    coverage <- round(100 * non_missing / expected, 2)
    message <- character()
    if (coverage < 80) message <- c(message, "low coverage")
    if (!is.na(suspicious) && suspicious > 0) message <- c(message, paste0("sparse API windows (n = ", suspicious, ")"))
    if (!is.na(n_error) && n_error > 0) message <- c(message, paste0("request errors (n = ", n_error, ")"))
    rows[[length(rows) + 1L]] <- data.frame(
      station_code = st,
      variable = variable,
      first_date = format(min(d$date), "%Y-%m-%d"),
      last_date = format(max(d$date), "%Y-%m-%d"),
      expected_days = expected,
      exported_days = nrow(d),
      non_missing_days = non_missing,
      missing_days = expected - non_missing,
      coverage_pct = coverage,
      request_success_windows = n_success,
      request_empty_windows = n_empty,
      request_error_windows = n_error,
      request_skipped_windows = n_skipped,
      request_success_rows_median = median_rows,
      suspicious_request_windows = suspicious,
      status = if (length(message)) "warning" else "ok",
      message = if (length(message)) paste(message, collapse = "; ") else "ok",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}
hydro_export_safe_table <- function(x) {
  sensitive <- grepl("token|authorization|cpf|cnpj|senha|password|header", names(x), ignore.case = TRUE)
  x[, !sensitive, drop = FALSE]
}
hydro_export_collect_plots <- function(x) {
  if (inherits(x, "ggplot")) list(plot = x) else list()
}
hydro_export_aliases <- function() {
  c(
    tab01_discharge_wide = "discharge_monthly_mean_wide",
    tab01_discharge_long = "discharge_monthly_mean_long",
    tab02_discharge = "discharge_missing_counts",
    tab03_discharge_wide = "discharge_daily_wide",
    tab03_discharge_long = "discharge_daily_long",
    tab04_discharge = "discharge_annual_maxima",
    tab05_discharge = "discharge_annual_low_flows",
    tab06_discharge = "discharge_flow_duration",
    tab01_rainfall_wide = "rainfall_monthly_total_wide",
    tab01_rainfall_long = "rainfall_monthly_total_long",
    tab02_rainfall = "rainfall_missing_counts",
    tab03_rainfall_wide = "rainfall_daily_wide",
    tab03_rainfall_long = "rainfall_daily_long",
    tab04_rainfall = "rainfall_annual_maxima",
    tab11 = "rainfall_indices",
    tab12 = "rating_curves",
    tab13 = "cross_sections",
    tab14 = "discharge_measurements",
    tab15 = "request_report",
    tab16 = "rainfall_diagnostics",
    tab17 = "measurement_diagnostics",
    tab18 = "rating_diagnostics",
    tab19 = "discharge_diagnostics"
  )
}
hydro_export_filter_components <- function(x, components) {
  if (identical(components, "all")) return(x)
  aliases <- hydro_export_aliases()
  components <- ifelse(components %in% names(aliases), aliases[components], components)
  x[intersect(components, names(x))]
}
hydro_export_output_path <- function(path, name, format, multiple) {
  if (multiple || !grepl(paste0("[.]", format, "$"), path, ignore.case = TRUE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(file.path(path, paste0(name, ".", format)))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}
hydro_export_write_components <- function(x, path, format, overwrite, ...) {
  multiple <- length(x) > 1L
  rows <- list()
  for (nm in names(x)) {
    target <- hydro_export_output_path(path, nm, format, multiple)
    if (file.exists(target) && !overwrite) stop("File exists: ", target, call. = FALSE)
    if (format == "csv") utils::write.csv(x[[nm]], target, row.names = FALSE, na = "")
    if (format == "rds") saveRDS(x[[nm]], target)
    if (format %in% c("png", "pdf")) ggplot2::ggsave(filename = target, plot = x[[nm]], ...)
    rows[[length(rows) + 1L]] <- data.frame(component = nm, file = target, format = format, object_class = paste(class(x[[nm]]), collapse = "/"), rows = if (is.data.frame(x[[nm]])) nrow(x[[nm]]) else NA_integer_, columns = if (is.data.frame(x[[nm]])) ncol(x[[nm]]) else NA_integer_, status = "written", stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}
hydro_export_write_manifest <- function(manifest_df, path, overwrite, manifest) {
  if (!isTRUE(manifest)) return(invisible(FALSE))
  dir <- if (dir.exists(path) || !grepl("[.]", basename(path))) path else dirname(path)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(dir, "hydro_export_manifest.csv")
  if (file.exists(target) && !overwrite) stop("File exists: ", target, call. = FALSE)
  utils::write.csv(manifest_df, target, row.names = FALSE, na = "")
  invisible(TRUE)
}
