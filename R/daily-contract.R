# Daily-series contract, local-reader parsing, and consistency preference.
ana_daily_contract_columns <- function() {
  c(
    "station_code", "date", "variable", "value", "unit",
    "consistency_level", "source_status", "source"
  )
}
ana_empty_daily_series <- function() {
  tibble::tibble(
    station_code = character(),
    date = as.Date(character()),
    variable = character(),
    value = numeric(),
    unit = character(),
    consistency_level = character(),
    source_status = character(),
    source = character()
  )
}
ana_keep_daily_contract <- function(data) {
  if (is.null(data)) {
    return(ana_empty_daily_series())
  }
  data <- tibble::as_tibble(data)
  n <- nrow(data)
  columns <- ana_daily_contract_columns()
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- rep(NA, n)
  }
  data <- data[, columns]
  data$station_code <- ana_normalize_station_code(data$station_code)
  data$date <- .parse_ana_date(data$date)
  data$variable <- as.character(data$variable)
  data$value <- .parse_ana_numeric(data$value)
  data$unit <- as.character(data$unit)
  data$consistency_level <- as.character(data$consistency_level)
  data$source_status <- as.character(data$source_status)
  data$source <- as.character(data$source)
  data
}
ana_prefer_daily_rows <- function(data) {
  data <- ana_keep_daily_contract(data)
  if (nrow(data) == 0L) {
    return(data)
  }
  data <- data[!is.na(data$date) & !is.na(data$station_code) & nzchar(data$variable), , drop = FALSE]
  if (nrow(data) == 0L) {
    return(ana_empty_daily_series())
  }
  consistency_priority <- ifelse(
    data$consistency_level == "2", 1L,
    ifelse(data$consistency_level == "1", 2L, 3L)
  )
  value_priority <- ifelse(is.na(data$value), 2L, 1L)
  row_order <- seq_len(nrow(data))
  ord <- order(
    data$station_code, data$date, data$variable,
    consistency_priority, value_priority, row_order,
    na.last = TRUE
  )
  data <- data[ord, , drop = FALSE]
  key <- paste(data$station_code, data$date, data$variable, sep = "\r")
  data <- data[!duplicated(key), , drop = FALSE]
  row.names(data) <- NULL
  ana_keep_daily_contract(data)
}
hydro_prefer_daily_observations <- function(x) {
  if (is.data.frame(x)) {
    if (!all(c("date", "value") %in% names(x))) {
      return(x)
    }
    data <- x
    if (!"station_code" %in% names(data)) {
      data$station_code <- "station"
    }
    if (!"variable" %in% names(data)) {
      data$variable <- "series"
    }
    if (!"consistency_level" %in% names(data)) {
      data$consistency_level <- NA_character_
    }
    if (nrow(data) == 0L) {
      return(data)
    }
    data$station_code <- ana_normalize_station_code(data$station_code)
    data$date <- .parse_ana_date(data$date)
    data$value <- .parse_ana_numeric(data$value)
    data$variable <- as.character(data$variable)
    data$consistency_level <- as.character(data$consistency_level)
    data$.hydro_original_order <- seq_len(nrow(data))
    consistency_priority <- ifelse(
      data$consistency_level == "2", 1L,
      ifelse(data$consistency_level == "1", 2L, 3L)
    )
    value_priority <- ifelse(is.na(data$value), 2L, 1L)
    ord <- order(
      data$station_code, data$date, data$variable,
      consistency_priority, value_priority, data$.hydro_original_order,
      na.last = TRUE
    )
    data <- data[ord, , drop = FALSE]
    key <- paste(data$station_code, data$date, data$variable, sep = "
")
    data <- data[!duplicated(key), , drop = FALSE]
    data$.hydro_original_order <- NULL
    row.names(data) <- NULL
    return(data)
  }
  if (!is.list(x)) {
    return(x)
  }
  for (i in seq_along(x)) {
    x[[i]] <- hydro_prefer_daily_observations(x[[i]])
  }
  x
}
hydro_prefer_daily_rows <- function(data, ...) {
  ana_prefer_daily_rows(data)
}
hydrostat_prefer_daily_rows <- function(data, ...) {
  ana_prefer_daily_rows(data)
}
hydrostat_daily_contract_columns <- function() {
  ana_daily_contract_columns()
}
hydrostat_empty_daily_series <- function() {
  ana_empty_daily_series()
}
hydrostat_daily_public_contract <- function(x) {
  ana_keep_daily_contract(x)
}
.check_local_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Informe um unico caminho de arquivo.", call. = FALSE)
  }
  if (grepl("^https?://", path, ignore.case = TRUE)) {
    stop("Esta funcao le apenas arquivos locais.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Arquivo nao encontrado: ", path, call. = FALSE)
  }
  path
}
.parse_ana_numeric <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null", "None", "none")] <- NA_character_
  x <- gsub("\\s+", "", x)
  both <- grepl(",", x, fixed = TRUE) & grepl(".", x, fixed = TRUE)
  x[both] <- gsub(".", "", x[both], fixed = TRUE)
  x[both] <- gsub(",", ".", x[both], fixed = TRUE)
  comma_only <- grepl(",", x, fixed = TRUE) & !grepl(".", x, fixed = TRUE)
  x[comma_only] <- gsub(",", ".", x[comma_only], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
.parse_ana_date <- function(x) {
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
  x[x %in% c("", "NA", "NaN", "NULL", "null", "None", "none")] <- NA_character_
  x <- sub("T", " ", x, fixed = TRUE)
  x <- sub("[.][0-9]+Z?$", "", x)
  x <- sub("Z$", "", x)
  out <- rep(as.Date(NA), length(x))
  formats <- c(
    "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%Y%m%d",
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
    "%Y/%m/%d %H:%M:%S"
  )
  for (fmt in formats) {
    need <- is.na(out) & !is.na(x)
    if (!any(need)) {
      break
    }
    out[need] <- suppressWarnings(as.Date(x[need], format = fmt))
  }
  out
}
.make_ana_month_day_date <- function(month_date, day_value) {
  month_date <- .parse_ana_date(month_date)
  year_value <- as.integer(format(month_date, "%Y"))
  month_value <- as.integer(format(month_date, "%m"))
  day_value <- as.integer(day_value)
  date_value <- suppressWarnings(as.Date(ISOdate(year_value, month_value, day_value, tz = "UTC")))
  valid <- !is.na(month_date) & !is.na(date_value) &
    format(date_value, "%Y-%m") == format(month_date, "%Y-%m")
  date_value[!valid] <- as.Date(NA)
  date_value
}
ana_normalize_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}
ana_normalize_station_code <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("[.]0$", "", x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "None", "none")] <- NA_character_
  x
}
.find_first_column <- function(data, candidates) {
  if (is.null(data) || !length(names(data))) {
    return(NA_character_)
  }
  exact <- intersect(candidates, names(data))
  if (length(exact) > 0L) {
    return(exact[[1L]])
  }
  data_key <- ana_normalize_name(names(data))
  candidate_key <- ana_normalize_name(candidates)
  match_pos <- match(candidate_key, data_key)
  match_pos <- match_pos[!is.na(match_pos)]
  if (length(match_pos) == 0L) {
    return(NA_character_)
  }
  names(data)[[match_pos[[1L]]]]
}
.extract_daily_columns <- function(data, variable_prefix, status = FALSE) {
  suffix <- if (status) "_?Status" else ""
  pattern <- paste0("^", variable_prefix, "_?([0-9]{2})", suffix, "$")
  hit <- grepl(pattern, names(data), ignore.case = TRUE, perl = TRUE)
  data.frame(
    column = names(data)[hit],
    day = as.integer(sub(pattern, "\\1", names(data)[hit], ignore.case = TRUE, perl = TRUE)),
    stringsAsFactors = FALSE
  )
}
.find_daily_status_column <- function(data, variable_prefix, day) {
  day <- sprintf("%02d", as.integer(day))
  .find_first_column(
    data,
    c(
      paste0(variable_prefix, day, "Status"),
      paste0(variable_prefix, "_", day, "Status"),
      paste0(variable_prefix, day, "_Status"),
      paste0(variable_prefix, "_", day, "_Status")
    )
  )
}
.standardize_ana_daily_variable <- function(data, variable_prefix, variable_name, unit, source_label) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    return(ana_empty_daily_series())
  }
  value_cols <- .extract_daily_columns(data, variable_prefix, status = FALSE)
  if (nrow(value_cols) == 0L) {
    return(ana_empty_daily_series())
  }
  station_col <- .find_first_column(data, c("station_code", "EstacaoCodigo", "codigoestacao", "CodigoEstacao", "CodigoDaEstacao", "CodEstacao"))
  date_col <- .find_first_column(data, c("date", "Data", "DataHora", "Data_Hora_Dado", "DataHoraDado"))
  consistency_col <- .find_first_column(data, c("consistency_level", "NivelConsistencia", "Nivel_Consistencia", "Consistencia"))
  if (is.na(station_col)) {
    stop("Nao foi possivel identificar o codigo da estacao nos dados.", call. = FALSE)
  }
  if (is.na(date_col)) {
    stop("Nao foi possivel identificar a coluna de data nos dados.", call. = FALSE)
  }
  month_date <- .parse_ana_date(data[[date_col]])
  station_code <- ana_normalize_station_code(data[[station_col]])
  consistency_level <- if (!is.na(consistency_col)) as.character(data[[consistency_col]]) else rep(NA_character_, nrow(data))
  pieces <- vector("list", nrow(value_cols))
  for (i in seq_len(nrow(value_cols))) {
    day_value <- value_cols$day[[i]]
    value_col <- value_cols$column[[i]]
    status_col <- .find_daily_status_column(data, variable_prefix, day_value)
    source_status <- if (!is.na(status_col)) as.character(data[[status_col]]) else rep(NA_character_, nrow(data))
    date_value <- .make_ana_month_day_date(month_date, day_value)
    keep <- !is.na(date_value)
    pieces[[i]] <- data.frame(
      station_code = station_code[keep],
      date = date_value[keep],
      variable = variable_name,
      value = .parse_ana_numeric(data[[value_col]])[keep],
      unit = unit,
      consistency_level = consistency_level[keep],
      source_status = source_status[keep],
      source = source_label,
      stringsAsFactors = FALSE
    )
  }
  ana_keep_daily_contract(do.call(rbind, pieces))
}
.standardize_ana_daily_table <- function(data, source_label, merge_consistency = TRUE) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    return(ana_empty_daily_series())
  }
  if (all(c("station_code", "date", "variable", "value") %in% names(data))) {
    out <- ana_keep_daily_contract(data)
  } else {
    out <- dplyr::bind_rows(
      .standardize_ana_daily_variable(data, "Vazao", "discharge", "m3/s", source_label),
      .standardize_ana_daily_variable(data, "Cota", "stage", "cm", source_label),
      .standardize_ana_daily_variable(data, "Chuva", "rainfall", "mm", source_label)
    )
  }
  if (merge_consistency) {
    out <- ana_prefer_daily_rows(out)
  } else {
    out <- ana_keep_daily_contract(out)
  }
  out[order(out$station_code, out$date, out$variable), , drop = FALSE]
}
.merge_daily_consistency <- function(data) {
  ana_prefer_daily_rows(data)
}
hydrostat_standardize_ana_daily_table <- function(data, source_label = NA_character_, merge_consistency = TRUE, ...) {
  .standardize_ana_daily_table(data, source_label = source_label, merge_consistency = merge_consistency)
}
.match_daily_variables <- function(variables) {
  allowed <- c("all", "discharge", "stage", "rainfall")
  if (missing(variables) || is.null(variables) || length(variables) == 0L || "all" %in% variables) {
    return(c("discharge", "stage", "rainfall"))
  }
  variables <- as.character(variables)
  invalid <- setdiff(variables, allowed)
  if (length(invalid) > 0L) {
    stop(
      "Variavel diaria invalida: ",
      paste(invalid, collapse = ", "),
      ". Use 'all', 'discharge', 'stage' ou 'rainfall'.",
      call. = FALSE
    )
  }
  unique(variables)
}
.filter_daily_variables <- function(data, variables) {
  variables <- .match_daily_variables(variables)
  data[data$variable %in% variables, , drop = FALSE]
}
.require_daily_data <- function(data, source_label) {
  if (nrow(data) == 0L) {
    stop(
      "O arquivo foi lido, mas nao contem colunas diarias reconhecidas para a selecao solicitada: ",
      source_label,
      ".",
      call. = FALSE
    )
  }
  data
}
ana_standardize_api_daily_items <- function(items = NULL,
                                            station_code = NULL,
                                            product = NULL,
                                            variable = NULL,
                                            unit = NULL,
                                            source = "ANA API",
                                            ...) {
  daily_products <- c("daily_discharge", "daily_stage", "daily_rainfall")
  if (length(station_code) == 1L && !is.na(station_code) && station_code %in% daily_products &&
      !(length(product) == 1L && !is.na(product) && product %in% daily_products)) {
    tmp <- product
    product <- station_code
    station_code <- tmp
  }
  if (is.null(variable) || length(variable) == 0L || is.na(variable) || !nzchar(variable)) {
    variable <- switch(
      as.character(product)[1],
      daily_stage = "stage",
      daily_rainfall = "rainfall",
      "discharge"
    )
  }
  unit <- if (is.null(unit) || length(unit) == 0L || is.na(unit) || !nzchar(unit)) {
    switch(variable, stage = "cm", rainfall = "mm", "m3/s")
  } else {
    as.character(unit)[1]
  }
  dots <- list(...)
  if (is.null(items)) {
    for (nm in c("data", "x", "content", "json", "records")) {
      if (!is.null(dots[[nm]])) {
        items <- dots[[nm]]
        break
      }
    }
  }
  data <- ana_daily_items_to_data_frame(items)
  if (nrow(data) == 0L) {
    return(ana_empty_daily_series())
  }
  if (!is.null(station_code) && length(station_code) == 1L && !is.na(station_code) && nzchar(station_code)) {
    data$station_code <- station_code
  }
  wide <- .standardize_ana_daily_table(data, source_label = source, merge_consistency = TRUE)
  wide <- wide[wide$variable == variable, , drop = FALSE]
  if (nrow(wide) > 0L) {
    return(wide)
  }
  date_col <- .find_first_column(data, c("date", "Data", "DataMedicao", "Data_Medicao", "DataHora", "DataHoraMedicao", "Data_Hora_Medicao", "Dia"))
  station_col <- .find_first_column(data, c("station_code", "CodEstacao", "CodigoEstacao", "codigo_estacao", "EstacaoCodigo", "Codigo"))
  consistency_col <- .find_first_column(data, c("consistency_level", "NivelConsistencia", "nivel_consistencia", "Consistencia", "Nivel"))
  status_col <- .find_first_column(data, c("source_status", "status", "Status"))
  value_col <- .find_first_column(data, ana_daily_value_candidates(variable))
  if (is.na(date_col) || is.na(value_col)) {
    return(ana_empty_daily_series())
  }
  station_value <- if (!is.na(station_col)) {
    data[[station_col]]
  } else if (!is.null(station_code) && length(station_code) > 0L) {
    rep(as.character(station_code)[1], nrow(data))
  } else {
    rep(NA_character_, nrow(data))
  }
  out <- data.frame(
    station_code = station_value,
    date = .parse_ana_date(data[[date_col]]),
    variable = variable,
    value = .parse_ana_numeric(data[[value_col]]),
    unit = unit,
    consistency_level = if (!is.na(consistency_col)) as.character(data[[consistency_col]]) else NA_character_,
    source_status = if (!is.na(status_col)) as.character(data[[status_col]]) else "success",
    source = source,
    stringsAsFactors = FALSE
  )
  ana_prefer_daily_rows(out)
}
ana_standardize_daily_api_items <- ana_standardize_api_daily_items
ana_all_standardize_api_daily <- function(items = NULL,
                                          product = NULL,
                                          station_code = NULL,
                                          variable = NULL,
                                          unit = NULL,
                                          source = "ANA API",
                                          ...) {
  ana_standardize_api_daily_items(
    items = items,
    station_code = station_code,
    product = product,
    variable = variable,
    unit = unit,
    source = source,
    ...
  )
}
ana_daily_items_to_data_frame <- function(items) {
  if (is.null(items)) {
    return(data.frame())
  }
  if (is.data.frame(items)) {
    return(as.data.frame(items, stringsAsFactors = FALSE))
  }
  if (is.list(items) && !is.null(items$items)) {
    return(ana_daily_items_to_data_frame(items$items))
  }
  if (is.list(items) && !is.null(items$data)) {
    return(ana_daily_items_to_data_frame(items$data))
  }
  if (!is.list(items) || length(items) == 0L) {
    return(data.frame())
  }
  if (length(items) == 1L && is.data.frame(items[[1L]])) {
    return(as.data.frame(items[[1L]], stringsAsFactors = FALSE))
  }
  if (!is.list(items[[1L]])) {
    return(data.frame())
  }
  names_all <- unique(unlist(lapply(items, names), use.names = FALSE))
  if (!length(names_all)) {
    return(data.frame())
  }
  rows <- lapply(items, function(x) {
    row <- as.list(rep(NA, length(names_all)))
    names(row) <- names_all
    row[names(x)] <- x
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
ana_daily_value_candidates <- function(variable) {
  switch(
    variable,
    stage = c("Cota", "cota", "stage", "ValorCota", "valor_cota", "value", "Valor"),
    rainfall = c("Chuva", "chuva", "rainfall", "Precipitacao", "precipitacao", "ValorChuva", "value", "Valor"),
    c("Vazao", "vazao", "discharge", "ValorVazao", "valor_vazao", "value", "Valor")
  )
}
