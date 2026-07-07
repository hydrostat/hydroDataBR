# hydroDataBR specialized hydrometric product acquisition
# Endpoint for discharge-measurement summaries.
ana_discharge_measurements_endpoint <- function() {
  "/HidroSerieResumoDescarga/v1"
}
# Endpoint for rating curves.
ana_rating_curves_endpoint <- function() {
  "/HidroSerieCurvaDescarga/v1"
}
# Endpoint for cross-section profiles.
ana_cross_sections_endpoint <- function() {
  "/HidroSeriePerfilTransversal/v1"
}
ana_stage04_station_param <- function() {
  paste0("C", intToUtf8(0x00F3), "digo da Esta", intToUtf8(0x00E7), intToUtf8(0x00E3), "o")
}
ana_stage04_is_empty <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")
}
ana_stage04_compact_query <- function(query) {
  query[!vapply(query, ana_stage04_is_empty, logical(1))]
}
ana_stage04_format_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- as.Date(x)[1L]
  if (is.na(x)) return(NULL)
  format(x, "%Y-%m-%d")
}
ana_stage04_product_query <- function(station_code,
                                      start_date = NULL,
                                      end_date = NULL,
                                      consistency_level = NULL,
                                      product = NULL) {
  if (ana_stage04_is_empty(station_code)) stop("station_code must be supplied.", call. = FALSE)
  query <- list()
  query[[ana_stage04_station_param()]] <- as.character(station_code)[1L]
  if (!ana_stage04_is_empty(start_date)) query[[ana_stage04_start_param()]] <- format(as.Date(start_date), "%Y-%m-%d")
  if (!ana_stage04_is_empty(end_date)) query[[ana_stage04_end_param()]] <- format(as.Date(end_date), "%Y-%m-%d")
  if (!is.null(product) && product %in% c("discharge_measurements", "cross_sections")) {
    query[[ana_stage04_type_filter_param()]] <- "DATA_LEITURA"
  }
  ana_stage04_compact_query(query)
}
ana_stage04_request_product <- function(endpoint,
                                        token,
                                        station_code,
                                        start_date = NULL,
                                        end_date = NULL,
                                        consistency_level = NULL,
                                        product = NULL,
                                        timeout = 120) {
  if (is.null(token)) {
    if (!ana_has_auth_credentials()) stop("ANA credentials are required for specialized authenticated routes.", call. = FALSE)
    token <- ana_authenticate(timeout = timeout)
  }
  query <- ana_stage04_product_query(station_code = station_code, start_date = start_date, end_date = end_date, consistency_level = consistency_level, product = product)
  ana_call_request_json(endpoint = endpoint, query = query, token = token)
}
ana_stage04_token_or_authenticate <- function(token = NULL,
                                              identifier = NULL,
                                              password = NULL,
                                              identificador = NULL,
                                              senha = NULL,
                                              cpf_cnpj = NULL,
                                              max_attempts = 3,
                                              retry_sleep_seconds = 1) {
  if (!is.null(token)) return(token)
  ana_authenticate(
    identifier = identifier,
    password = password,
    identificador = identificador,
    senha = senha,
    cpf_cnpj = cpf_cnpj,
    max_attempts = max_attempts,
    retry_sleep_seconds = retry_sleep_seconds
  )
}
ana_stage04_record_list <- function(record, candidates) {
  if (is.null(record) || length(record) == 0) {
    return(NULL)
  }
  record_names <- names(record)
  if (is.null(record_names)) {
    return(NULL)
  }
  exact <- match(candidates, record_names, nomatch = 0L)
  exact <- exact[exact > 0L]
  if (length(exact) > 0) {
    return(record[[exact[[1]]]])
  }
  lower_match <- match(tolower(candidates), tolower(record_names), nomatch = 0L)
  lower_match <- lower_match[lower_match > 0L]
  if (length(lower_match) > 0) {
    return(record[[lower_match[[1]]]])
  }
  NULL
}
ana_stage04_parent_scalars <- function(parent) {
  if (is.null(parent) || length(parent) == 0) {
    return(list())
  }
  keep <- vapply(
    parent,
    function(x) {
      !is.list(x) && !is.data.frame(x) && length(x) <= 1
    },
    logical(1)
  )
  parent[keep]
}
ana_stage04_merge_parent_child <- function(parent, child) {
  parent_scalars <- ana_stage04_parent_scalars(parent)
  missing_names <- setdiff(names(parent_scalars), names(child))
  c(child, parent_scalars[missing_names])
}
ana_stage04_child_records <- function(response, child_candidates) {
  parents <- ana_items_to_records(ana_extract_items(response))
  out <- list()
  for (parent in parents) {
    children <- ana_stage04_record_list(parent, child_candidates)
    if (!is.null(children) && length(children) > 0) {
      child_records <- ana_items_to_records(children)
      child_records <- lapply(
        child_records,
        function(child) ana_stage04_merge_parent_child(parent, child)
      )
      out <- c(out, child_records)
    } else {
      out <- c(out, list(parent))
    }
  }
  out
}
ana_stage04_normalize_field_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub(intToUtf8(0x00B2), "2", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x00B3), "3", x, fixed = TRUE)
  x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))
  x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
  x_ascii <- tolower(x_ascii)
  gsub("[^a-z0-9]+", "", x_ascii)
}
ana_stage04_record_value_normalized <- function(record, candidates) {
  if (is.null(record) || length(record) == 0) {
    return(NA_character_)
  }
  record_names <- names(record)
  if (is.null(record_names)) {
    return(NA_character_)
  }
  normalized_names <- ana_stage04_normalize_field_name(record_names)
  normalized_candidates <- ana_stage04_normalize_field_name(candidates)
  idx <- match(normalized_candidates, normalized_names, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) == 0L) {
    return(NA_character_)
  }
  ana_scalar_character(record[[idx[[1L]]]])
}
ana_stage04_record_value <- function(record, candidates) {
  value <- ana_record_value(record, candidates)
  if (!is.na(value)) {
    return(value)
  }
  value <- ana_stage04_record_value_normalized(record, candidates)
  if (!is.na(value)) {
    return(value)
  }
  record_names <- names(record)
  if (is.null(record_names)) {
    return(NA_character_)
  }
  for (nm in record_names) {
    child <- record[[nm]]
    if (is.list(child) && !is.data.frame(child)) {
      value <- ana_record_value(child, candidates)
      if (!is.na(value)) {
        return(value)
      }
      value <- ana_stage04_record_value_normalized(child, candidates)
      if (!is.na(value)) {
        return(value)
      }
    }
  }
  NA_character_
}
ana_stage04_record_column <- function(records, candidates) {
  vapply(
    records,
    ana_stage04_record_value,
    candidates = candidates,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
ana_stage04_as_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- as.Date(rep(NA_character_, length(x)))
  is_iso <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  out[is_iso] <- as.Date(substr(x[is_iso], 1, 10))
  is_br <- is.na(out) & !is.na(x) & grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  out[is_br] <- as.Date(substr(x[is_br], 1, 10), format = "%d/%m/%Y")
  out
}
ana_stage04_as_datetime <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  formats <- c(
    "%Y-%m-%dT%H:%M:%S%z",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%d",
    "%d/%m/%Y"
  )
  for (i in seq_along(x)) {
    if (is.na(x[[i]])) {
      next
    }
    value <- sub("\\.\\d{1,6}", "", x[[i]])
    for (fmt in formats) {
      parsed <- suppressWarnings(strptime(value, format = fmt, tz = "UTC"))
      if (!is.na(parsed)) {
        out[[i]] <- as.POSIXct(parsed, tz = "UTC")
        break
      }
    }
  }
  out
}
ana_stage04_int <- function(x) {
  suppressWarnings(as.integer(ana_as_number(x)))
}
ana_empty_discharge_measurements <- function() {
  tibble::tibble(
    station_code = character(),
    measurement_datetime = as.POSIXct(character(), tz = "UTC"),
    consistency_level = character(),
    stage_cm = numeric(),
    discharge_m3s = numeric(),
    wetted_area_m2 = numeric(),
    width_m = numeric(),
    mean_depth_m = numeric(),
    mean_velocity_ms = numeric(),
    source = character()
  )
}
ana_empty_rating_curves <- function() {
  tibble::tibble(
    rating_curve_id = character(),
    rating_curve_segment_id = character(),
    station_code = character(),
    valid_from = as.Date(character()),
    valid_to = as.Date(character()),
    consistency_level = character(),
    segment_number = integer(),
    n_segments_reported = integer(),
    curve_type = character(),
    equation_type = character(),
    stage_min_cm = numeric(),
    stage_max_cm = numeric(),
    table_stage_step_cm = numeric(),
    coefficient_a = numeric(),
    coefficient_h0_cm = numeric(),
    coefficient_n = numeric(),
    source = character()
  )
}
ana_empty_cross_sections <- function() {
  list(
    sections = tibble::tibble(
      cross_section_id = character(),
      station_code = character(),
      measurement_datetime = as.POSIXct(character(), tz = "UTC"),
      consistency_level = character(),
      survey_number = integer(),
      section_type = character(),
      source_record_id = character(),
      n_vertices = integer(),
      observation = character(),
      source = character()
    ),
    vertices = tibble::tibble(
      cross_section_id = character(),
      cross_section_vertex_id = character(),
      station_code = character(),
      measurement_datetime = as.POSIXct(character(), tz = "UTC"),
      consistency_level = character(),
      survey_number = integer(),
      section_type = character(),
      vertex_order = integer(),
      vertex_distance_m = numeric(),
      vertex_stage_cm = numeric(),
      source = character()
    )
  )
}
ana_standardize_discharge_measurements <- function(response) {
  records <- ana_stage04_child_records(
    response,
    c(
      "medicoes",
      "Medicoes",
      "resumoDescarga",
      "ResumoDescarga",
      "serieResumoDescarga",
      "SerieResumoDescarga"
    )
  )
  if (length(records) == 0) {
    return(ana_empty_discharge_measurements())
  }
  discharge_name_accent <- paste0("Vaz", intToUtf8(0x00E3), "o")
  discharge_unit_accent <- paste0(discharge_name_accent, " (m3/s)")
  area_name_accent <- paste0(intToUtf8(0x00C1), "rea")
  area_unit_sup2 <- paste0("m", intToUtf8(0x00B2))
  tibble::tibble(
    station_code = ana_stage04_record_column(
      records,
      c(
        "codigoestacao",
        "CodigoEstacao",
        "Codigo_Estacao",
        "CodEstacao",
        "station_code"
      )
    ),
    measurement_datetime = ana_stage04_as_datetime(ana_stage04_record_column(
      records,
      c(
        "Data_Hora_Dado",
        "DataHoraDado",
        "Data_Hora_Medicao",
        "DataHoraMedicao",
        "DataMedicao",
        "Data_Medicao",
        "Data",
        "measurement_datetime"
      )
    )),
    consistency_level = ana_stage04_record_column(
      records,
      c(
        "Nivel_Consistencia",
        "NivelConsistencia",
        "Consistencia",
        "consistency_level"
      )
    ),
    stage_cm = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Cota",
        "Cota (cm)",
        "Cota_cm",
        "CotaMedida",
        "Nivel",
        "stage_cm"
      )
    )),
    discharge_m3s = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Vazao",
        discharge_name_accent,
        "Vazao (m3/s)",
        discharge_unit_accent,
        "Vazao_m3s",
        "Vazao_m3_s",
        "VazaoMedida",
        "DescargaLiquida",
        "discharge_m3s"
      )
    )),
    wetted_area_m2 = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Area_Molhada",
        "AreaMolhada",
        "Area Molhada",
        "Area_Molhada (m2)",
        "Area Molhada (m2)",
        "Area_Molhada_m2",
        paste0("Area_Molhada (", area_unit_sup2, ")"),
        paste0("Area Molhada (", area_unit_sup2, ")"),
        area_name_accent,
        paste0(area_name_accent, "_Molhada"),
        paste0(area_name_accent, " Molhada"),
        paste0(area_name_accent, "_Molhada (m2)"),
        paste0(area_name_accent, " Molhada (m2)"),
        paste0(area_name_accent, "_Molhada (", area_unit_sup2, ")"),
        paste0(area_name_accent, " Molhada (", area_unit_sup2, ")"),
        "AreaSecao",
        "Area_Secao",
        "Area Secao",
        "AreaDaSecao",
        "Area_Da_Secao",
        "AreaMolhadaMedida",
        "Area_Molhada_Medida",
        "Area",
        "wetted_area_m2"
      )
    )),
    width_m = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Largura",
        "Largura (m)",
        "Largura_m",
        "width_m"
      )
    )),
    mean_depth_m = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Profundidade",
        "Profundidade (m)",
        "Profundidade_m",
        "ProfundidadeMedia",
        "Profundidade_Media",
        "ProfMedia",
        "mean_depth_m"
      )
    )),
    mean_velocity_ms = ana_as_number(ana_stage04_record_column(
      records,
      c(
        "Vel_Media",
        "VelMedia",
        "Vel_Media (m/s)",
        "Vel_Media_ms",
        "VelocidadeMedia",
        "Velocidade_Media",
        "mean_velocity_ms"
      )
    )),
    source = "ana_hidrowebservice_discharge_measurements"
  )
}
ana_standardize_rating_curves <- function(response) {
  records <- ana_stage04_child_records(
    response,
    c(
      "segmentos",
      "Segmentos",
      "trechos",
      "Trechos",
      "curvas",
      "Curvas",
      "curvaDescarga",
      "CurvaDescarga",
      "serieCurvaDescarga",
      "SerieCurvaDescarga"
    )
  )
  if (length(records) == 0) {
    return(ana_empty_rating_curves())
  }
  station_code <- ana_stage04_record_column(
    records,
    c("CodigoEstacao", "Codigo_Estacao", "codigoestacao", "CodEstacao", "station_code")
  )
  valid_from <- ana_stage04_as_date(ana_stage04_record_column(
    records,
    c("DataInicio", "Data_Inicio", "InicioValidade", "DataValidadeInicial", "valid_from")
  ))
  valid_to <- ana_stage04_as_date(ana_stage04_record_column(
    records,
    c("DataFim", "Data_Fim", "FimValidade", "DataValidadeFinal", "valid_to")
  ))
  segment_number <- ana_stage04_int(ana_stage04_record_column(
    records,
    c("NumeroTrecho", "Numero_Trecho", "Trecho", "Segmento", "segment_number")
  ))
  rating_curve_id <- ana_stage04_record_column(
    records,
    c("CodigoCurva", "Codigo_Curva", "IdCurva", "IdentificadorCurva", "rating_curve_id")
  )
  rating_curve_segment_id <- ana_stage04_record_column(
    records,
    c(
      "CodigoTrecho",
      "Codigo_Trecho",
      "IdTrecho",
      "IdentificadorTrecho",
      "rating_curve_segment_id"
    )
  )
  missing_curve_id <- is.na(rating_curve_id) | rating_curve_id == ""
  rating_curve_id[missing_curve_id] <- paste(
    station_code[missing_curve_id],
    ifelse(is.na(valid_from[missing_curve_id]), "na", as.character(valid_from[missing_curve_id])),
    sep = "_"
  )
  missing_segment_id <- is.na(rating_curve_segment_id) | rating_curve_segment_id == ""
  rating_curve_segment_id[missing_segment_id] <- paste(
    rating_curve_id[missing_segment_id],
    ifelse(is.na(segment_number[missing_segment_id]), seq_along(segment_number[missing_segment_id]), segment_number[missing_segment_id]),
    sep = "_segment_"
  )
  tibble::tibble(
    rating_curve_id = rating_curve_id,
    rating_curve_segment_id = rating_curve_segment_id,
    station_code = station_code,
    valid_from = valid_from,
    valid_to = valid_to,
    consistency_level = ana_stage04_record_column(
      records,
      c("NivelConsistencia", "Nivel_Consistencia", "Consistencia", "consistency_level")
    ),
    segment_number = segment_number,
    n_segments_reported = ana_stage04_int(ana_stage04_record_column(
      records,
      c("NumeroTotalTrechos", "Numero_Trechos", "TotalTrechos", "n_segments_reported")
    )),
    curve_type = ana_stage04_record_column(
      records,
      c("TipoCurva", "Tipo_Curva", "curve_type")
    ),
    equation_type = ana_stage04_record_column(
      records,
      c("TipoEquacao", "Tipo_Equacao", "Equacao", "equation_type")
    ),
    stage_min_cm = ana_as_number(ana_stage04_record_column(
      records,
      c("CotaMinima", "Cota_Minima", "CotaInicial", "stage_min_cm")
    )),
    stage_max_cm = ana_as_number(ana_stage04_record_column(
      records,
      c("CotaMaxima", "Cota_Maxima", "CotaFinal", "stage_max_cm")
    )),
    table_stage_step_cm = ana_as_number(ana_stage04_record_column(
      records,
      c("IntervaloCota", "Intervalo_Cota", "PassoCota", "table_stage_step_cm")
    )),
    coefficient_a = ana_as_number(ana_stage04_record_column(
      records,
      c("CoeficienteA", "Coeficiente_A", "CoefA", "A", "coefficient_a")
    )),
    coefficient_h0_cm = ana_as_number(ana_stage04_record_column(
      records,
      c("CoeficienteH0", "Coeficiente_H0", "CoefH0", "H0", "h0", "coefficient_h0_cm", "coefficient_h0_m")
    )),
    coefficient_n = ana_as_number(ana_stage04_record_column(
      records,
      c("CoeficienteN", "Coeficiente_N", "CoefN", "N", "n", "coefficient_n")
    )),
    source = "ana_hidrowebservice_rating_curves"
  )
}
ana_stage04_cross_section_child <- function(record) {
  ana_stage04_record_list(
    record,
    c(
      "vertices",
      "Vertices",
      "pontos",
      "Pontos",
      "perfil",
      "Perfil",
      "perfilTransversal",
      "PerfilTransversal",
      "pontosPerfilTransversal",
      "PontosPerfilTransversal"
    )
  )
}
# Stage 07E cross-section helpers begin
ana_cross_sections_empty_standard <- function() {
  sections <- data.frame(
    section_id = character(),
    station_code = character(),
    measurement_date = as.Date(character()),
    measurement_datetime = character(),
    consistency_level = integer(),
    survey_number = character(),
    section_type = character(),
    n_vertices = integer(),
    n_vertices_reported = integer(),
    distance_pipf_m = numeric(),
    x_min_m = numeric(),
    x_max_m = numeric(),
    y_min_cm = numeric(),
    y_max_cm = numeric(),
    geometry_step_cm = numeric(),
    notes = character(),
    source_record_id = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  vertices <- data.frame(
    section_id = character(),
    station_code = character(),
    measurement_date = as.Date(character()),
    measurement_datetime = character(),
    consistency_level = integer(),
    survey_number = character(),
    section_type = character(),
    source_record_id = character(),
    vertex_index = integer(),
    distance_m = numeric(),
    stage_cm = numeric(),
    source = character(),
    stringsAsFactors = FALSE
  )
  list(sections = sections, vertices = vertices)
}
ana_cross_sections_pick <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[hit[1]]])
}
ana_cross_sections_number <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x <- gsub("\\.", "", x)
  x <- sub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
ana_cross_sections_number_dot <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x <- sub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
ana_cross_sections_integer <- function(x) {
  suppressWarnings(as.integer(ana_cross_sections_number_dot(x)))
}
ana_cross_sections_date <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  out <- rep(as.Date(NA), length(x))
  is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  out[is_iso] <- suppressWarnings(as.Date(substr(x[is_iso], 1, 10)))
  is_br <- grepl("^\\d{2}/\\d{2}/\\d{4}", x)
  out[is_br] <- suppressWarnings(as.Date(x[is_br], format = "%d/%m/%Y"))
  out
}
ana_cross_sections_items_to_data_frame <- function(items) {
  if (is.null(items)) {
    return(data.frame())
  }
  if (is.data.frame(items)) {
    return(items)
  }
  if (!is.list(items) || length(items) == 0) {
    return(data.frame())
  }
  all_names <- unique(unlist(lapply(items, names), use.names = FALSE))
  if (length(all_names) == 0) {
    return(data.frame())
  }
  rows <- lapply(items, function(x) {
    row <- as.list(rep(NA_character_, length(all_names)))
    names(row) <- all_names
    for (nm in names(x)) {
      value <- x[[nm]]
      if (is.null(value) || length(value) == 0) {
        row[[nm]] <- NA_character_
      } else if (length(value) == 1) {
        row[[nm]] <- as.character(value)
      } else {
        row[[nm]] <- paste(as.character(value), collapse = "|")
      }
    }
    row
  })
  out <- do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))
  names(out) <- all_names
  out
}
ana_cross_sections_make_section_id <- function(station_code,
                                               measurement_date,
                                               survey_number,
                                               source_record_id) {
  paste(
    station_code,
    as.character(measurement_date),
    ifelse(is.na(survey_number), "", survey_number),
    ifelse(is.na(source_record_id), "", source_record_id),
    sep = "_"
  )
}
ana_cross_sections_parse_vertical <- function(vertical) {
  if (is.na(vertical) || !nzchar(trimws(vertical))) {
    return(data.frame(distance_m = numeric(), stage_cm = numeric()))
  }
  parts <- unlist(strsplit(as.character(vertical), "\\|", fixed = FALSE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(data.frame(distance_m = numeric(), stage_cm = numeric()))
  }
  rows <- lapply(parts, function(part) {
    comma_positions <- gregexpr(",", part, fixed = TRUE)[[1]]
    if (identical(comma_positions, -1L)) {
      return(NULL)
    }
    last_comma <- comma_positions[length(comma_positions)]
    distance_text <- substr(part, 1, last_comma - 1)
    stage_text <- substr(part, last_comma + 1, nchar(part))
    if (!nzchar(distance_text)) {
      distance_text <- "0"
    }
    data.frame(
      distance_m = ana_cross_sections_number(distance_text),
      stage_cm = ana_cross_sections_number(stage_text),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(distance_m = numeric(), stage_cm = numeric()))
  }
  do.call(rbind, rows)
}
ana_build_cross_sections_query <- function(station_code,
                                           start_date,
                                           end_date,
                                           consistency_level = NULL) {
  if (missing(station_code) || is.null(station_code) || length(station_code) != 1) {
    stop("`station_code` must be a single station code.", call. = FALSE)
  }
  if (is.null(start_date) || is.null(end_date)) {
    stop("`start_date` and `end_date` are required for cross-section requests.", call. = FALSE)
  }
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date)) {
    stop("`start_date` and `end_date` must be valid dates.", call. = FALSE)
  }
  if (end_date < start_date) {
    stop("`end_date` must be greater than or equal to `start_date`.", call. = FALSE)
  }
  if (as.integer(end_date - start_date) + 1L > 366L) {
    stop("Cross-section requests must cover at most 366 days.", call. = FALSE)
  }
  query <- list(
    "C\u00f3digo da Esta\u00e7\u00e3o" = as.character(station_code),
    "Tipo Filtro Data" = "DATA_LEITURA",
    "Data Inicial (yyyy-MM-dd)" = format(start_date, "%Y-%m-%d"),
    "Data Final (yyyy-MM-dd)" = format(end_date, "%Y-%m-%d")
  )
  query
}
ana_cross_sections_request_json <- function(token, endpoint, query) {
  arg_names <- names(formals(ana_request_json))
  args <- list()
  if ("token" %in% arg_names) {
    args$token <- token
  }
  if ("endpoint" %in% arg_names) {
    args$endpoint <- endpoint
  }
  if ("query" %in% arg_names) {
    args$query <- query
  } else if ("query_params" %in% arg_names) {
    args$query_params <- query
  } else if ("params" %in% arg_names) {
    args$params <- query
  } else {
    args$query <- query
  }
  do.call(ana_request_json, args)
}
ana_standardize_cross_sections <- function(response) {
  empty <- ana_cross_sections_empty_standard()
  items <- if (is.list(response) && !is.null(response$items)) response$items else response
  raw <- ana_cross_sections_items_to_data_frame(items)
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    return(empty)
  }
  station_code <- ana_cross_sections_pick(
    raw,
    c("codigoestacao", "CodigoEstacao", "EstacaoCodigo", "station_code")
  )
  measurement_datetime <- ana_cross_sections_pick(
    raw,
    c("Data_Hora_Medicao", "DataHoraMedicao", "measurement_datetime", "Hora")
  )
  date_from_datetime <- ana_cross_sections_date(measurement_datetime)
  date_from_date <- ana_cross_sections_date(
    ana_cross_sections_pick(raw, c("Data", "measurement_date"))
  )
  measurement_date <- date_from_datetime
  measurement_date[is.na(measurement_date)] <- date_from_date[is.na(measurement_date)]
  consistency_level <- ana_cross_sections_integer(
    ana_cross_sections_pick(raw, c("Nivel_Consistencia", "NivelConsistencia", "consistency_level"))
  )
  survey_number <- ana_cross_sections_pick(
    raw,
    c("Num_Levantamento", "NumLevantamento", "NumeroLevantamento", "survey_number")
  )
  section_type <- ana_cross_sections_pick(
    raw,
    c("Tipo_Secao", "TipoSecao", "TipoSecaoTransversal", "section_type")
  )
  source_record_id <- ana_cross_sections_pick(
    raw,
    c("Registro_ID", "CodigoPerfil", "Codigo_Perfil", "IdPerfil", "source_record_id")
  )
  if (all(is.na(source_record_id))) {
    source_record_id <- survey_number
  }
  section_id <- ana_cross_sections_make_section_id(
    station_code,
    measurement_date,
    survey_number,
    source_record_id
  )
  vertical_column <- ana_cross_sections_pick(raw, c("Vertical"))
  has_hidroweb_vertical <- any(!is.na(vertical_column) & nzchar(trimws(vertical_column)))
  number_fun <- if (has_hidroweb_vertical) ana_cross_sections_number else ana_cross_sections_number_dot
  distance_pipf_m <- number_fun(
    ana_cross_sections_pick(raw, c("Distancia_pipf", "DistanciaPIPF", "distance_pipf_m"))
  )
  x_max_m <- number_fun(
    ana_cross_sections_pick(raw, c("Eixo_X_Dist_Maxima", "EixoXDistMaxima", "x_max_m"))
  )
  x_min_m <- number_fun(
    ana_cross_sections_pick(raw, c("Eixo_X_Dist_Minima", "EixoXDistMinima", "x_min_m"))
  )
  y_max_cm <- number_fun(
    ana_cross_sections_pick(raw, c("Eixo_Y_Cota_Maxima", "EixoYCotaMaxima", "y_max_cm"))
  )
  y_min_cm <- number_fun(
    ana_cross_sections_pick(raw, c("Eixo_Y_Cota_Minima", "EixoYCotaMinima", "y_min_cm"))
  )
  geometry_step_cm <- number_fun(
    ana_cross_sections_pick(raw, c("Elm_Geom_Passo_Cota", "ElmGeomPassoCota", "geometry_step_cm"))
  )
  n_vertices_reported <- ana_cross_sections_integer(
    ana_cross_sections_pick(raw, c("Num_Verticais", "NumVerticais", "n_vertices_reported"))
  )
  notes <- ana_cross_sections_pick(raw, c("Observacoes", "notes"))
  if (has_hidroweb_vertical) {
    vertices_list <- vector("list", nrow(raw))
    for (i in seq_len(nrow(raw))) {
      parsed <- ana_cross_sections_parse_vertical(vertical_column[i])
      if (nrow(parsed) == 0) {
        vertices_list[[i]] <- NULL
      } else {
        vertices_list[[i]] <- data.frame(
          section_id = section_id[i],
          station_code = station_code[i],
          measurement_date = measurement_date[i],
          measurement_datetime = measurement_datetime[i],
          consistency_level = consistency_level[i],
          survey_number = survey_number[i],
          section_type = section_type[i],
          source_record_id = source_record_id[i],
          vertex_index = seq_len(nrow(parsed)),
          distance_m = parsed$distance_m,
          stage_cm = parsed$stage_cm,
          source = "hidroweb_cross_sections",
          stringsAsFactors = FALSE
        )
      }
    }
    vertices_list <- vertices_list[!vapply(vertices_list, is.null, logical(1))]
    vertices <- if (length(vertices_list) == 0) empty$vertices else do.call(rbind, vertices_list)
  } else {
    vertices <- data.frame(
      section_id = section_id,
      station_code = station_code,
      measurement_date = measurement_date,
      measurement_datetime = measurement_datetime,
      consistency_level = consistency_level,
      survey_number = survey_number,
      section_type = section_type,
      source_record_id = source_record_id,
      vertex_index = ave(seq_along(section_id), section_id, FUN = seq_along),
      distance_m = ana_cross_sections_number_dot(
        ana_cross_sections_pick(raw, c("Distancia", "distance_m"))
      ),
      stage_cm = ana_cross_sections_number_dot(
        ana_cross_sections_pick(raw, c("Cota", "stage_cm"))
      ),
      source = "ana_hidrowebservice_cross_sections",
      stringsAsFactors = FALSE
    )
  }
  section_keys <- !duplicated(section_id)
  sections <- data.frame(
    section_id = section_id[section_keys],
    station_code = station_code[section_keys],
    measurement_date = measurement_date[section_keys],
    measurement_datetime = measurement_datetime[section_keys],
    consistency_level = consistency_level[section_keys],
    survey_number = survey_number[section_keys],
    section_type = section_type[section_keys],
    n_vertices = as.integer(tabulate(match(vertices$section_id, unique(section_id[section_keys])), nbins = sum(section_keys))),
    n_vertices_reported = n_vertices_reported[section_keys],
    distance_pipf_m = distance_pipf_m[section_keys],
    x_min_m = x_min_m[section_keys],
    x_max_m = x_max_m[section_keys],
    y_min_cm = y_min_cm[section_keys],
    y_max_cm = y_max_cm[section_keys],
    geometry_step_cm = geometry_step_cm[section_keys],
    notes = notes[section_keys],
    source_record_id = source_record_id[section_keys],
    source = if (has_hidroweb_vertical) "hidroweb_cross_sections" else "ana_hidrowebservice_cross_sections",
    stringsAsFactors = FALSE
  )
  sections <- sections[order(sections$station_code, sections$measurement_date, sections$survey_number), , drop = FALSE]
  vertices <- vertices[order(vertices$section_id, vertices$vertex_index), , drop = FALSE]
  row.names(sections) <- NULL
  row.names(vertices) <- NULL
  list(sections = sections, vertices = vertices)
}
# Stage 07E cross-section helpers end
#' Obter medicoes de descarga liquida da ANA
#'
#' Consulta as medicoes de descarga liquida de uma estacao no HidroWebService
#' da ANA e retorna uma tabela padronizada.
#'
#' @param token Objeto de token retornado por [ana_authenticate()].
#' @param station_code Codigo da estacao. Mantido como texto.
#' @param start_date Data inicial no formato `YYYY-MM-DD`.
#' @param end_date Data final no formato `YYYY-MM-DD`.
#' @param consistency_level Nivel de consistencia usado para filtrar o resultado padronizado, quando aplicavel.
#'
#' @param timeout Tempo maximo da requisicao em segundos.
#' @return Um tibble com medicoes de descarga liquida padronizadas.
#' @keywords internal
.hydrodatabr_original_get_ana_discharge_measurements <- function(token = NULL,
                                           station_code,
                                           start_date = NULL,
                                           end_date = NULL,
                                           consistency_level = NULL,
                                           timeout = 120) {
  response <- ana_stage04_request_product(endpoint = ana_discharge_measurements_endpoint(), token = token, station_code = station_code, start_date = start_date, end_date = end_date, consistency_level = consistency_level, product = "discharge_measurements", timeout = timeout)
  out <- ana_standardize_discharge_measurements(response)
  if (!is.null(consistency_level) && "consistency_level" %in% names(out)) out <- out[out$consistency_level %in% as.character(consistency_level), , drop = FALSE]
  tibble::as_tibble(out)
}
#' Obter curvas-chave da ANA
#'
#' Consulta as curvas-chave de uma estacao no HidroWebService da ANA e retorna
#' uma tabela padronizada em que cada linha representa um trecho da curva.
#'
#' O coeficiente `coefficient_h0_cm` representa o deslocamento da curva-chave
#' e usa a mesma unidade das cotas da curva.
#'
#' @param station_code Código da estação ANA.
#' @param start_date Data inicial no formato `YYYY-MM-DD` ou objeto `Date`.
#' @param end_date Data final no formato `YYYY-MM-DD` ou objeto `Date`.
#' @param ... Argumentos adicionais repassados internamente.
#'
#' @param timeout Tempo maximo da requisicao em segundos.
#' @return Um tibble com trechos de curvas-chave padronizados.
#' @keywords internal
#' @noRd
.hydrodatabr_original_get_ana_rating_curves <- function(token = NULL,
                                  station_code,
                                  start_date = NULL,
                                  end_date = NULL,
                                  consistency_level = NULL,
                                  timeout = 120) {
  response <- ana_stage04_request_product(endpoint = ana_rating_curves_endpoint(), token = token, station_code = station_code, start_date = start_date, end_date = end_date, consistency_level = consistency_level, product = "rating_curves", timeout = timeout)
  out <- ana_standardize_rating_curves(response)
  if (!is.null(consistency_level) && "consistency_level" %in% names(out)) out <- out[out$consistency_level %in% as.character(consistency_level), , drop = FALSE]
  tibble::as_tibble(out)
}
#' Obter perfis transversais de uma estacao ANA
#'
#' Baixa perfis transversais de uma estacao pelo HidroWebService ANA.
#' A resposta e padronizada em duas tabelas: uma tabela de levantamentos
#' de secao transversal e uma tabela de vertices do perfil transversal.
#'
#' @param station_code Código da estação ANA.
#' @param start_date Data inicial no formato `YYYY-MM-DD` ou objeto `Date`.
#' @param end_date Data final no formato `YYYY-MM-DD` ou objeto `Date`.
#' @param ... Argumentos adicionais repassados internamente.
#'
#' @param max_attempts Numero maximo de tentativas de autenticacao.
#' @param retry_sleep_seconds Espera, em segundos, entre tentativas de autenticacao.
#' @param identifier ANA HidroWebService identifier. If NULL, environment variables are used.
#' @param password ANA HidroWebService password. If NULL, environment variables are used.
#' @param identificador Alias for `identifier`, kept for compatibility.
#' @param senha Alias for `password`, kept for compatibility.
#' @param cpf_cnpj Alias for `identifier`, kept for compatibility with older scripts.
#' @return Uma lista com duas tabelas: `sections` e `vertices`.
#' @keywords internal
#' @noRd
.hydrodatabr_original_get_ana_cross_sections <- function(token = NULL,
                                   station_code,
                                   start_date = NULL,
                                   end_date = NULL,
                                   consistency_level = NULL,
                                   identifier = NULL,
                                   password = NULL,
                                   identificador = NULL,
                                   senha = NULL,
                                   cpf_cnpj = NULL,
                                   max_attempts = 3,
                                   retry_sleep_seconds = 1) {
  token <- ana_stage04_token_or_authenticate(token, identifier, password, identificador, senha, cpf_cnpj, max_attempts, retry_sleep_seconds)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date)) stop("`start_date` and `end_date` are required for cross-section requests.", call. = FALSE)
  if (end_date < start_date) stop("`end_date` must be greater than or equal to `start_date`.", call. = FALSE)
  if (as.integer(end_date - start_date) + 1L > 366L) stop("Cross-section requests must cover at most 366 days.", call. = FALSE)
  response <- ana_stage04_request_product(
    endpoint = ana_cross_sections_endpoint(),
    token = token,
    station_code = station_code,
    start_date = start_date,
    end_date = end_date,
    consistency_level = consistency_level,
    product = "cross_sections"
  )
  result <- ana_standardize_cross_sections(response)
  if (!is.null(consistency_level)) {
    consistency_level <- as.character(consistency_level)
    if ("consistency_level" %in% names(result$sections)) {
      result$sections <- result$sections[as.character(result$sections$consistency_level) %in% consistency_level, , drop = FALSE]
    }
    if ("consistency_level" %in% names(result$vertices)) {
      result$vertices <- result$vertices[as.character(result$vertices$consistency_level) %in% consistency_level, , drop = FALSE]
    }
    row.names(result$sections) <- NULL
    row.names(result$vertices) <- NULL
  }
  result
}
ana_stage04_bind_tibbles <- function(x, empty) {
  x <- x[vapply(x, function(tbl) is.data.frame(tbl) && nrow(tbl) > 0, logical(1))]
  if (length(x) == 0) {
    return(empty)
  }
  tibble::as_tibble(do.call(rbind, lapply(x, as.data.frame)))
}
ana_stage04_batch_report <- function(reports) {
  if (length(reports) == 0) {
    return(tibble::tibble(
      station_code = character(),
      success = logical(),
      n_rows = integer(),
      message = character(),
      source = character()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(reports, as.data.frame)))
}
ana_stage04_batch <- function(station_codes, product_fun, empty, source, ...) {
  data <- list()
  reports <- list()
  for (station_code in as.character(station_codes)) {
    result <- tryCatch(
      product_fun(station_code = station_code, ...),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      reports[[length(reports) + 1]] <- data.frame(
        station_code = station_code,
        success = FALSE,
        n_rows = 0L,
        message = conditionMessage(result),
        source = source,
        stringsAsFactors = FALSE
      )
      next
    }
    data[[length(data) + 1]] <- result
    reports[[length(reports) + 1]] <- data.frame(
      station_code = station_code,
      success = TRUE,
      n_rows = nrow(result),
      message = "OK",
      source = source,
      stringsAsFactors = FALSE
    )
  }
  list(
    data = ana_stage04_bind_tibbles(data, empty),
    request_report = ana_stage04_batch_report(reports)
  )
}
#' Obter medicoes de descarga liquida da ANA em lote
#'
#' Consulta medicoes de descarga liquida para varias estacoes e retorna os
#' dados padronizados junto com um relatorio de requisicoes.
#'
#' @param station_codes Vetor de codigos de estacoes.
#' @param token Objeto de token retornado por [ana_authenticate()].
#' @param start_date Data inicial no formato `YYYY-MM-DD`.
#' @param end_date Data final no formato `YYYY-MM-DD`.
#' @param consistency_level Nivel de consistencia solicitado ao servico, quando aplicavel.
#'
#' @return Uma lista com `data` e `request_report`.
#' @keywords internal
get_ana_discharge_measurements_batch <- function(station_codes,
                                                 token = NULL,
                                                 start_date = NULL,
                                                 end_date = NULL,
                                                 consistency_level = NULL) {
  ana_stage04_batch(
    station_codes = station_codes,
    product_fun = get_ana_discharge_measurements,
    empty = ana_empty_discharge_measurements(),
    source = "ana_hidrowebservice_discharge_measurements",
    token = token,
    start_date = start_date,
    end_date = end_date,
    consistency_level = consistency_level
  )
}
#' Obter curvas-chave da ANA em lote
#'
#' Consulta curvas-chave para varias estacoes e retorna os dados padronizados
#' junto com um relatorio de requisicoes.
#'
#' @param station_codes Vetor de codigos de estacoes.
#' @param token Objeto de token retornado por [ana_authenticate()].
#' @param start_date Data inicial no formato `YYYY-MM-DD`.
#' @param end_date Data final no formato `YYYY-MM-DD`.
#' @param consistency_level Nivel de consistencia solicitado ao servico, quando aplicavel.
#'
#' @return Uma lista com `data` e `request_report`.
#' @keywords internal
get_ana_rating_curves_batch <- function(station_codes,
                                        token = NULL,
                                        start_date = NULL,
                                        end_date = NULL,
                                        consistency_level = NULL) {
  ana_stage04_batch(
    station_codes = station_codes,
    product_fun = get_ana_rating_curves,
    empty = ana_empty_rating_curves(),
    source = "ana_hidrowebservice_rating_curves",
    token = token,
    start_date = start_date,
    end_date = end_date,
    consistency_level = consistency_level
  )
}
#' Obter secoes transversais da ANA em lote
#'
#' Consulta secoes transversais para varias estacoes e retorna tabelas
#' padronizadas de secoes e vertices junto com um relatorio de requisicoes.
#'
#' @param station_codes Vetor de codigos de estacoes.
#' @param token Objeto de token retornado por [ana_authenticate()].
#' @param start_date Data inicial no formato `YYYY-MM-DD`.
#' @param end_date Data final no formato `YYYY-MM-DD`.
#' @param consistency_level Nivel de consistencia solicitado ao servico, quando aplicavel.
#'
#' @return Uma lista com `data` e `request_report`. O elemento `data` contem
#' os tibbles `sections` e `vertices`.
#' @keywords internal
get_ana_cross_sections_batch <- function(station_codes,
                                         token = NULL,
                                         start_date = NULL,
                                         end_date = NULL,
                                         consistency_level = NULL) {
  section_data <- list()
  vertex_data <- list()
  reports <- list()
  for (station_code in as.character(station_codes)) {
    result <- tryCatch(
      get_ana_cross_sections(
        token = token,
        station_code = station_code,
        start_date = start_date,
        end_date = end_date,
        consistency_level = consistency_level
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      reports[[length(reports) + 1]] <- data.frame(
        station_code = station_code,
        success = FALSE,
        n_rows = 0L,
        message = conditionMessage(result),
        source = "ana_hidrowebservice_cross_sections",
        stringsAsFactors = FALSE
      )
      next
    }
    section_data[[length(section_data) + 1]] <- result$sections
    vertex_data[[length(vertex_data) + 1]] <- result$vertices
    reports[[length(reports) + 1]] <- data.frame(
      station_code = station_code,
      success = TRUE,
      n_rows = nrow(result$sections),
      message = "OK",
      source = "ana_hidrowebservice_cross_sections",
      stringsAsFactors = FALSE
    )
  }
  empty <- ana_empty_cross_sections()
  list(
    data = list(
      sections = ana_stage04_bind_tibbles(section_data, empty$sections),
      vertices = ana_stage04_bind_tibbles(vertex_data, empty$vertices)
    ),
    request_report = ana_stage04_batch_report(reports)
  )
}
# Canonical ANA Stage 04 parameter helpers.
ana_stage04_start_param <- function() "Data Inicial (yyyy-MM-dd)"
ana_stage04_end_param <- function() "Data Final (yyyy-MM-dd)"
ana_stage04_type_filter_param <- function() "Tipo Filtro Data"
ana_stage04_filter_date_param <- function() ana_stage04_type_filter_param()
# Compatibility constants kept for older tests and user scripts.
ana_stage04_param_station <- ana_stage04_station_param()
ana_stage04_param_start <- ana_stage04_start_param()
ana_stage04_param_end <- ana_stage04_end_param()
ana_stage04_param_filter_date <- ana_stage04_type_filter_param()
ana_stage04_param_date_filter <- ana_stage04_type_filter_param()
