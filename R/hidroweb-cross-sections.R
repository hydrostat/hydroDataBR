#' Ler seções transversais de arquivos HidroWeb
#'
#' Lê o arquivo `PerfilTransversal.csv` exportado pelo HidroWeb, diretamente ou
#' dentro de um ZIP, e retorna duas tabelas padronizadas: uma com as seções e
#' outra com os vértices do perfil transversal.
#'
#' @param file Caminho para um arquivo `.zip` do HidroWeb ou para um arquivo
#'   `PerfilTransversal.csv` já extraído.
#'
#' @return Lista com os elementos `sections` e `vertices`.
#' @export
#'
#' @examples
#' # Exemplo com arquivo local do HidroWeb.
#' if (FALSE) {
#'   secoes <- read_hidroweb_cross_sections("Estacao_56460000.zip")
#'   plot_hydro_data(secoes, plot = "cross_section_profile")
#' }
read_hidroweb_cross_sections <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || file == "") {
    stop("`file` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("`file` does not exist.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  if (identical(ext, "zip")) {
    tmp_dir <- tempfile("hidroweb_cross_sections_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    zip_info <- utils::unzip(file, list = TRUE)
    target <- zip_info$Name[grepl("PerfilTransversal\\.csv$", zip_info$Name, ignore.case = TRUE)]
    if (length(target) == 0L) {
      stop("No PerfilTransversal.csv file was found in the ZIP archive.", call. = FALSE)
    }
    utils::unzip(file, files = target[1L], exdir = tmp_dir)
    csv_file <- file.path(tmp_dir, target[1L])
  } else {
    csv_file <- file
  }
  ana_parse_hidroweb_cross_sections_csv(csv_file)
}
ana_parse_hidroweb_cross_sections_csv <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "bytes")
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")
  lines[is.na(lines)] <- ""
  header_line <- grep("^EstacaoCodigo;", lines, useBytes = TRUE)
  if (length(header_line) == 0L) {
    stop("The file does not contain a PerfilTransversal.csv header.", call. = FALSE)
  }
  csv_text <- paste(lines[header_line[1L]:length(lines)], collapse = "\n")
  raw <- utils::read.csv(
    text = csv_text,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA", "null")
  )
  ana_standardize_hidroweb_cross_sections_table(raw)
}
ana_standardize_hidroweb_cross_sections_table <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(ana_empty_cross_sections())
  }
  required <- c(
    "EstacaoCodigo", "NivelConsistencia", "Data", "Hora",
    "NumLevantamento", "TipoSecao", "NumVerticais", "Vertical"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop(
      "PerfilTransversal.csv is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  section_rows <- vector("list", nrow(raw))
  vertex_rows <- list()
  vertex_counter <- 0L
  for (i in seq_len(nrow(raw))) {
    station_code <- as.character(raw$EstacaoCodigo[i])
    consistency_level <- ana_hidroweb_cross_sections_as_integer(raw$NivelConsistencia[i])
    survey_number <- ana_hidroweb_cross_sections_as_integer(raw$NumLevantamento[i])
    section_type <- ana_hidroweb_cross_sections_as_integer(raw$TipoSecao[i])
    measurement_datetime <- ana_parse_hidroweb_cross_section_datetime(
      raw$Data[i],
      raw$Hora[i]
    )
    vertices <- ana_parse_hidroweb_vertical(raw$Vertical[i])
    n_vertices <- nrow(vertices)
    cross_section_id <- paste(
      station_code,
      format(measurement_datetime, "%Y%m%d%H%M%S"),
      ifelse(is.na(survey_number), "NA", survey_number),
      ifelse(is.na(section_type), "NA", section_type),
      sep = "_"
    )
    section_rows[[i]] <- data.frame(
      cross_section_id = cross_section_id,
      station_code = station_code,
      measurement_datetime = measurement_datetime,
      consistency_level = consistency_level,
      survey_number = survey_number,
      section_type = section_type,
      n_vertices = n_vertices,
      source_status = NA_character_,
      source = "HidroWeb ZIP: PerfilTransversal.csv",
      stringsAsFactors = FALSE
    )
    if (n_vertices > 0L) {
      vertex_rows_i <- data.frame(
        cross_section_id = rep(cross_section_id, n_vertices),
        cross_section_vertex_id = paste0(cross_section_id, "_", seq_len(n_vertices)),
        station_code = rep(station_code, n_vertices),
        measurement_datetime = rep(measurement_datetime, n_vertices),
        consistency_level = rep(consistency_level, n_vertices),
        survey_number = rep(survey_number, n_vertices),
        section_type = rep(section_type, n_vertices),
        vertex_order = seq_len(n_vertices),
        vertex_distance_m = vertices$vertex_distance_m,
        vertex_stage_cm = vertices$vertex_stage_cm,
        source_status = rep(NA_character_, n_vertices),
        source = rep("HidroWeb ZIP: PerfilTransversal.csv", n_vertices),
        stringsAsFactors = FALSE
      )
      vertex_counter <- vertex_counter + n_vertices
      vertex_rows[[length(vertex_rows) + 1L]] <- vertex_rows_i
    }
  }
  sections <- do.call(rbind, section_rows)
  vertices <- if (length(vertex_rows) == 0L) {
    data.frame(
      cross_section_id = character(),
      cross_section_vertex_id = character(),
      station_code = character(),
      measurement_datetime = as.POSIXct(character(), tz = "UTC"),
      consistency_level = integer(),
      survey_number = integer(),
      section_type = integer(),
      vertex_order = integer(),
      vertex_distance_m = numeric(),
      vertex_stage_cm = numeric(),
      source_status = character(),
      source = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, vertex_rows)
  }
  row.names(sections) <- NULL
  row.names(vertices) <- NULL
  list(
    sections = tibble::as_tibble(sections),
    vertices = tibble::as_tibble(vertices)
  )
}
ana_parse_hidroweb_cross_section_datetime <- function(date_value, time_value) {
  date_part <- as.Date(as.character(date_value), format = "%d/%m/%Y")
  time_text <- as.character(time_value)
  time_part <- sub("^.*?(\\d{1,2}:\\d{2})(?::\\d{2})?.*$", "\\1", time_text, perl = TRUE)
  if (is.na(date_part)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  if (is.na(time_part) || !grepl("^\\d{1,2}:\\d{2}$", time_part)) {
    time_part <- "00:00"
  }
  as.POSIXct(
    paste(format(date_part, "%Y-%m-%d"), time_part),
    tz = "UTC"
  )
}
ana_parse_hidroweb_vertical <- function(vertical_text) {
  if (length(vertical_text) == 0L || is.na(vertical_text) || vertical_text == "") {
    return(data.frame(vertex_distance_m = numeric(), vertex_stage_cm = numeric()))
  }
  parts <- strsplit(as.character(vertical_text), "\\|", fixed = FALSE)[[1L]]
  parts <- parts[nzchar(parts)]
  out <- lapply(parts, ana_parse_hidroweb_vertical_token)
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) {
    return(data.frame(vertex_distance_m = numeric(), vertex_stage_cm = numeric()))
  }
  do.call(rbind, out)
}
ana_parse_hidroweb_vertical_token <- function(token) {
  token <- trimws(token)
  if (!nzchar(token)) {
    return(NULL)
  }
  pieces <- strsplit(token, ",", fixed = TRUE)[[1L]]
  pieces <- trimws(pieces)
  if (length(pieces) < 2L) {
    return(NULL)
  }
  stage_text <- pieces[length(pieces)]
  distance_pieces <- pieces[-length(pieces)]
  if (length(distance_pieces) == 1L) {
    distance_text <- distance_pieces[1L]
  } else {
    distance_text <- paste0(distance_pieces[1L], ".", paste(distance_pieces[-1L], collapse = ""))
  }
  data.frame(
    vertex_distance_m = suppressWarnings(as.numeric(distance_text)),
    vertex_stage_cm = suppressWarnings(as.numeric(stage_text)),
    stringsAsFactors = FALSE
  )
}
# Convert HidroWeb cross-section integer fields safely.
ana_hidroweb_cross_sections_as_integer <- function(x) {
  if (length(x) == 0) {
    return(NA_integer_)
  }
  value <- trimws(as.character(x))
  value[value == ""] <- NA_character_
  value <- gsub(",", ".", value, fixed = TRUE)
  out <- suppressWarnings(as.integer(as.numeric(value)))
  out
}
