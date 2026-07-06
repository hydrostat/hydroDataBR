# HidroWeb local-file readers.
#' Ler séries diárias de arquivos HidroWeb
#'
#' Lê arquivos locais CSV ou ZIP baixados do HidroWeb e converte as séries
#' diárias para o contrato padronizado do hydroDataBR. A função não acessa a
#' internet; ela trabalha apenas com arquivos já disponíveis no computador.
#'
#' @param path Caminho local para um arquivo CSV ou ZIP do HidroWeb.
#' @param variables Variáveis a retornar: `"all"`, `"discharge"`,
#'   `"stage"` ou `"rainfall"`. É possível informar mais de uma variável.
#' @param merge_consistency Se `TRUE`, mantém uma linha por estação, data e
#'   variável, priorizando dados consistidos (`NivelConsistencia = 2`) quando
#'   existirem. Na ausência de dado consistido, usa o dado não consistido
#'   disponível.
#'
#' @return `data.frame` com as colunas `station_code`, `date`, `variable`,
#'   `value`, `unit`, `consistency_level`, `source_status` e `source`.
#' @export
#'
#' @examples
#' # Exemplo com arquivo local ja baixado do HidroWeb.
#' if (FALSE) {
#'   dados <- read_hidroweb("Estacao_56460000.zip")
#'   plot_hydro_data(dados, plot = "daily_series")
#' }
read_hidroweb <- function(
    path,
    variables = c("all", "discharge", "stage", "rainfall"),
    merge_consistency = TRUE
) {
  path <- .check_local_file(path)
  variables <- .match_daily_variables(variables)
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    out <- .read_hidroweb_zip(path, merge_consistency = merge_consistency)
  } else {
    table <- .read_hidroweb_csv_table(path)
    out <- .standardize_ana_daily_table(
      data = table,
      source_label = paste0("HidroWeb CSV: ", basename(path)),
      merge_consistency = merge_consistency
    )
  }
  out <- .filter_daily_variables(out, variables)
  .require_daily_data(out, basename(path))
}
.read_hidroweb_csv_table <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "Latin1")
  header_candidates <- which(grepl("^\\s*EstacaoCodigo\\s*;", lines))
  if (length(header_candidates) == 0L) {
    stop("Nao foi possivel localizar o cabecalho EstacaoCodigo no arquivo CSV.", call. = FALSE)
  }
  header_line <- header_candidates[[1L]]
  utils::read.csv2(
    file = path,
    skip = header_line - 1L,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN", "NULL", "null"),
    fileEncoding = "Latin1",
    check.names = FALSE,
    colClasses = "character"
  ) |>
    tibble::as_tibble()
}
.read_hidroweb_zip <- function(path, merge_consistency = TRUE) {
  unzip_dir <- tempfile("hydrodatabr_hidroweb_zip_")
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(unzip_dir, recursive = TRUE, force = TRUE), add = TRUE)
  extracted_files <- utils::unzip(path, exdir = unzip_dir)
  csv_files <- extracted_files[grepl("\\.csv$", extracted_files, ignore.case = TRUE)]
  if (length(csv_files) == 0L) {
    stop("O ZIP nao contem arquivos CSV.", call. = FALSE)
  }
  daily_files <- csv_files[grepl(
    "Vazoes|Vazao|Cotas|Cota|Chuva|Chuvas|Pluv",
    basename(csv_files),
    ignore.case = TRUE
  )]
  if (length(daily_files) == 0L) {
    stop("O ZIP nao contem arquivos diarios de vazao, cota ou chuva reconhecidos.", call. = FALSE)
  }
  pieces <- vector("list", length(daily_files))
  for (i in seq_along(daily_files)) {
    table <- .read_hidroweb_csv_table(daily_files[[i]])
    pieces[[i]] <- .standardize_ana_daily_table(
      data = table,
      source_label = paste0("HidroWeb ZIP: ", basename(daily_files[[i]])),
      merge_consistency = merge_consistency
    )
  }
  dplyr::bind_rows(pieces) |>
    dplyr::arrange(.data$station_code, .data$date, .data$variable)
}
