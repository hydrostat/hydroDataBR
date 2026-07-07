#' Baixar vértices opcionais de seções transversais
#'
#' Baixa o arquivo opcional com os vértices completos de seções transversais da
#' ANA incluído no repositório GitHub do pacote, mas não instalado junto com o
#' pacote principal.
#'
#' O pacote principal inclui metadados e resumos de seções transversais, mas não
#' inclui os vértices completos dos perfis. Esta função permite baixar esses
#' vértices quando o usuário precisar reconstruir ou analisar os perfis
#' transversais completos.
#'
#' O download usa cache local do usuário. A função não consulta serviços vivos
#' da ANA e não exige credenciais.
#'
#' @param path Caminho de diretório onde o arquivo será salvo. Por padrão, usa o
#'   diretório de cache do usuário para o pacote.
#' @param version Versão do retrato de dados. Atualmente apenas `"2026-06"` é
#'   suportado.
#' @param read Valor lógico. Se `TRUE`, lê e retorna o objeto R após baixar ou
#'   localizar o arquivo em cache. Se `FALSE`, retorna apenas o caminho local do
#'   arquivo.
#' @param overwrite Valor lógico. Se `TRUE`, força novo download mesmo quando o
#'   arquivo já existe no cache.
#' @param quiet Valor lógico. Se `TRUE`, reduz mensagens durante o download.
#' @param url URL alternativa ou caminho local alternativo para o arquivo `.rds`.
#'   Este argumento é principalmente útil para testes.
#' @param metadata_url URL alternativa ou caminho local alternativo para o
#'   arquivo de metadados `.csv`. Este argumento é principalmente útil para
#'   testes.
#'
#' @return Se `read = TRUE`, um `data.frame` com os vértices de seções
#'   transversais. Se `read = FALSE`, o caminho local do arquivo baixado.
#'
#' @examples
#' if (FALSE) {
#'   vertices <- download_ana_cross_section_vertices()
#'   head(vertices)
#'
#'   path <- download_ana_cross_section_vertices(read = FALSE)
#'   path
#' }
#'
#' @export
download_ana_cross_section_vertices <- function(
    path = tools::R_user_dir("hydroDataBR", "cache"),
    version = "2026-06",
    read = TRUE,
    overwrite = FALSE,
    quiet = FALSE,
    url = NULL,
    metadata_url = NULL
) {
  if (!identical(version, "2026-06")) {
    stop("Only version '2026-06' is currently supported.", call. = FALSE)
  }
  
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty character string.", call. = FALSE)
  }
  
  if (!is.logical(read) || length(read) != 1 || is.na(read)) {
    stop("`read` must be TRUE or FALSE.", call. = FALSE)
  }
  
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }
  
  base_url <- "https://raw.githubusercontent.com/hydrostat/hydroDataBR/main/data-optional"
  file_name <- paste0("ana_cross_section_vertices_", version, ".rds")
  metadata_name <- paste0("ana_cross_section_vertices_", version, "_metadata.csv")
  
  if (is.null(url)) {
    url <- paste0(base_url, "/", file_name)
  }
  
  if (is.null(metadata_url)) {
    metadata_url <- paste0(base_url, "/", metadata_name)
  }
  
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(path, file_name)
  
  expected_sha256 <- hydrodatabr_optional_vertices_sha256(metadata_url, quiet = quiet)
  
  if (!file.exists(local_file) || overwrite) {
    if (!quiet) {
      message("Downloading optional cross-section vertices to: ", local_file)
    }
    hydrodatabr_download_or_copy(url, local_file, quiet = quiet)
  }
  
  if (!file.exists(local_file)) {
    stop("Download failed; file was not created: ", local_file, call. = FALSE)
  }
  
  if (!is.null(expected_sha256) && nzchar(expected_sha256)) {
    actual_sha256 <- unname(tools::sha256sum(local_file))
    if (!identical(tolower(actual_sha256), tolower(expected_sha256))) {
      unlink(local_file)
      stop(
        "Checksum mismatch for downloaded file. The local file was removed.",
        call. = FALSE
      )
    }
  }
  
  if (!read) {
    return(normalizePath(local_file, winslash = "/", mustWork = TRUE))
  }
  
  readRDS(local_file)
}

hydrodatabr_optional_vertices_sha256 <- function(metadata_url, quiet = FALSE) {
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch(
    {
      hydrodatabr_download_or_copy(metadata_url, tmp, quiet = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  
  if (!ok || !file.exists(tmp)) {
    if (!quiet) {
      message("Could not read optional data metadata; checksum validation skipped.")
    }
    return(NULL)
  }
  
  metadata <- tryCatch(
    utils::read.csv(tmp, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(metadata) || !"sha256" %in% names(metadata) || !nrow(metadata)) {
    if (!quiet) {
      message("Optional data metadata has no SHA256 field; checksum validation skipped.")
    }
    return(NULL)
  }
  
  as.character(metadata$sha256[[1]])
}

hydrodatabr_download_or_copy <- function(source, destination, quiet = FALSE) {
  if (file.exists(source)) {
    ok <- file.copy(source, destination, overwrite = TRUE)
    if (!ok) {
      stop("Could not copy local file: ", source, call. = FALSE)
    }
    return(invisible(destination))
  }
  
  status <- utils::download.file(
    url = source,
    destfile = destination,
    mode = "wb",
    quiet = quiet
  )
  
  if (!identical(status, 0L)) {
    stop("Could not download file from: ", source, call. = FALSE)
  }
  
  invisible(destination)
}