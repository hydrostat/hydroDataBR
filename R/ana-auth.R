# Safe ANA authentication and request helpers.
ana_auth_base_url <- function() {
  "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"
}
ana_auth_path <- function() {
  "/OAUth/v1"
}
ana_get_env_value <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  value <- as.character(value)[1L]
  if (is.na(value) || !nzchar(trimws(value))) return(NULL)
  value
}
ana_first_non_empty <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value)) next
    value <- as.character(value)[1L]
    if (!is.na(value) && nzchar(trimws(value))) return(value)
  }
  NULL
}
ana_build_url <- function(base_url, path = "", query = list()) {
  base_url <- sub("/+$", "", as.character(base_url)[1L])
  path <- as.character(path)[1L]
  if (is.na(path) || !nzchar(path)) {
    path <- ""
  } else if (!grepl("^/", path)) {
    path <- paste0("/", path)
  }
  query <- query[!vapply(query, is.null, logical(1L))]
  if (!length(query)) return(paste0(base_url, path))
  nms <- names(query)
  vals <- vapply(query, function(x) as.character(x)[1L], character(1L))
  keep <- !is.na(vals) & nzchar(vals)
  nms <- nms[keep]
  vals <- vals[keep]
  if (!length(vals)) return(paste0(base_url, path))
  qs <- paste0(utils::URLencode(nms, reserved = TRUE), "=", utils::URLencode(vals, reserved = TRUE), collapse = "&")
  paste0(base_url, path, "?", qs)
}
ana_perform_get <- function(url, headers = list(), timeout = 60) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for ANA network requests.", call. = FALSE)
  }
  headers <- headers[!vapply(headers, is.null, logical(1L))]
  headers <- headers[vapply(headers, function(x) length(x) > 0L && !is.na(as.character(x)[1L]), logical(1L))]
  headers <- lapply(headers, function(x) as.character(x)[1L])
  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (length(headers)) req <- do.call(httr2::req_headers, c(list(.req = req), headers))
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) stop(conditionMessage(resp), call. = FALSE)
  list(status = httr2::resp_status(resp), body = httr2::resp_body_string(resp), content_type = httr2::resp_content_type(resp))
}
ana_extract_token <- function(parsed) {
  fields <- c("tokenautenticacao", "tokenAutenticacao", "token_autenticacao", "token")
  candidates <- list(parsed)
  if (!is.null(parsed$items)) {
    candidates <- c(candidates, list(parsed$items))
    if (is.list(parsed$items) && length(parsed$items) && is.list(parsed$items[[1L]])) candidates <- c(candidates, parsed$items)
  }
  for (candidate in candidates) {
    for (field in fields) {
      value <- candidate[[field]]
      if (!is.null(value) && length(value)) {
        value <- as.character(value)[1L]
        if (!is.na(value) && nzchar(value)) return(value)
      }
    }
  }
  NULL
}
ana_token_value <- function(token) {
  if (inherits(token, "ana_token")) return(as.character(token$token)[1L])
  as.character(token)[1L]
}
ana_auth_credentials_from_args <- function(identifier = NULL, password = NULL, identificador = NULL, senha = NULL, cpf_cnpj = NULL, ...) {
  id <- ana_first_non_empty(identifier, identificador, cpf_cnpj, ana_get_env_value("ANA_HIDROWEBSERVICE_IDENTIFIER"), ana_get_env_value("ANA_HIDRO_IDENTIFICADOR"))
  pw <- ana_first_non_empty(password, senha, ana_get_env_value("ANA_HIDROWEBSERVICE_PASSWORD"), ana_get_env_value("ANA_HIDRO_SENHA"))
  list(identifier = id, password = pw)
}
ana_has_auth_credentials <- function(...) {
  auth <- ana_auth_credentials_from_args(...)
  !is.null(auth$identifier) && !is.null(auth$password)
}
#' Autenticar no HidroWebService da ANA
#'
#' Autentica o usuário no serviço autenticado da ANA e retorna um objeto de
#' token para ser usado nas consultas que exigem credenciais. O token é mantido
#' apenas em memória e o método de impressão evita mostrar seu valor sensível.
#'
#' Use esta função quando for consultar rotas da API autenticada, como inventário
#' de estações, séries diárias pela API, medições de descarga, curvas-chave ou
#' seções transversais. As credenciais podem ser informadas diretamente nos
#' argumentos ou por variáveis de ambiente, o que é mais seguro para scripts de
#' trabalho.
#'
#' @param identifier Identificador de acesso ao HidroWebService da ANA. Também
#'   pode ser fornecido pelas variáveis de ambiente
#'   `ANA_HIDROWEBSERVICE_IDENTIFIER` ou `ANA_HIDRO_IDENTIFICADOR`.
#' @param password Senha de acesso ao HidroWebService da ANA. Também pode ser
#'   fornecida pelas variáveis de ambiente `ANA_HIDROWEBSERVICE_PASSWORD` ou
#'   `ANA_HIDRO_SENHA`.
#' @param identificador Alias de compatibilidade para `identifier`.
#' @param senha Alias de compatibilidade para `password`.
#' @param cpf_cnpj Alias de compatibilidade para `identifier`.
#' @param base_url URL base do serviço autenticado. Em geral, mantenha o padrão.
#' @param timeout Tempo máximo da requisição, em segundos.
#' @param max_attempts Número máximo de tentativas de autenticação.
#' @param retry_sleep_seconds Espera entre tentativas, em segundos.
#'
#' @return Objeto de classe `ana_token`, adequado para uso nas funções de
#'   aquisição autenticada do pacote.
#' @export
#'
#' @examples
#' # Exemplo operacional. Requer credenciais válidas da ANA.
#' if (FALSE) {
#'   token <- ana_authenticate(
#'     identifier = Sys.getenv("ANA_HIDROWEBSERVICE_IDENTIFIER"),
#'     password = Sys.getenv("ANA_HIDROWEBSERVICE_PASSWORD")
#'   )
#' }
ana_authenticate <- function(identifier = NULL, password = NULL, identificador = NULL, senha = NULL, cpf_cnpj = NULL, base_url = ana_auth_base_url(), timeout = 60, max_attempts = 3, retry_sleep_seconds = 1) {
  auth <- ana_auth_credentials_from_args(identifier = identifier, password = password, identificador = identificador, senha = senha, cpf_cnpj = cpf_cnpj)
  identifier <- auth$identifier
  password <- auth$password
  if (is.null(identifier) || is.null(password)) {
    stop("Provide ANA API credentials through arguments or through the environment variables ANA_HIDROWEBSERVICE_IDENTIFIER/ANA_HIDROWEBSERVICE_PASSWORD or ANA_HIDRO_IDENTIFICADOR/ANA_HIDRO_SENHA.", call. = FALSE)
  }
  max_attempts <- suppressWarnings(as.integer(max_attempts)[1L])
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    ans <- tryCatch({
      response <- ana_perform_get(
        url = ana_build_url(base_url = base_url, path = ana_auth_path()),
        headers = list(Identificador = identifier, Senha = password, accept = "*/*"),
        timeout = timeout
      )
      if (response$status < 200 || response$status >= 300) stop("ANA authentication failed. HTTP status: ", response$status, ".", call. = FALSE)
      parsed <- tryCatch(jsonlite::fromJSON(response$body, simplifyVector = FALSE), error = function(e) e)
      if (inherits(parsed, "error")) stop("ANA authentication response could not be parsed as JSON.", call. = FALSE)
      token <- ana_extract_token(parsed)
      if (is.null(token)) stop("ANA authentication response did not contain a valid token.", call. = FALSE)
      out <- list(token = token, created_at = Sys.time(), expires_at = Sys.time() + 60 * 60, base_url = base_url)
      class(out) <- c("ana_token", "list")
      out
    }, error = function(e) e)
    if (!inherits(ans, "error")) return(ans)
    last_error <- ans
    if (attempt < max_attempts && retry_sleep_seconds > 0) Sys.sleep(retry_sleep_seconds)
  }
  stop(conditionMessage(last_error), call. = FALSE)
}
is_ana_token_expired <- function(token, safety_margin_seconds = 60) {
  if (!inherits(token, "ana_token")) stop("token must be an object created by ana_authenticate().", call. = FALSE)
  expires_at <- token$expires_at
  if (is.null(expires_at) || is.na(expires_at)) return(TRUE)
  Sys.time() + as.difftime(safety_margin_seconds, units = "secs") >= expires_at
}
#' @noRd
#' @exportS3Method base::print
print.ana_token <- function(x, ...) {
  cat("<ANA HidroWebService token>\n")
  cat("  created_at: ", format(x$created_at, usetz = TRUE), "\n", sep = "")
  cat("  expires_at: ", format(x$expires_at, usetz = TRUE), "\n", sep = "")
  cat("  token: <hidden>\n")
  invisible(x)
}
ana_request_json <- function(endpoint, query = list(), token = NULL, base_url = ana_auth_base_url(), timeout = 60) {
  if (!is.null(token) && inherits(token, "ana_token")) {
    if (is_ana_token_expired(token)) stop("ANA token is expired or too close to expiration.", call. = FALSE)
    base_url <- token$base_url
  }
  headers <- list(accept = "*/*")
  if (!is.null(token)) {
    token_text <- ana_token_value(token)
    if (is.na(token_text) || !nzchar(token_text)) stop("token is empty.", call. = FALSE)
    headers$Authorization <- paste("Bearer", token_text)
  }
  response <- ana_perform_get(url = ana_build_url(base_url = base_url, path = endpoint, query = query), headers = headers, timeout = timeout)
  if (response$status < 200 || response$status >= 300) {
    body <- gsub("[\r\n]+", " ", substr(response$body, 1L, 240L))
    body <- gsub("(?i)(authorization|bearer|token|senha|password|cpf|cnpj|identificador)[^,; ]*", "<redacted>", body, perl = TRUE)
    msg <- paste0("ANA JSON request failed. HTTP status: ", response$status, ".")
    if (nzchar(trimws(body))) msg <- paste0(msg, " Response body: ", body)
    stop(msg, call. = FALSE)
  }
  parsed <- tryCatch(jsonlite::fromJSON(response$body, flatten = TRUE), error = function(e) e)
  if (inherits(parsed, "error")) stop("ANA JSON response could not be parsed.", call. = FALSE)
  parsed
}
