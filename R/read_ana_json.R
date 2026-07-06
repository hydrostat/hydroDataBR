# ANA API JSON local-file reader.
#' Ler séries diárias de JSON da ANA
#'
#' Lê um arquivo JSON local com registros de série histórica da ANA, incluindo
#' respostas que usam o campo `items`, e converte os dados para o contrato
#' diário padronizado do hydroDataBR.
#'
#' @param path Caminho local para um arquivo JSON.
#' @param variables Variáveis a retornar: `"all"`, `"discharge"`,
#'   `"stage"` ou `"rainfall"`. É possível informar mais de uma variável.
#' @param merge_consistency Se `TRUE`, mantém uma linha por estação, data e
#'   variável, priorizando dados consistidos quando existirem.
#'
#' @return `data.frame` com a série diária padronizada.
#' @export
#'
#' @examples
#' # Exemplo com arquivo JSON local.
#' if (FALSE) {
#'   dados <- read_ana_json("serie_historica_56460000.json")
#'   analyze_hydro_data(dados, analysis = "daily_availability")
#' }
read_ana_json <- function(
    path,
    variables = c("all", "discharge", "stage", "rainfall"),
    merge_consistency = TRUE
) {
  path <- .check_local_file(path)
  variables <- .match_daily_variables(variables)
  table <- .read_ana_json_series_table(path)
  out <- .standardize_ana_daily_table(
    data = table,
    source_label = "ANA API JSON",
    merge_consistency = merge_consistency
  )
  out <- .filter_daily_variables(out, variables)
  .require_daily_data(out, basename(path))
}
.read_ana_json_series_table <- function(path) {
  content <- jsonlite::fromJSON(path, flatten = TRUE, simplifyVector = TRUE)
  if (is.data.frame(content)) {
    return(tibble::as_tibble(content))
  }
  if (!is.null(content$items)) {
    if (is.data.frame(content$items)) {
      return(tibble::as_tibble(content$items))
    }
    if (is.list(content$items) && length(content$items) > 0L) {
      return(dplyr::bind_rows(content$items) |> tibble::as_tibble())
    }
  }
  stop("O JSON nao contem registros de serie historica reconhecidos.", call. = FALSE)
}
