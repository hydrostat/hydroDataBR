# ANA legacy XML local-file reader.
#' Ler séries diárias de XML da ANA
#'
#' Lê um arquivo XML local da operação `HidroSerieHistorica` do WebService
#' legado da ANA e retorna uma série diária no contrato padronizado do pacote.
#' A função é indicada quando o usuário já possui o XML salvo em disco.
#'
#' @param path Caminho local para um arquivo XML.
#' @param variables Variáveis a retornar: `"all"`, `"discharge"`,
#'   `"stage"` ou `"rainfall"`. É possível informar mais de uma variável.
#' @param merge_consistency Se `TRUE`, mantém uma linha por estação, data e
#'   variável, priorizando dados consistidos quando existirem.
#'
#' @return `data.frame` com a série diária padronizada.
#' @export
#'
#' @examples
#' # Exemplo com arquivo XML local.
#' if (FALSE) {
#'   dados <- read_ana_xml("HidroSerieHistorica_56460000.xml")
#'   table_hydro_data(dados, table = "daily_availability")
#' }
read_ana_xml <- function(
    path,
    variables = c("all", "discharge", "stage", "rainfall"),
    merge_consistency = TRUE
) {
  path <- .check_local_file(path)
  variables <- .match_daily_variables(variables)
  table <- .read_ana_xml_series_table(path)
  out <- .standardize_ana_daily_table(
    data = table,
    source_label = "ANA WebService XML",
    merge_consistency = merge_consistency
  )
  out <- .filter_daily_variables(out, variables)
  .require_daily_data(out, basename(path))
}
.read_ana_xml_series_table <- function(path) {
  doc <- xml2::read_xml(path)
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='SerieHistorica']")
  if (length(nodes) == 0L) {
    stop(
      "O XML foi lido, mas nao contem registros SerieHistorica. ",
      "Verifique se o arquivo corresponde a operacao HidroSerieHistorica da ANA.",
      call. = FALSE
    )
  }
  records <- lapply(nodes, function(node) {
    children <- xml2::xml_children(node)
    values <- xml2::xml_text(children)
    names(values) <- xml2::xml_name(children)
    as.list(values)
  })
  dplyr::bind_rows(records) |>
    tibble::as_tibble()
}
