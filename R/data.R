#' Inventario embutido de estacoes hidrometeorologicas da ANA
#'
#' Snapshot local de metadados principais de estacoes, derivado da tabela
#' `stations_minimal` do banco `shiny_minimal.duckdb` do projeto
#' `ana_api_get_clean`. O objeto serve para consultas locais rapidas, filtros
#' por posto e apoio a funcoes de analise e visualizacao. Para obter dados
#' atualizados diretamente do servico da ANA, use [get_ana_stations()] ou
#' [get_ana_data()].
#'
#' @format Um data frame com 37.584 linhas e 27 colunas:
#' \describe{
#'   \item{station_code}{Codigo da estacao.}
#'   \item{station_name}{Nome da estacao.}
#'   \item{station_type}{Tipo da estacao.}
#'   \item{state_code}{Sigla da unidade federativa.}
#'   \item{municipality}{Nome do municipio.}
#'   \item{basin_code}{Codigo da bacia hidrografica.}
#'   \item{basin_name}{Nome da bacia hidrografica.}
#'   \item{latitude}{Latitude da estacao, em graus decimais.}
#'   \item{longitude}{Longitude da estacao, em graus decimais.}
#'   \item{altitude_m}{Altitude da estacao, em metros, quando disponivel.}
#'   \item{drainage_area_km2}{Area de drenagem, em km2, quando disponivel.}
#'   \item{operator}{Entidade operadora registrada.}
#'   \item{responsible_agency}{Entidade responsavel registrada.}
#'   \item{is_operating}{Indicador logico de operacao da estacao.}
#'   \item{discharge_start_date}{Data inicial disponivel para vazao diaria.}
#'   \item{discharge_end_date}{Data final disponivel para vazao diaria.}
#'   \item{telemetric_start_date}{Data inicial disponivel para dados telemetricos.}
#'   \item{telemetric_end_date}{Data final disponivel para dados telemetricos.}
#'   \item{stage_start_date}{Data inicial disponivel para cota diaria.}
#'   \item{stage_end_date}{Data final disponivel para cota diaria.}
#'   \item{rainfall_start_date}{Data inicial disponivel para chuva diaria.}
#'   \item{rainfall_end_date}{Data final disponivel para chuva diaria.}
#'   \item{has_discharge_measurements}{Indica presenca de medicoes de descarga.}
#'   \item{has_telemetry}{Indica presenca de dados telemetricos.}
#'   \item{has_stage_data}{Indica presenca de dados de cota.}
#'   \item{has_rainfall_data}{Indica presenca de dados de chuva.}
#'   \item{last_update}{Data da ultima atualizacao registrada no snapshot.}
#' }
#'
#' @source Banco `shiny_minimal.duckdb`, tabela `stations_minimal`, do projeto
#' `ana_api_get_clean`.
#' @usage data(ana_stations)
#' @name ana_stations
#' @docType data
#' @keywords datasets
"ana_stations"
