# Internal helpers for station metadata filtering.
ana_load_builtin_stations <- function() {
  ns <- environment(ana_load_builtin_stations)
  if (exists("ana_stations", envir = ns, inherits = FALSE)) {
    return(get("ana_stations", envir = ns, inherits = FALSE))
  }
  data_env <- new.env(parent = emptyenv())
  loaded <- tryCatch(
    {
      utils::data("ana_stations", package = "hydroDataBR", envir = data_env)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!loaded || !exists("ana_stations", envir = data_env, inherits = FALSE)) {
    stop("Built-in dataset `ana_stations` could not be loaded.", call. = FALSE)
  }
  get("ana_stations", envir = data_env, inherits = FALSE)
}
ana_require_station_column <- function(station_data, column) {
  if (!column %in% names(station_data)) {
    stop("Station data must contain column `", column, "`.", call. = FALSE)
  }
}
ana_non_empty_value <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}
ana_has_any_column_value <- function(station_data, columns) {
  present <- intersect(columns, names(station_data))
  if (length(present) == 0L) {
    stop(
      "Station data do not contain any of the required availability columns: ",
      paste(columns, collapse = ", "),
      call. = FALSE
    )
  }
  available <- rep(FALSE, nrow(station_data))
  for (column in present) {
    available <- available | ana_non_empty_value(station_data[[column]])
  }
  available
}
ana_station_has_product <- function(station_data, product) {
  switch(
    product,
    discharge = ana_has_any_column_value(
      station_data,
      c("discharge_start_date", "discharge_end_date")
    ),
    stage = if ("has_stage_data" %in% names(station_data)) {
      station_data$has_stage_data %in% TRUE
    } else {
      ana_has_any_column_value(station_data, c("stage_start_date", "stage_end_date"))
    },
    rainfall = if ("has_rainfall_data" %in% names(station_data)) {
      station_data$has_rainfall_data %in% TRUE
    } else {
      ana_has_any_column_value(station_data, c("rainfall_start_date", "rainfall_end_date"))
    },
    telemetry = if ("has_telemetry" %in% names(station_data)) {
      station_data$has_telemetry %in% TRUE
    } else {
      ana_has_any_column_value(station_data, c("telemetric_start_date", "telemetric_end_date"))
    },
    discharge_measurements = {
      ana_require_station_column(station_data, "has_discharge_measurements")
      station_data$has_discharge_measurements %in% TRUE
    }
  )
}
#' Filtrar o inventário embutido de estações ANA
#'
#' Filtra o conjunto `ana_stations`, incluído no pacote, ou uma tabela de
#' estações fornecida pelo usuário. A função ajuda a localizar postos por código,
#' estado, município, tipo de estação, bacia, nome, situação operacional e
#' disponibilidade de produtos hidrológicos.
#'
#' @param station_data Tabela de metadados de estações. Se `NULL`, usa o
#'   inventário embutido `ana_stations`.
#' @param station_code Código(s) de estação.
#' @param state_code Sigla(s) de unidade federativa.
#' @param municipality Nome(s) de município para filtro exato.
#' @param station_type Tipo(s) de estação.
#' @param basin_code Código(s) de bacia.
#' @param name_pattern Texto ou expressão regular para busca no nome da estação.
#' @param product Produto cuja disponibilidade é exigida. Valores aceitos:
#'   `"discharge"`, `"stage"`, `"rainfall"`, `"telemetry"` e
#'   `"discharge_measurements"`.
#' @param is_operating Valor lógico para filtrar estações em operação.
#'
#' @return `data.frame` com as estações que atendem aos filtros informados.
#' @export
#'
#' @examples
#' estacoes_mg <- filter_ana_stations(
#'   state_code = "MG",
#'   product = "discharge"
#' )
#' head(estacoes_mg)
filter_ana_stations <- function(station_data = NULL,
                                station_code = NULL,
                                state_code = NULL,
                                municipality = NULL,
                                station_type = NULL,
                                basin_code = NULL,
                                name_pattern = NULL,
                                product = NULL,
                                is_operating = NULL) {
  if (is.null(station_data)) {
    station_data <- ana_load_builtin_stations()
  }
  if (!is.data.frame(station_data)) {
    stop("`station_data` must be a data frame.", call. = FALSE)
  }
  out <- station_data
  if (!is.null(station_code)) {
    ana_require_station_column(out, "station_code")
    out <- out[as.character(out$station_code) %in% as.character(station_code), , drop = FALSE]
  }
  if (!is.null(state_code)) {
    ana_require_station_column(out, "state_code")
    out <- out[toupper(as.character(out$state_code)) %in% toupper(as.character(state_code)), , drop = FALSE]
  }
  if (!is.null(municipality)) {
    ana_require_station_column(out, "municipality")
    out <- out[toupper(as.character(out$municipality)) %in% toupper(as.character(municipality)), , drop = FALSE]
  }
  if (!is.null(station_type)) {
    ana_require_station_column(out, "station_type")
    out <- out[toupper(as.character(out$station_type)) %in% toupper(as.character(station_type)), , drop = FALSE]
  }
  if (!is.null(basin_code)) {
    ana_require_station_column(out, "basin_code")
    out <- out[as.character(out$basin_code) %in% as.character(basin_code), , drop = FALSE]
  }
  if (!is.null(name_pattern)) {
    ana_require_station_column(out, "station_name")
    out <- out[grepl(name_pattern, out$station_name, ignore.case = TRUE), , drop = FALSE]
  }
  if (!is.null(is_operating)) {
    ana_require_station_column(out, "is_operating")
    if (length(is_operating) != 1L || is.na(is_operating)) {
      stop("`is_operating` must be TRUE, FALSE, or NULL.", call. = FALSE)
    }
    out <- out[(out$is_operating %in% TRUE) == isTRUE(is_operating), , drop = FALSE]
  }
  if (!is.null(product)) {
    valid_products <- c(
      "discharge",
      "stage",
      "rainfall",
      "telemetry",
      "discharge_measurements"
    )
    invalid_products <- setdiff(product, valid_products)
    if (length(invalid_products) > 0L) {
      stop(
        "Invalid `product`: ",
        paste(invalid_products, collapse = ", "),
        call. = FALSE
      )
    }
    for (single_product in product) {
      keep <- ana_station_has_product(out, single_product)
      out <- out[keep %in% TRUE, , drop = FALSE]
    }
  }
  row.names(out) <- NULL
  out
}
