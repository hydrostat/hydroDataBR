# Daily conventional ANA acquisition functions.
ana_legacy_base_url <- function() {
  "https://telemetriaws1.ana.gov.br/ServiceANA.asmx"
}
ana_daily_variable_spec <- function(variable) {
  variable <- match.arg(variable, c("discharge", "stage", "rainfall"))
  switch(
    variable,
    discharge = list(variable = "discharge", data_type = "3", unit = "m3/s"),
    stage = list(variable = "stage", data_type = "1", unit = "cm"),
    rainfall = list(variable = "rainfall", data_type = "2", unit = "mm")
  )
}
ana_format_legacy_date <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  x <- x[1]
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%d/%m/%Y"))
  }
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") {
    return("")
  }
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) {
    return(x)
  }
  date_value <- .parse_ana_date(x)
  if (is.na(date_value)) {
    stop("date_start and date_end must be Date objects or valid date strings.", call. = FALSE)
  }
  format(date_value, "%d/%m/%Y")
}
ana_request_xml_file <- function(path, query = list(), base_url = ana_legacy_base_url(), timeout = 60) {
  url <- ana_build_url(base_url = base_url, path = path, query = query)
  response <- ana_perform_get(
    url = url,
    headers = list(accept = "text/xml"),
    timeout = timeout
  )
  if (response$status < 200 || response$status >= 300) {
    stop("ANA XML request failed. HTTP status: ", response$status, ".", call. = FALSE)
  }
  xml_path <- tempfile(fileext = ".xml")
  writeLines(response$body, con = xml_path, useBytes = TRUE)
  xml_path
}
get_ana_daily_series <- function(
    station_code,
    variable,
    date_start = NULL,
    date_end = NULL,
    consistency_level = "2",
    base_url = ana_legacy_base_url(),
    timeout = 60
) {
  spec <- ana_daily_variable_spec(variable)
  station_code <- trimws(as.character(station_code)[1])
  consistency_level <- as.character(consistency_level)[1]
  if (is.na(station_code) || station_code == "") {
    stop("station_code must be provided.", call. = FALSE)
  }
  if (!consistency_level %in% c("1", "2")) {
    stop("consistency_level must be '1' or '2'.", call. = FALSE)
  }
  xml_path <- ana_request_xml_file(
    path = "/HidroSerieHistorica",
    query = list(
      codEstacao = station_code,
      dataInicio = ana_format_legacy_date(date_start),
      dataFim = ana_format_legacy_date(date_end),
      tipoDados = spec$data_type,
      nivelConsistencia = consistency_level
    ),
    base_url = base_url,
    timeout = timeout
  )
  data <- read_ana_xml(xml_path)
  data <- ana_keep_daily_contract(data)
  data <- data[data$variable == spec$variable, , drop = FALSE]
  ana_prefer_daily_rows(data)
}
#' Obter vazao diaria pela rota WebService legada
#'
#' Obtem uma serie diaria convencional de vazao pela operacao
#' `HidroSerieHistorica` do WebService XML legado da ANA e retorna o contrato
#' diario padronizado usado pelos leitores offline.
#'
#' @details
#' Esta e uma rota de baixo nivel mantida para compatibilidade interna. Para uso
#' cotidiano, prefira [get_ana_data()] ou os atalhos publicos
#' `get_ana_daily_discharge()`, `get_ana_daily_stage()` e
#' `get_ana_daily_rainfall()`, que usam os argumentos publicos
#' `start_date` e `end_date`.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param date_start Data inicial. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param date_end Data final. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param consistency_level Nivel de consistencia a solicitar, geralmente
#'   `"1"` ou `"2"`.
#' @param timeout Tempo maximo da requisicao, em segundos.
#' @param base_url URL base do WebService XML legado da ANA.
#'
#' @return Um `data.frame`/`tibble` no contrato diario padronizado:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#' @keywords internal
#'
#' @examples
#' if (FALSE) {
#'   dados <- ana_get_daily_discharge_impl(
#'     "00000000",
#'     date_start = "2020-01-01",
#'     date_end = "2020-01-31"
#'   )
#' }
#' @noRd
ana_get_daily_discharge_impl <- function(
    station_code,
    date_start = NULL,
    date_end = NULL,
    consistency_level = "2",
    timeout = 60,
    base_url = ana_legacy_base_url()
) {
  get_ana_daily_series(
    station_code = station_code,
    variable = "discharge",
    date_start = date_start,
    date_end = date_end,
    consistency_level = consistency_level,
    timeout = timeout,
    base_url = base_url
  )
}
#' Obter cota diaria pela rota WebService legada
#'
#' Obtem uma serie diaria convencional de cota pela operacao
#' `HidroSerieHistorica` do WebService XML legado da ANA e retorna o contrato
#' diario padronizado usado pelos leitores offline.
#'
#' @details
#' Esta e uma rota de baixo nivel mantida para compatibilidade interna. Para uso
#' cotidiano, prefira [get_ana_data()] ou os atalhos publicos
#' `get_ana_daily_discharge()`, `get_ana_daily_stage()` e
#' `get_ana_daily_rainfall()`, que usam os argumentos publicos
#' `start_date` e `end_date`.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param date_start Data inicial. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param date_end Data final. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param consistency_level Nivel de consistencia a solicitar, geralmente
#'   `"1"` ou `"2"`.
#' @param timeout Tempo maximo da requisicao, em segundos.
#' @param base_url URL base do WebService XML legado da ANA.
#'
#' @return Um `data.frame`/`tibble` no contrato diario padronizado:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#' @keywords internal
#'
#' @examples
#' if (FALSE) {
#'   dados <- ana_get_daily_stage_impl(
#'     "00000000",
#'     date_start = "2020-01-01",
#'     date_end = "2020-01-31"
#'   )
#' }
#' @noRd
ana_get_daily_stage_impl <- function(
    station_code,
    date_start = NULL,
    date_end = NULL,
    consistency_level = "2",
    timeout = 60,
    base_url = ana_legacy_base_url()
) {
  get_ana_daily_series(
    station_code = station_code,
    variable = "stage",
    date_start = date_start,
    date_end = date_end,
    consistency_level = consistency_level,
    timeout = timeout,
    base_url = base_url
  )
}
#' Obter chuva diaria pela rota WebService legada
#'
#' Obtem uma serie diaria convencional de chuva pela operacao
#' `HidroSerieHistorica` do WebService XML legado da ANA e retorna o contrato
#' diario padronizado usado pelos leitores offline.
#'
#' @details
#' Esta e uma rota de baixo nivel mantida para compatibilidade interna. Para uso
#' cotidiano, prefira [get_ana_data()] ou os atalhos publicos
#' `get_ana_daily_discharge()`, `get_ana_daily_stage()` e
#' `get_ana_daily_rainfall()`, que usam os argumentos publicos
#' `start_date` e `end_date`.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param date_start Data inicial. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param date_end Data final. Pode ser `Date`, texto `yyyy-mm-dd`, texto
#'   `dd/mm/yyyy` ou `NULL`.
#' @param consistency_level Nivel de consistencia a solicitar, geralmente
#'   `"1"` ou `"2"`.
#' @param timeout Tempo maximo da requisicao, em segundos.
#' @param base_url URL base do WebService XML legado da ANA.
#'
#' @return Um `data.frame`/`tibble` no contrato diario padronizado:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#' @keywords internal
#'
#' @examples
#' if (FALSE) {
#'   dados <- ana_get_daily_rainfall_impl(
#'     "00000000",
#'     date_start = "2020-01-01",
#'     date_end = "2020-01-31"
#'   )
#' }
#' @noRd
ana_get_daily_rainfall_impl <- function(
    station_code,
    date_start = NULL,
    date_end = NULL,
    consistency_level = "2",
    timeout = 60,
    base_url = ana_legacy_base_url()
) {
  get_ana_daily_series(
    station_code = station_code,
    variable = "rainfall",
    date_start = date_start,
    date_end = date_end,
    consistency_level = consistency_level,
    timeout = timeout,
    base_url = base_url
  )
}
#' Obter serie diaria de vazao da ANA
#'
#' Obtem dados diarios de vazao de uma estacao ANA e retorna a serie
#' no contrato diario padronizado do pacote.
#'
#' @details
#' Esta funcao e um atalho para `get_ana_data(product = "daily_discharge")`.
#' O fluxo recomendado para novos usuarios e usar [get_ana_data()] diretamente,
#' especialmente quando for combinar produtos ou usar `product = "all"`.
#'
#' `data_source = "webservice"` usa o WebService XML legado online.
#' `data_source = "api"` usa o HidroWebService autenticado. Os argumentos
#' publicos de data sao `start_date` e `end_date`; os aliases internos
#' `date_start` e `date_end` podem ser aceitos por compatibilidade em `...`, mas
#' nao devem ser preferidos em novos codigos.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Use `"webservice"` para o WebService XML
#'   legado online ou `"api"` para a API autenticada.
#' @param source Alias de `data_source`, mantido para compatibilidade.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, `request_function` em testes ou aliases de data herdados.
#'
#' @return Um `data.frame`/`tibble` com a serie diaria padronizada:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_daily_discharge(
#'     station_code = "00000000",
#'     start_date = "2020-01-01",
#'     end_date = "2020-01-31",
#'     data_source = "webservice"
#'   )
#' }
#' @noRd
get_ana_daily_discharge <- function(station_code, start_date = NULL, end_date = NULL, data_source = "webservice", source = NULL, ...) {
  effective_source <- if (!is.null(source)) source else data_source
  effective_source <- tolower(as.character(effective_source)[1])
  dots <- list(...)  # capture early so date_start/date_end aliases can be consumed safely
  if (is.null(start_date) && "date_start" %in% names(dots)) {
    start_date <- dots$date_start
    dots$date_start <- NULL
  }
  if (is.null(end_date) && "date_end" %in% names(dots)) {
    end_date <- dots$date_end
    dots$date_end <- NULL
  }
  if (identical(effective_source, "api")) {
    return(get_ana_data(
      product = "daily_discharge",
      data_source = "api",
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      ...
    ))
  }
  impl <- ana_get_daily_discharge_impl
  args <- c(
    list(
      station_code = station_code,
      date_start = start_date,
      date_end = end_date
    ),
    dots
  )
  fml <- names(formals(impl))
  if ("data_source" %in% fml) args$data_source <- effective_source
  if ("source" %in% fml) args$source <- effective_source
  # Do not forward NULL date arguments to implementations that allow full-period calls.
  args <- args[!vapply(args, is.null, logical(1))]
  if ("..." %in% fml) {
    do.call(impl, args)
  } else {
    do.call(impl, args[names(args) %in% fml])
  }
}
#' Obter serie diaria de cota da ANA
#'
#' Obtem dados diarios de cota de uma estacao ANA e retorna a serie
#' no contrato diario padronizado do pacote.
#'
#' @details
#' Esta funcao e um atalho para `get_ana_data(product = "daily_stage")`.
#' O fluxo recomendado para novos usuarios e usar [get_ana_data()] diretamente,
#' especialmente quando for combinar produtos ou usar `product = "all"`.
#'
#' `data_source = "webservice"` usa o WebService XML legado online.
#' `data_source = "api"` usa o HidroWebService autenticado. Os argumentos
#' publicos de data sao `start_date` e `end_date`; os aliases internos
#' `date_start` e `date_end` podem ser aceitos por compatibilidade em `...`, mas
#' nao devem ser preferidos em novos codigos.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Use `"webservice"` para o WebService XML
#'   legado online ou `"api"` para a API autenticada.
#' @param source Alias de `data_source`, mantido para compatibilidade.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, `request_function` em testes ou aliases de data herdados.
#'
#' @return Um `data.frame`/`tibble` com a serie diaria padronizada:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_daily_stage(
#'     station_code = "00000000",
#'     start_date = "2020-01-01",
#'     end_date = "2020-01-31",
#'     data_source = "webservice"
#'   )
#' }
#' @noRd
get_ana_daily_stage <- function(station_code, start_date = NULL, end_date = NULL, data_source = "webservice", source = NULL, ...) {
  effective_source <- if (!is.null(source)) source else data_source
  effective_source <- tolower(as.character(effective_source)[1])
  dots <- list(...)  # capture early so date_start/date_end aliases can be consumed safely
  if (is.null(start_date) && "date_start" %in% names(dots)) {
    start_date <- dots$date_start
    dots$date_start <- NULL
  }
  if (is.null(end_date) && "date_end" %in% names(dots)) {
    end_date <- dots$date_end
    dots$date_end <- NULL
  }
  if (identical(effective_source, "api")) {
    return(get_ana_data(
      product = "daily_stage",
      data_source = "api",
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      ...
    ))
  }
  impl <- ana_get_daily_stage_impl
  args <- c(
    list(
      station_code = station_code,
      date_start = start_date,
      date_end = end_date
    ),
    dots
  )
  fml <- names(formals(impl))
  if ("data_source" %in% fml) args$data_source <- effective_source
  if ("source" %in% fml) args$source <- effective_source
  # Do not forward NULL date arguments to implementations that allow full-period calls.
  args <- args[!vapply(args, is.null, logical(1))]
  if ("..." %in% fml) {
    do.call(impl, args)
  } else {
    do.call(impl, args[names(args) %in% fml])
  }
}
#' Obter serie diaria de chuva da ANA
#'
#' Obtem dados diarios de chuva de uma estacao ANA e retorna a serie
#' no contrato diario padronizado do pacote.
#'
#' @details
#' Esta funcao e um atalho para `get_ana_data(product = "daily_rainfall")`.
#' O fluxo recomendado para novos usuarios e usar [get_ana_data()] diretamente,
#' especialmente quando for combinar produtos ou usar `product = "all"`.
#'
#' `data_source = "webservice"` usa o WebService XML legado online.
#' `data_source = "api"` usa o HidroWebService autenticado. Os argumentos
#' publicos de data sao `start_date` e `end_date`; os aliases internos
#' `date_start` e `date_end` podem ser aceitos por compatibilidade em `...`, mas
#' nao devem ser preferidos em novos codigos.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Use `"webservice"` para o WebService XML
#'   legado online ou `"api"` para a API autenticada.
#' @param source Alias de `data_source`, mantido para compatibilidade.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, `request_function` em testes ou aliases de data herdados.
#'
#' @return Um `data.frame`/`tibble` com a serie diaria padronizada:
#'   `station_code`, `date`, `variable`, `value`, `unit`,
#'   `consistency_level`, `source_status` e `source`.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_daily_rainfall(
#'     station_code = "00000000",
#'     start_date = "2020-01-01",
#'     end_date = "2020-01-31",
#'     data_source = "webservice"
#'   )
#' }
#' @noRd
get_ana_daily_rainfall <- function(station_code, start_date = NULL, end_date = NULL, data_source = "webservice", source = NULL, ...) {
  effective_source <- if (!is.null(source)) source else data_source
  effective_source <- tolower(as.character(effective_source)[1])
  dots <- list(...)  # capture early so date_start/date_end aliases can be consumed safely
  if (is.null(start_date) && "date_start" %in% names(dots)) {
    start_date <- dots$date_start
    dots$date_start <- NULL
  }
  if (is.null(end_date) && "date_end" %in% names(dots)) {
    end_date <- dots$date_end
    dots$date_end <- NULL
  }
  if (identical(effective_source, "api")) {
    return(get_ana_data(
      product = "daily_rainfall",
      data_source = "api",
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      ...
    ))
  }
  impl <- ana_get_daily_rainfall_impl
  args <- c(
    list(
      station_code = station_code,
      date_start = start_date,
      date_end = end_date
    ),
    dots
  )
  fml <- names(formals(impl))
  if ("data_source" %in% fml) args$data_source <- effective_source
  if ("source" %in% fml) args$source <- effective_source
  # Do not forward NULL date arguments to implementations that allow full-period calls.
  args <- args[!vapply(args, is.null, logical(1))]
  if ("..." %in% fml) {
    do.call(impl, args)
  } else {
    do.call(impl, args[names(args) %in% fml])
  }
}
