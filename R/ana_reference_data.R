# hydroDataBR reference-data acquisition
# Internal endpoint used by the ANA HidroWebService station inventory route.
ana_stations_endpoint <- function() {
  "/HidroInventarioEstacoes/v1"
}
# Call ana_request_json while remaining compatible with small signature changes.
ana_call_request_json <- function(endpoint, query, token) {
  request_formals <- names(formals(ana_request_json))
  if ("..." %in% request_formals) {
    return(ana_request_json(endpoint = endpoint, query = query, token = token))
  }
  args <- list()
  if ("endpoint" %in% request_formals) {
    args$endpoint <- endpoint
  } else if ("path" %in% request_formals) {
    args$path <- endpoint
  } else if ("url_path" %in% request_formals) {
    args$url_path <- endpoint
  } else if ("route" %in% request_formals) {
    args$route <- endpoint
  } else {
    stop("Could not determine the endpoint argument used by ana_request_json().", call. = FALSE)
  }
  if ("query" %in% request_formals) {
    args$query <- query
  } else if ("params" %in% request_formals) {
    args$params <- query
  } else if ("parameters" %in% request_formals) {
    args$parameters <- query
  } else if (length(query) > 0) {
    stop("Could not determine the query-parameter argument used by ana_request_json().", call. = FALSE)
  }
  if ("token" %in% request_formals) {
    args$token <- token
  } else if ("ana_token" %in% request_formals) {
    args$ana_token <- token
  }
  do.call(ana_request_json, args)
}
ana_extract_items <- function(response) {
  if (is.data.frame(response)) {
    return(response)
  }
  if (is.list(response) && !is.null(response$items)) {
    return(response$items)
  }
  response
}
ana_items_to_records <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(list())
  }
  if (is.data.frame(items)) {
    records <- vector("list", nrow(items))
    for (i in seq_len(nrow(items))) {
      records[[i]] <- as.list(items[i, , drop = FALSE])
    }
    return(records)
  }
  if (is.list(items) && is.null(names(items)) && is.list(items[[1]])) {
    return(lapply(items, as.list))
  }
  list(as.list(items))
}
ana_record_value <- function(record, candidates) {
  if (is.null(record) || length(record) == 0) {
    return(NA_character_)
  }
  record_names <- names(record)
  if (is.null(record_names)) {
    return(NA_character_)
  }
  exact <- match(candidates, record_names, nomatch = 0L)
  exact <- exact[exact > 0L]
  if (length(exact) > 0) {
    value <- record[[exact[[1]]]]
    return(ana_scalar_character(value))
  }
  lower_match <- match(tolower(candidates), tolower(record_names), nomatch = 0L)
  lower_match <- lower_match[lower_match > 0L]
  if (length(lower_match) > 0) {
    value <- record[[lower_match[[1]]]]
    return(ana_scalar_character(value))
  }
  NA_character_
}
ana_scalar_character <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  value <- value[[1]]
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(NA_character_)
  }
  as.character(value)
}
ana_record_column <- function(records, candidates) {
  if (is.null(records)) return(character(0))
  records <- tibble::as_tibble(records, .name_repair = "minimal")
  n <- nrow(records)
  if (!n || !ncol(records) || !length(candidates)) return(rep(NA_character_, n))
  normalize_column_name <- function(x) {
    x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    x <- tolower(x)
    gsub("[^a-z0-9]+", "", x)
  }
  record_names <- normalize_column_name(names(records))
  candidate_names <- normalize_column_name(candidates)
  hit <- match(candidate_names, record_names, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (!length(hit)) return(rep(NA_character_, n))
  out <- records[[hit[1L]]]
  if (is.factor(out)) out <- as.character(out)
  as.character(out)
}
ana_as_number <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
ana_as_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.Date(substr(x, 1, 10)))
}
ana_as_logical_operating <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- rep(NA, length(x))
  out[x %in% c("1", "SIM", "TRUE", "T", "YES", "Y")] <- TRUE
  out[x %in% c("0", "NAO", "N\u00C3O", "FALSE", "F", "NO", "N")] <- FALSE
  out
}
ana_standardize_stations <- function(response) {
  empty_stations <- function() {
    tibble::tibble(
      station_code = character(),
      station_name = character(),
      state_code = character(),
      state_name = character(),
      municipality_code = character(),
      municipality = character(),
      basin_code = character(),
      basin_name = character(),
      station_type = character(),
      operating = logical(),
      latitude = numeric(),
      longitude = numeric(),
      altitude_m = numeric(),
      drainage_area_km2 = numeric(),
      operator_code = character(),
      operator_acronym = character(),
      responsible_acronym = character(),
      last_update = as.Date(character()),
      source = character()
    )
  }
  get_items <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    if (is.list(x)) {
      for (nm in c("items", "Items", "data", "Data", "content", "Content", "resultado", "Resultado", "records", "Records")) {
        if (!is.null(x[[nm]])) return(x[[nm]])
      }
    }
    x
  }
  as_records <- function(x) {
    if (is.null(x)) return(tibble::tibble())
    if (is.data.frame(x)) return(tibble::as_tibble(x, .name_repair = "unique"))
    if (is.list(x) && length(x) > 0L) {
      is_row_list <- all(vapply(x, function(z) is.list(z) || is.data.frame(z), logical(1L)))
      if (is_row_list) {
        rows <- lapply(x, function(z) {
          if (is.data.frame(z)) return(z)
          as.data.frame(z, stringsAsFactors = FALSE, check.names = FALSE)
        })
        return(tibble::as_tibble(dplyr::bind_rows(rows), .name_repair = "unique"))
      }
      return(tibble::as_tibble(x, .name_repair = "unique"))
    }
    tibble::tibble()
  }
  raw <- as_records(get_items(response))
  if (!is.data.frame(raw) || !nrow(raw)) return(empty_stations())
  n <- nrow(raw)
  normalize_key <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    x <- tolower(x)
    gsub("[^a-z0-9]+", "", x, perl = TRUE)
  }
  pick_chr <- function(candidates) {
    if (!ncol(raw)) return(rep(NA_character_, n))
    raw_names <- normalize_key(names(raw))
    candidate_names <- normalize_key(candidates)
    hit <- match(candidate_names, raw_names, nomatch = 0L)
    hit <- hit[hit > 0L]
    if (!length(hit)) return(rep(NA_character_, n))
    out <- as.character(raw[[hit[1L]]])
    if (length(out) == n) return(out)
    rep_len(out, n)
  }
  pick_num <- function(candidates) {
    x <- pick_chr(candidates)
    suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  }
  pick_date <- function(candidates) {
    x <- pick_chr(candidates)
    out <- suppressWarnings(as.Date(x))
    missing <- is.na(out) & !is.na(x) & grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}", x)
    out[missing] <- suppressWarnings(as.Date(x[missing], format = "%d/%m/%Y"))
    out
  }
  pick_logical <- function(candidates) {
    x <- normalize_key(pick_chr(candidates))
    out <- rep(NA, length(x))
    out[x %in% c("true", "t", "1", "sim", "s", "ativo", "ativa", "operando", "emoperacao", "ematividade")] <- TRUE
    out[x %in% c("false", "f", "0", "nao", "n", "inativo", "inativa", "desativado", "desativada", "paralisado")] <- FALSE
    out
  }
  tibble::tibble(
    station_code = pick_chr(c("station_code", "codigoestacao", "codigo estacao", "codigo da estacao", "codigo_estacao", "Codigo_Estacao", "CodigoEstacao", "CodEstacao", "codigo")),
    station_name = pick_chr(c("station_name", "estacao_nome", "estacao nome", "Estacao_Nome", "nome_estacao", "nome estacao", "nome da estacao", "Nome_Estacao", "NomeEstacao", "nomeestacao", "nomeposto", "NomePosto", "posto", "Posto", "estacao", "Estacao", "nome", "Nome")),
    state_code = pick_chr(c("state_code", "ufestacao", "UF_Estacao", "uf_estacao", "UF", "uf", "estado_codigo", "Estado_Codigo", "UF_Codigo", "unidadefederativa")),
    state_name = pick_chr(c("state_name", "uf_nome_estacao", "UF_Nome_Estacao", "uf_nome", "UF_Nome", "nomeuf", "estado", "estado_nome", "Estado_Nome", "nomeestado")),
    municipality_code = pick_chr(c("municipality_code", "codigomunicipio", "codigo municipio", "codmunicipio", "municipio_codigo", "CodigoMunicipio")),
    municipality = pick_chr(c("municipality", "municipio_nome", "Municipio_Nome", "nome_municipio", "Nome_Municipio", "nomemunicipio", "municipio", "Municipio", "cidade")),
    basin_code = pick_chr(c("basin_code", "codigobacia", "codigo bacia", "codigo da bacia", "codbacia", "CodigoBacia")),
    basin_name = pick_chr(c("basin_name", "bacia_nome", "Bacia_Nome", "nome_bacia", "Nome_Bacia", "nomebacia", "bacia", "Bacia")),
    station_type = pick_chr(c("station_type", "tipoestacao", "tipo estacao", "tipo da estacao", "tipo_estacao", "TipoEstacao", "tipo", "Tipo")),
    operating = pick_logical(c("operating", "is_operating", "operando", "emoperacao", "em operacao", "situacao", "Situacao", "status", "Status")),
    latitude = pick_num(c("latitude", "Latitude", "lat")),
    longitude = pick_num(c("longitude", "Longitude", "long", "lon")),
    altitude_m = pick_num(c("altitude_m", "altitude", "Altitude", "altitudem")),
    drainage_area_km2 = pick_num(c("drainage_area_km2", "areadrenagemkm2", "area de drenagem", "areadrenagem", "AreaDrenagem", "Area_Drenagem")),
    operator_code = pick_chr(c("operator_code", "codigooperador", "codigooperadora", "codoperador", "operador_codigo")),
    operator_acronym = pick_chr(c("operator_acronym", "siglaoperador", "siglaoperadora", "operador", "operadora", "Operadora")),
    responsible_acronym = pick_chr(c("responsible_acronym", "siglaresponsavel", "responsavel", "Responsavel", "responsibleagency")),
    last_update = pick_date(c("last_update", "dataatualizacao", "data atualizacao", "dataultimaatualizacao", "ultimaatualizacao", "dataalteracao", "DataAtualizacao")),
    source = rep("ana_hidrowebservice_inventory", n)
  )
}
ana_compact_query <- function(query) {
  query[!vapply(query, function(x) is.null(x) || length(x) == 0 || is.na(x) || identical(x, ""), logical(1))]
}
#' Obter inventario de estacoes da ANA
#'
#' Consulta o inventario de estacoes no HidroWebService da ANA e retorna uma
#' tabela padronizada com campos essenciais de identificacao, localizacao e
#' classificacao das estacoes.
#'
#' Pelo menos um filtro deve ser informado: `station_code`, `state_code` ou
#' `basin_code`.
#'
#' @param token Objeto de token retornado por [ana_authenticate()].
#' @param station_code Codigo da estacao. Mantido como texto.
#' @param state_code Sigla da unidade federativa, como `MG` ou `SP`.
#' @param basin_code Codigo da bacia hidrografica usado pelo HidroWebService.
#' @param updated_from Data inicial de atualizacao no formato `YYYY-MM-DD`.
#' @param updated_to Data final de atualizacao no formato `YYYY-MM-DD`.
#'
#' @return Um tibble com campos padronizados de inventario de estacoes.
#' @keywords internal
ana_get_stations_impl <- function(token = NULL,
                             station_code = NULL,
                             state_code = NULL,
                             basin_code = NULL,
                             updated_from = NULL,
                             updated_to = NULL) {
  has_required_filter <- any(!vapply(
    list(station_code, state_code, basin_code),
    function(x) is.null(x) || length(x) == 0 || is.na(x) || identical(x, ""),
    logical(1)
  ))
  if (!has_required_filter) {
    stop(
      "At least one of station_code, state_code, or basin_code must be supplied.",
      call. = FALSE
    )
  }
  if (!is.null(state_code)) {
    state_code <- toupper(state_code)
  }
  query <- ana_compact_query(list(
    CodigoDaEstacao = station_code,
    DataAtualizacaoInicial = updated_from,
    DataAtualizacaoFinal = updated_to,
    UnidadeFederativa = state_code,
    CodigoDaBacia = basin_code
  ))
  response <- ana_call_request_json(
    endpoint = ana_stations_endpoint(),
    query = query,
    token = token
  )
  ana_standardize_stations(response)
}
#' Consultar o inventário de estações da ANA
#'
#' Consulta o inventário de estações pelo serviço autenticado da ANA e retorna
#' metadados padronizados dos postos encontrados. A função pode ser usada para
#' buscar uma estação específica, estações de uma unidade federativa ou estações
#' associadas a uma bacia.
#'
#' @param station_code Código(s) de estação ANA, como texto ou número.
#' @param state_code Sigla(s) de unidade federativa, como `"MG"` ou `"SP"`.
#' @param basin_code Código(s) de bacia.
#' @param token Token retornado por `ana_authenticate()`. Se `NULL`, a função
#'   tenta autenticar com credenciais disponíveis nos argumentos ou no ambiente.
#' @param ... Argumentos adicionais usados internamente, como `timeout`,
#'   `max_attempts`, `retry_sleep_seconds` e opções de teste.
#' @param data_source Fonte dos dados. Na prática, esta função usa a rota
#'   autenticada de inventário da ANA.
#' @param source Alias de compatibilidade para `data_source`.
#'
#' @return `data.frame` com metadados padronizados das estações retornadas pela
#'   ANA.
#' @export
#'
#' @examples
#' # Exemplo operacional. Requer credenciais validas da ANA.
#' if (FALSE) {
#'   token <- ana_authenticate()
#'   estacoes <- get_ana_stations(state_code = "MG", token = token)
#'   head(estacoes)
#' }
get_ana_stations <- function(station_code = NULL, state_code = NULL, basin_code = NULL, token = NULL, ..., data_source = "api", source = data_source) {
  requested_source <- source
  if (is.null(requested_source) || !length(requested_source)) requested_source <- data_source
  requested_source <- tolower(as.character(requested_source)[[1]])
  if (is.na(requested_source) || !nzchar(requested_source)) requested_source <- "api"
  allowed_sources <- c("api", "hidrowebservice", "webservice", "stations", "inventory")
  if (!requested_source %in% allowed_sources) {
    stop("get_ana_stations() supports the ANA HidroWebService inventory route.", call. = FALSE)
  }
  station_code <- if (is.null(station_code)) character(0) else unique(as.character(station_code))
  state_code <- if (is.null(state_code)) character(0) else unique(toupper(as.character(state_code)))
  basin_code <- if (is.null(basin_code)) character(0) else unique(as.character(basin_code))
  station_code <- station_code[!is.na(station_code) & nzchar(station_code)]
  state_code <- state_code[!is.na(state_code) & nzchar(state_code)]
  basin_code <- basin_code[!is.na(basin_code) & nzchar(basin_code)]
  if (!length(station_code) && !length(state_code) && !length(basin_code)) {
    stop("At least one of station_code, state_code, or basin_code must be supplied.", call. = FALSE)
  }
  dots <- list(...) 
  request_function <- ana_request_json
  if ("request_function" %in% names(dots) && is.function(dots$request_function)) {
    request_function <- dots$request_function
    dots$request_function <- NULL
  }
  timeout <- 60
  if ("timeout" %in% names(dots) && length(dots$timeout)) timeout <- dots$timeout[[1]]
  endpoint <- ana_stations_endpoint()
  if (is.null(token) && identical(request_function, ana_request_json)) token <- ana_authenticate()
  queries <- list()
  if (length(station_code)) {
    station_query_name <- "C\u00f3digo da Esta\u00e7\u00e3o"
    for (code in station_code) {
      queries[[length(queries) + 1L]] <- stats::setNames(list(code), station_query_name)
    }
  } else if (length(state_code)) {
    for (code in state_code) {
      queries[[length(queries) + 1L]] <- list(UF = code)
    }
  } else {
    basin_query_name <- "C\u00f3digo da Bacia"
    for (code in basin_code) {
      queries[[length(queries) + 1L]] <- stats::setNames(list(code), basin_query_name)
    }
  }
  call_request <- function(query) {
    args <- list(endpoint = endpoint, query = query, token = token)
    request_formals <- names(formals(request_function))
    if ("timeout" %in% request_formals || "..." %in% request_formals) args$timeout <- timeout
    if (!"..." %in% request_formals) args <- args[names(args) %in% request_formals]
    do.call(request_function, args)
  }
  errors <- character(0)
  collected <- list()
  for (query in queries) {
    response <- tryCatch(call_request(query), error = function(e) e)
    if (inherits(response, "error")) {
      errors <- c(errors, conditionMessage(response))
      next
    }
    stations <- ana_standardize_stations(response)
    if (is.data.frame(stations) && nrow(stations)) collected[[length(collected) + 1L]] <- stations
  }
  if (!length(collected)) {
    if (length(errors)) stop(errors[[1]], call. = FALSE)
    return(ana_standardize_stations(tibble::tibble()))
  }
  result <- dplyr::bind_rows(collected)
  if (length(station_code) && "station_code" %in% names(result)) {
    result <- result[result$station_code %in% station_code, , drop = FALSE]
  }
  if ("station_code" %in% names(result)) {
    result <- dplyr::distinct(result, .data$station_code, .keep_all = TRUE)
  } else {
    result <- dplyr::distinct(result)
  }
  tibble::as_tibble(result)
}
# This block avoids infix helper definitions so the file remains parse-safe.
