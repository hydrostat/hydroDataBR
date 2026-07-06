# Reference-table acquisition for ANA HidroWebService.
# Code comments are in English by project convention.
ana_reference_source <- function() {
  "ana_hidrowebservice"
}
ana_reference_live_endpoint_stage05B <- function(product, endpoint) {
  if (is.null(endpoint) || length(endpoint) == 0 || is.na(endpoint)) {
    endpoint <- ""
  }
  endpoint <- as.character(endpoint[[1]])
  if (startsWith(endpoint, "/Hidro")) {
    return(endpoint)
  }
  switch(
    product,
    states = "/HidroUF/v1",
    municipalities = "/HidroMunicipio/v1",
    basins = "/HidroBacia/v1",
    subbasins = "/HidroSubBacia/v1",
    rivers = "/HidroRio/v1",
    entities = "/HidroEntidade/v1",
    endpoint
  )
}
ana_reference_request_stage05B <- function(product, endpoint, token, query = list(), request_function = ana_request_json) {
  effective_endpoint <- endpoint
  if (identical(request_function, ana_request_json)) {
    effective_endpoint <- ana_reference_live_endpoint_stage05B(product, endpoint)
  }
  request_formals <- names(formals(request_function))
  has_dots <- "..." %in% request_formals
  request_args <- list()
  path_arg <- intersect(
    c("endpoint", "path", "route", "resource", "url"),
    request_formals
  )
  if (length(path_arg) > 0) {
    request_args[[path_arg[[1]]]] <- effective_endpoint
  } else {
    request_args[[1]] <- effective_endpoint
  }
  if ("token" %in% request_formals || has_dots) {
    request_args$token <- token
  }
  # Keep Stage 05A mocks observable, but avoid sending package-level
  # generic filter names to live routes whose query contracts were not
  # part of Stage 05B validation. Filtering is performed locally below.
  query_to_send <- query
  if (identical(request_function, ana_request_json)) {
    query_to_send <- list()
  }
  if ("query" %in% request_formals || has_dots) {
    request_args$query <- query_to_send
  }
  if ("params" %in% request_formals) {
    request_args$params <- query_to_send
  }
  do.call(request_function, request_args)
}
ana_reference_items_stage05B <- function(response) {
  if (is.data.frame(response)) {
    return(tibble::as_tibble(response))
  }
  items <- response
  container_names <- c(
    "items", "data", "Dados", "dados", "results", "Results",
    "value", "Value", "content", "Content",
    "Estados", "Estado", "UFs", "Ufs", "UF",
    "Municipios", "Municipio",
    "Bacias", "Bacia",
    "SubBacias", "Sub_Bacias", "SubBacia",
    "Rios", "Rio",
    "Entidades", "Entidade"
  )
  if (is.list(response)) {
    matched_container <- intersect(container_names, names(response))
    if (length(matched_container) > 0) {
      items <- response[[matched_container[[1]]]]
    }
  }
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble())
  }
  if (is.data.frame(items)) {
    return(tibble::as_tibble(items))
  }
  if (!is.list(items)) {
    return(tibble::tibble())
  }
  if (!is.null(names(items)) && all(vapply(items, function(x) length(x) == 1 && !is.list(x), logical(1)))) {
    items <- list(items)
  }
  item_names <- unique(unlist(lapply(items, names), use.names = FALSE))
  item_names <- item_names[!is.na(item_names) & nzchar(item_names)]
  if (length(item_names) == 0) {
    return(tibble::tibble())
  }
  rows <- lapply(items, function(item) {
    row <- as.list(stats::setNames(rep(NA, length(item_names)), item_names))
    if (is.list(item)) {
      for (name in intersect(names(item), item_names)) {
        value <- item[[name]]
        if (length(value) == 0) {
          row[[name]] <- NA
        } else if (length(value) == 1 && !is.list(value)) {
          row[[name]] <- value
        } else {
          row[[name]] <- NA
        }
      }
    }
    row
  })
  tibble::as_tibble(do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE)))
}
ana_reference_normalize_name_stage05B <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", x))
}
ana_reference_chr_stage05B <- function(data, candidates) {
  if (nrow(data) == 0) {
    return(character())
  }
  data_names <- names(data)
  normalized_data_names <- ana_reference_normalize_name_stage05B(data_names)
  for (candidate in candidates) {
    candidate_index <- which(data_names == candidate)
    if (length(candidate_index) == 0) {
      candidate_index <- which(normalized_data_names == ana_reference_normalize_name_stage05B(candidate))
    }
    if (length(candidate_index) > 0) {
      values <- as.character(data[[candidate_index[[1]]]])
      if (!all(is.na(values))) {
        return(values)
      }
    }
  }
  rep(NA_character_, nrow(data))
}
ana_reference_match_name_stage05B <- function(code, lookup_code, lookup_name) {
  out <- lookup_name[match(code, lookup_code)]
  out[is.na(out)] <- NA_character_
  out
}
ana_reference_filter_code_stage05B <- function(data, column, values) {
  if (is.null(values)) {
    return(data)
  }
  keep <- as.character(data[[column]]) %in% as.character(values)
  tibble::as_tibble(data[keep, , drop = FALSE])
}
get_ana_basins <- function(token, endpoint = "Bacias", request_function = ana_request_json) {
  response <- ana_reference_request_stage05B(
    product = "basins",
    endpoint = endpoint,
    token = token,
    request_function = request_function
  )
  items <- ana_reference_items_stage05B(response)
  tibble::tibble(
    basin_code = ana_reference_chr_stage05B(
      items,
      c(
        "basin_code", "codigobacia", "Bacia_Codigo", "CodigoBacia",
        "Codigo_Bacia", "CodBacia", "Cod_Bacia", "Codigo",
        "codigo", "code"
      )
    ),
    basin_name = ana_reference_chr_stage05B(
      items,
      c(
        "basin_name", "Nome_Bacia", "Bacia_Nome", "NomeBacia",
        "Nome", "nome", "name"
      )
    ),
    source = ana_reference_source()
  )
}
get_ana_subbasins <- function(token, basin_code = NULL, endpoint = "SubBacias", request_function = ana_request_json) {
  query <- list()
  if (!is.null(basin_code)) {
    query$basin_code <- basin_code
  }
  response <- ana_reference_request_stage05B(
    product = "subbasins",
    endpoint = endpoint,
    token = token,
    query = query,
    request_function = request_function
  )
  items <- ana_reference_items_stage05B(response)
  out_basin_code <- ana_reference_chr_stage05B(
    items,
    c(
      "basin_code", "Bacia_Codigo", "codigobacia", "CodigoBacia",
      "Codigo_Bacia", "CodBacia", "Cod_Bacia"
    )
  )
  out_basin_name <- ana_reference_chr_stage05B(
    items,
    c("basin_name", "Bacia_Nome", "Nome_Bacia", "NomeBacia")
  )
  if (all(is.na(out_basin_name)) && identical(request_function, ana_request_json)) {
    basins <- get_ana_basins(token = token)
    out_basin_name <- ana_reference_match_name_stage05B(
      out_basin_code,
      basins$basin_code,
      basins$basin_name
    )
  }
  out <- tibble::tibble(
    subbasin_code = ana_reference_chr_stage05B(
      items,
      c(
        "subbasin_code", "codigosubbacia", "Sub_Bacia_Codigo",
        "SubBacia_Codigo", "CodigoSubBacia", "Codigo_SubBacia",
        "CodSubBacia", "Cod_SubBacia", "Codigo", "codigo", "code"
      )
    ),
    subbasin_name = ana_reference_chr_stage05B(
      items,
      c(
        "subbasin_name", "Sub_Bacia_Nome", "Nome_Sub_Bacia",
        "SubBacia_Nome", "NomeSubBacia", "Nome", "nome", "name"
      )
    ),
    basin_code = out_basin_code,
    basin_name = out_basin_name,
    source = ana_reference_source()
  )
  ana_reference_filter_code_stage05B(out, "basin_code", basin_code)
}
get_ana_municipalities <- function(token, state_code = NULL, endpoint = "Municipios", request_function = ana_request_json) {
  query <- list()
  if (!is.null(state_code)) {
    query$state_code <- state_code
  }
  response <- ana_reference_request_stage05B(
    product = "municipalities",
    endpoint = endpoint,
    token = token,
    query = query,
    request_function = request_function
  )
  items <- ana_reference_items_stage05B(response)
  out_state_code <- ana_reference_chr_stage05B(
    items,
    c(
      "state_code", "Estado_Codigo", "codigouf", "Estado_Codigo_IBGE",
      "Estado_Sigla", "UF", "Uf", "uf", "SiglaUF", "Sigla_UF"
    )
  )
  out_state_name <- ana_reference_chr_stage05B(
    items,
    c("state_name", "Estado_Nome", "Nome_Estado", "NomeEstado")
  )
  if (all(is.na(out_state_name)) && identical(request_function, ana_request_json)) {
    states <- get_ana_states(token = token)
    out_state_name <- ana_reference_match_name_stage05B(
      out_state_code,
      states$state_code,
      states$state_name
    )
  }
  out <- tibble::tibble(
    municipality_code = ana_reference_chr_stage05B(
      items,
      c(
        "municipality_code", "codigomunicipio", "Municipio_Codigo_IBGE",
        "Municipio_Codigo", "CodigoMunicipio", "Codigo_Municipio",
        "CodMunicipio", "Cod_Municipio", "Codigo", "codigo", "code"
      )
    ),
    municipality = ana_reference_chr_stage05B(
      items,
      c(
        "municipality", "Municipio_Nome", "Nome_Municipio",
        "NomeMunicipio", "Nome", "nome", "name"
      )
    ),
    state_code = out_state_code,
    state_name = out_state_name,
    source = ana_reference_source()
  )
  ana_reference_filter_code_stage05B(out, "state_code", state_code)
}
get_ana_rivers <- function(token, endpoint = "Rios", request_function = ana_request_json) {
  response <- ana_reference_request_stage05B(
    product = "rivers",
    endpoint = endpoint,
    token = token,
    request_function = request_function
  )
  items <- ana_reference_items_stage05B(response)
  tibble::tibble(
    river_code = ana_reference_chr_stage05B(
      items,
      c(
        "river_code", "codigorio", "Rio_Codigo", "CodigoRio",
        "Codigo_Rio", "CodRio", "Cod_Rio", "Codigo",
        "codigo", "code"
      )
    ),
    river_name = ana_reference_chr_stage05B(
      items,
      c("river_name", "Nome_Rio", "Rio_Nome", "NomeRio", "Nome", "nome", "name")
    ),
    source = ana_reference_source()
  )
}
get_ana_entities <- function(token, endpoint = "Entidades", request_function = ana_request_json) {
  response <- ana_reference_request_stage05B(
    product = "entities",
    endpoint = endpoint,
    token = token,
    request_function = request_function
  )
  items <- ana_reference_items_stage05B(response)
  tibble::tibble(
    entity_code = ana_reference_chr_stage05B(
      items,
      c(
        "entity_code", "codigoentidade", "Entidade_Codigo",
        "CodigoEntidade", "Codigo_Entidade", "CodEntidade",
        "Cod_Entidade", "Codigo", "codigo", "code"
      )
    ),
    entity_name = ana_reference_chr_stage05B(
      items,
      c(
        "entity_name", "Entidade_Nome", "Nome_Entidade",
        "NomeEntidade", "Nome", "nome", "name"
      )
    ),
    entity_acronym = ana_reference_chr_stage05B(
      items,
      c(
        "entity_acronym", "Entidade_Sigla", "Sigla_Entidade",
        "SiglaEntidade", "Acronimo", "Acronym", "Sigla",
        "sigla", "acronym"
      )
    ),
    source = ana_reference_source()
  )
}
# State-code aliases used by legacy reference contracts
# This minimal override keeps Stage 05B live routes and restores Stage 05A
# mocked-state compatibility when state abbreviations use alternative names.
ana_reference_state_abbreviation_stage05B <- function(state_name) {
  if (length(state_name) == 0) {
    return(character())
  }
  normalized <- iconv(as.character(state_name), from = "", to = "ASCII//TRANSLIT")
  normalized <- tolower(gsub("[^A-Za-z ]", "", normalized))
  normalized <- trimws(gsub("[[:space:]]+", " ", normalized))
  lookup <- c(
    acre = "AC",
    alagoas = "AL",
    amapa = "AP",
    amazonas = "AM",
    bahia = "BA",
    ceara = "CE",
    "distrito federal" = "DF",
    "espirito santo" = "ES",
    goias = "GO",
    maranhao = "MA",
    "mato grosso" = "MT",
    "mato grosso do sul" = "MS",
    "minas gerais" = "MG",
    para = "PA",
    paraiba = "PB",
    parana = "PR",
    pernambuco = "PE",
    piaui = "PI",
    "rio de janeiro" = "RJ",
    "rio grande do norte" = "RN",
    "rio grande do sul" = "RS",
    rondonia = "RO",
    roraima = "RR",
    "santa catarina" = "SC",
    "sao paulo" = "SP",
    sergipe = "SE",
    tocantins = "TO"
  )
  out <- unname(lookup[normalized])
  out[is.na(out)] <- NA_character_
  out
}
ana_standardize_states <- function(x) {
  items <- ana_reference_items_stage05B(x)
  state_name <- ana_reference_chr_stage05B(
    items,
    c(
      "state_name", "Estado_Nome", "Nome_Estado", "NomeEstado",
      "NomeUF", "Nome_UF", "UF_Nome", "Nome", "nome", "name"
    )
  )
  state_code <- ana_reference_chr_stage05B(
    items,
    c(
      "state_code", "state_abbreviation", "state_acronym",
      "state_uf", "uf_code", "uf_acronym",
      "Estado_Sigla", "Sigla_Estado", "SiglaEstado",
      "UF", "Uf", "uf", "SG_UF", "sg_uf",
      "SiglaUF", "Sigla_UF", "UF_Sigla",
      "Sigla", "sigla", "abbrev", "abbreviation", "acronym",
      "codigouf", "CodigoUF", "Codigo_UF",
      "Estado_Codigo", "Estado_Codigo_IBGE", "Codigo", "codigo", "code"
    )
  )
  needs_name_lookup <- is.na(state_code) | !grepl("^[A-Za-z]{2}$", state_code)
  if (any(needs_name_lookup)) {
    state_code_from_name <- ana_reference_state_abbreviation_stage05B(state_name)
    state_code[needs_name_lookup] <- state_code_from_name[needs_name_lookup]
  }
  state_code <- toupper(state_code)
  tibble::tibble(
    state_code = state_code,
    state_name = state_name,
    source = ana_reference_source()
  )
}
get_ana_states <- function(token, endpoint = "Estados", request_function = ana_request_json) {
  response <- ana_reference_request_stage05B(
    product = "states",
    endpoint = endpoint,
    token = token,
    request_function = request_function
  )
  ana_standardize_states(response)
}
