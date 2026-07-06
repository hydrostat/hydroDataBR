# ANA acquisition dispatch and authenticated product-all planning.
# Code comments are in English by project convention.
ana_api_is_all_product <- function(product) {
  length(product) == 1L && !is.na(product) &&
    tolower(trimws(as.character(product))) %in% c("all", "todos", "tudo")
}
ana_is_all_product <- ana_api_is_all_product
ana_api_is_api_source <- function(data_source) {
  length(data_source) == 1L && !is.na(data_source) &&
    tolower(trimws(as.character(data_source))) %in% c("api", "authenticated_api", "ana_api")
}
ana_normalize_product_name <- function(product) {
  if (length(product) != 1L || is.na(product)) {
    stop("`product` must be a single non-missing value.", call. = FALSE)
  }
  product <- tolower(trimws(as.character(product)))
  product <- gsub("[-. ]+", "_", product)
  aliases <- c(
    discharge = "daily_discharge",
    daily_flow = "daily_discharge",
    flow = "daily_discharge",
    vazao = "daily_discharge",
    daily_discharge = "daily_discharge",
    cota = "daily_stage",
    level = "daily_stage",
    stage = "daily_stage",
    daily_level = "daily_stage",
    daily_stage = "daily_stage",
    chuva = "daily_rainfall",
    precipitation = "daily_rainfall",
    rainfall = "daily_rainfall",
    daily_precipitation = "daily_rainfall",
    daily_rainfall = "daily_rainfall",
    daily_series = "daily_series",
    daily_data = "daily_series",
    station = "stations",
    stations = "stations",
    inventory = "stations",
    station_inventory = "stations",
    measurement = "discharge_measurements",
    measurements = "discharge_measurements",
    discharge_measurement = "discharge_measurements",
    discharge_measurements = "discharge_measurements",
    rating_curve = "rating_curves",
    rating_curves = "rating_curves",
    curve = "rating_curves",
    curves = "rating_curves",
    cross_section = "cross_sections",
    cross_sections = "cross_sections",
    section = "cross_sections",
    sections = "cross_sections",
    state = "states",
    states = "states",
    uf = "states",
    ufs = "states",
    municipality = "municipalities",
    municipalities = "municipalities",
    city = "municipalities",
    cities = "municipalities",
    basin = "basins",
    basins = "basins",
    subbasin = "subbasins",
    subbasins = "subbasins",
    river = "rivers",
    rivers = "rivers",
    entity = "entities",
    entities = "entities",
    operator = "entities",
    operators = "entities"
  )
  if (!product %in% names(aliases)) {
    stop("Unsupported product: ", product, call. = FALSE)
  }
  unname(aliases[[product]])
}
ana_is_product_alias <- function(product) {
  out <- tryCatch({
    ana_normalize_product_name(product)
    TRUE
  }, error = function(e) FALSE)
  isTRUE(out)
}
ana_normalize_data_source_name <- function(data_source) {
  if (length(data_source) != 1L || is.na(data_source)) {
    stop("`data_source` must be a single non-missing value.", call. = FALSE)
  }
  data_source <- tolower(trimws(as.character(data_source)))
  data_source <- gsub("[-. ]+", "_", data_source)
  aliases <- c(
    webservice = "webservice",
    hidrowebservice = "webservice",
    ana_webservice = "webservice",
    ws = "webservice",
    service = "webservice",
    online = "webservice",
    hidroweb = "hidroweb",
    file = "hidroweb",
    local_file = "hidroweb",
    csv = "hidroweb",
    zip = "hidroweb",
    csv_zip = "hidroweb",
    xml = "xml",
    ana_xml = "xml",
    legacy_xml = "xml",
    json = "json",
    api_json = "json",
    ana_json = "json",
    local_json = "json"
  )
  if (!data_source %in% names(aliases)) {
    stop("Unsupported ANA data source: ", data_source, call. = FALSE)
  }
  unname(aliases[[data_source]])
}
ana_normalize_station_code_vector <- function(station_code) {
  if (is.null(station_code)) return(character())
  station_code <- trimws(as.character(station_code))
  station_code <- station_code[!is.na(station_code) & nzchar(station_code)]
  unique(station_code)
}
ana_bind_rows <- function(items) {
  items <- items[!vapply(items, is.null, logical(1L))]
  if (!length(items)) return(tibble::tibble())
  dplyr::bind_rows(items)
}
ana_count_records <- function(x) {
  if (is.data.frame(x)) return(nrow(x))
  if (is.list(x) && is.data.frame(x$sections) && is.data.frame(x$vertices)) {
    return(nrow(x$sections) + nrow(x$vertices))
  }
  if (is.list(x)) {
    return(sum(vapply(x, function(z) if (is.data.frame(z)) nrow(z) else 0L, integer(1L))))
  }
  0L
}
ana_api_result_rows <- ana_count_records
ana_api_n_rows <- ana_count_records
ana_filter_supported_args <- function(fun, args) {
  formal_names <- names(formals(fun))
  if (is.null(formal_names) || is.null(names(args))) return(args)
  arg_names <- names(args)
  unnamed <- !nzchar(arg_names)
  explicit_token <- "token" %in% formal_names
  is_token <- arg_names == "token"
  if ("..." %in% formal_names) {
    args[unnamed | !is_token | explicit_token]
  } else {
    args[unnamed | arg_names %in% formal_names]
  }
}
ana_call_station_product <- function(product_or_fun, station_code, ...) {
  fun <- if (is.function(product_or_fun)) product_or_fun else ana_station_product_function(product_or_fun)
  dots <- list(...)
  if (length(dots)) dots <- dots[!names(dots) %in% c("station_code", "station_codes")]
  attempts <- list(
    c(list(station_code = as.character(station_code)), dots),
    c(list(station_codes = as.character(station_code)), dots),
    c(list(as.character(station_code)), dots),
    dots
  )
  errors <- character()
  for (args in attempts) {
    args <- ana_filter_supported_args(fun, args)
    result <- try(do.call(fun, args), silent = TRUE)
    if (!inherits(result, "try-error")) return(result)
    errors <- c(errors, conditionMessage(attr(result, "condition")))
  }
  stop(errors[[1L]], call. = FALSE)
}
ana_station_product_function <- function(product) {
  product <- ana_normalize_product_name(product)
  switch(
    product,
    daily_discharge = get_ana_daily_discharge,
    daily_stage = get_ana_daily_stage,
    daily_rainfall = get_ana_daily_rainfall,
    stations = get_ana_stations,
    discharge_measurements = get_ana_discharge_measurements,
    rating_curves = get_ana_rating_curves,
    cross_sections = get_ana_cross_sections,
    stop("Unsupported product: ", product, call. = FALSE)
  )
}
ana_get_local_data <- function(product, data_source, path, station_code = NULL, ...) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` is required for local data sources.", call. = FALSE)
  }
  if (!product %in% c("daily_discharge", "daily_stage", "daily_rainfall", "daily_series")) {
    stop("The selected product is not available for this local data source.", call. = FALSE)
  }
  data <- switch(
    data_source,
    hidroweb = read_hidroweb(path, ...),
    xml = read_ana_xml(path, ...),
    json = read_ana_json(path, ...),
    stop("Unsupported ANA data source: ", data_source, call. = FALSE)
  )
  station_code <- ana_normalize_station_code_vector(station_code)
  if (length(station_code) && "station_code" %in% names(data)) {
    data <- data[data$station_code %in% station_code, , drop = FALSE]
  }
  variable <- switch(
    product,
    daily_discharge = "discharge",
    daily_stage = "stage",
    daily_rainfall = "rainfall",
    daily_series = NA_character_
  )
  if (!is.na(variable) && "variable" %in% names(data)) {
    data <- data[data$variable == variable, , drop = FALSE]
  }
  data
}
ana_require_no_station_code <- function(product, station_code) {
  if (length(station_code)) {
    stop("`station_code` must be NULL for product = '", product, "'.", call. = FALSE)
  }
}
ana_get_webservice_data_single <- function(product, station_code = character(), ...) {
  switch(
    product,
    daily_discharge = ana_call_station_product(get_ana_daily_discharge, station_code, ...),
    daily_stage = ana_call_station_product(get_ana_daily_stage, station_code, ...),
    daily_rainfall = ana_call_station_product(get_ana_daily_rainfall, station_code, ...),
    stations = ana_call_station_product(get_ana_stations, station_code, ...),
    discharge_measurements = ana_call_station_product(get_ana_discharge_measurements, station_code, ...),
    rating_curves = ana_call_station_product(get_ana_rating_curves, station_code, ...),
    cross_sections = ana_call_station_product(get_ana_cross_sections, station_code, ...),
    states = {
      ana_require_no_station_code(product, station_code)
      get_ana_states(...)
    },
    municipalities = {
      ana_require_no_station_code(product, station_code)
      get_ana_municipalities(...)
    },
    basins = {
      ana_require_no_station_code(product, station_code)
      get_ana_basins(...)
    },
    subbasins = {
      ana_require_no_station_code(product, station_code)
      get_ana_subbasins(...)
    },
    rivers = {
      ana_require_no_station_code(product, station_code)
      get_ana_rivers(...)
    },
    entities = {
      ana_require_no_station_code(product, station_code)
      get_ana_entities(...)
    },
    stop("Unsupported product: ", product, call. = FALSE)
  )
}
ana_get_webservice_data_batch_direct <- function(product, station_code, ...) {
  station_code <- ana_normalize_station_code_vector(station_code)
  if (!length(station_code)) {
    stop("`station_code` must contain at least one station code for batch acquisition.", call. = FALSE)
  }
  fun <- ana_station_product_function(product)
  data_items <- vector("list", length(station_code))
  section_items <- vector("list", length(station_code))
  vertex_items <- vector("list", length(station_code))
  report_items <- vector("list", length(station_code))
  for (i in seq_along(station_code)) {
    code <- station_code[[i]]
    result <- tryCatch(ana_call_station_product(fun, code, ...), error = function(e) e)
    if (inherits(result, "error")) {
      report_items[[i]] <- tibble::tibble(
        station_code = code,
        status = "error",
        success = FALSE,
        n_records = 0L,
        n_rows = 0L,
        error_message = conditionMessage(result)
      )
      next
    }
    n_records <- ana_count_records(result)
    report_items[[i]] <- tibble::tibble(
      station_code = code,
      status = "success",
      success = TRUE,
      n_records = n_records,
      n_rows = n_records,
      error_message = NA_character_
    )
    if (identical(product, "cross_sections") && is.list(result)) {
      section_items[[i]] <- result$sections
      vertex_items[[i]] <- result$vertices
    } else {
      data_items[[i]] <- result
    }
  }
  data <- if (identical(product, "cross_sections")) {
    list(sections = ana_bind_rows(section_items), vertices = ana_bind_rows(vertex_items))
  } else {
    ana_bind_rows(data_items)
  }
  list(data = data, request_report = ana_bind_rows(report_items))
}
ana_get_webservice_data_batch <- function(product, station_code, ...) {
  ana_get_webservice_data_batch_direct(product = product, station_code = station_code, ...)
}
ana_api_empty_product <- function(product) {
  switch(
    product,
    daily_discharge = ana_empty_daily_series(),
    daily_stage = ana_empty_daily_series(),
    daily_rainfall = ana_empty_daily_series(),
    discharge_measurements = ana_empty_discharge_measurements(),
    rating_curves = ana_empty_rating_curves(),
    cross_sections = ana_empty_cross_sections(),
    tibble::tibble()
  )
}
ana_api_as_date <- function(x) {
  if (is.null(x) || length(x) == 0L) return(as.Date(NA))
  if (inherits(x, "Date")) return(as.Date(x)[1L])
  x <- trimws(as.character(x)[1L])
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", x)) return(as.Date(substr(x, 1L, 10L)))
  if (grepl("^\\d{2}/\\d{2}/\\d{4}", x)) return(as.Date(substr(x, 1L, 10L), format = "%d/%m/%Y"))
  suppressWarnings(as.Date(x))
}
ana_api_station_inventory <- function(station_inventory = NULL) {
  if (!is.null(station_inventory)) return(as.data.frame(station_inventory, stringsAsFactors = FALSE))
  candidates <- list()
  ns <- tryCatch(asNamespace("hydroDataBR"), error = function(e) NULL)
  if (!is.null(ns) && exists("ana_stations", envir = ns, inherits = TRUE)) {
    candidates[[length(candidates) + 1L]] <- get("ana_stations", envir = ns, inherits = TRUE)
  }
  data_env <- new.env(parent = emptyenv())
  suppressWarnings(try(utils::data("ana_stations", package = "hydroDataBR", envir = data_env), silent = TRUE))
  if (exists("ana_stations", envir = data_env, inherits = FALSE)) {
    candidates[[length(candidates) + 1L]] <- get("ana_stations", envir = data_env, inherits = FALSE)
  }
  if (exists("ana_stations", envir = .GlobalEnv, inherits = FALSE)) {
    candidates[[length(candidates) + 1L]] <- get("ana_stations", envir = .GlobalEnv, inherits = FALSE)
  }
  for (candidate in candidates) {
    if (is.data.frame(candidate) && nrow(candidate)) return(as.data.frame(candidate, stringsAsFactors = FALSE))
  }
  data.frame(stringsAsFactors = FALSE)
}
ana_api_station_row <- function(station_code, station_inventory = NULL) {
  inv <- ana_api_station_inventory(station_inventory)
  if (!is.data.frame(inv) || !nrow(inv)) return(NULL)
  code_col <- if ("station_code" %in% names(inv)) {
    "station_code"
  } else {
    nms <- names(inv)
    key <- gsub("[^a-z0-9]", "", tolower(iconv(nms, from = "UTF-8", to = "ASCII//TRANSLIT")))
    hit <- which(key %in% c("stationcode", "codestacao", "codigoestacao", "estacaocodigo", "code", "codigo", "cod"))
    if (length(hit)) nms[[hit[[1L]]]] else NA_character_
  }
  if (is.na(code_col)) return(NULL)
  key <- sub("[.]0+$", "", trimws(as.character(station_code)[1L]))
  inv_key <- sub("[.]0+$", "", trimws(as.character(inv[[code_col]])))
  hit <- which(inv_key == key)
  if (!length(hit)) {
    z <- function(x) { y <- sub("^0+", "", x); ifelse(nzchar(y), y, "0") }
    hit <- which(z(inv_key) == z(key))
  }
  if (!length(hit)) return(NULL)
  inv[hit[[1L]], , drop = FALSE]
}
ana_api_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return("")
  out <- tolower(as.character(x)[1L])
  out <- iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(out)) "" else out
}
ana_api_row_value <- function(row, name) {
  if (is.null(row) || !is.data.frame(row) || !nrow(row) || !(name %in% names(row))) return(NA)
  row[[name]][[1L]]
}
ana_api_truthy <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(FALSE)
  if (is.logical(x)) return(isTRUE(x[[1L]]))
  if (is.numeric(x)) return(!is.na(x[[1L]]) && x[[1L]] != 0)
  ana_api_text(x) %in% c("true", "t", "1", "yes", "sim")
}
ana_api_reference_fields <- function(product) {
  switch(
    product,
    daily_discharge = c("discharge_start_date", "discharge_end_date"),
    daily_stage = c("stage_start_date", "stage_end_date"),
    daily_rainfall = c("rainfall_start_date", "rainfall_end_date"),
    discharge_measurements = c("discharge_start_date", "discharge_end_date"),
    rating_curves = c("discharge_start_date", "discharge_end_date"),
    cross_sections = c("discharge_start_date", "discharge_end_date"),
    c(NA_character_, NA_character_)
  )
}
ana_api_reference_dates <- function(row, product) {
  fields <- ana_api_reference_fields(product)
  start <- ana_api_as_date(ana_api_row_value(row, fields[[1L]]))
  end <- ana_api_as_date(ana_api_row_value(row, fields[[2L]]))
  if (!is.na(start) && is.na(end) && ana_api_truthy(ana_api_row_value(row, "is_operating"))) {
    end <- Sys.Date()
  }
  list(start = start, end = end, fields = fields)
}
ana_api_station_kind <- function(row) {
  if (is.null(row) || !is.data.frame(row) || !nrow(row)) return("unknown")
  type <- ana_api_text(ana_api_row_value(row, "station_type"))
  if (grepl("pluv|rain", type)) return("rainfall")
  if (grepl("flu|discharge", type)) return("discharge")
  if (!is.na(ana_api_row_value(row, "discharge_start_date"))) return("discharge")
  if (!is.na(ana_api_row_value(row, "rainfall_start_date"))) return("rainfall")
  "unknown"
}
ana_api_product_applicable <- function(product, kind, row) {
  discharge_dates <- ana_api_reference_dates(row, "daily_discharge")
  stage_dates <- ana_api_reference_dates(row, "daily_stage")
  rainfall_dates <- ana_api_reference_dates(row, "daily_rainfall")
  has_rain <- ana_api_truthy(ana_api_row_value(row, "has_rainfall_data")) || !is.na(rainfall_dates$start) || !is.na(rainfall_dates$end)
  has_stage <- ana_api_truthy(ana_api_row_value(row, "has_stage_data")) || !is.na(stage_dates$start) || !is.na(stage_dates$end)
  has_discharge <- !is.na(discharge_dates$start) || !is.na(discharge_dates$end)
  has_measurements <- ana_api_truthy(ana_api_row_value(row, "has_discharge_measurements")) || has_discharge
  if (kind == "unknown" && is.null(row)) return(TRUE)
  if (product == "daily_rainfall") return(kind == "rainfall" || has_rain)
  if (product == "daily_stage") return(kind != "rainfall" && has_stage)
  if (product == "daily_discharge") return(kind != "rainfall" && (has_discharge || kind == "discharge"))
  if (product %in% c("discharge_measurements", "rating_curves", "cross_sections")) {
    return(kind != "rainfall" && has_measurements)
  }
  TRUE
}
ana_api_effective_dates <- function(row, product, start_date = NULL, end_date = NULL) {
  av <- ana_api_reference_dates(row, product)
  user_start <- ana_api_as_date(start_date)
  user_end <- ana_api_as_date(end_date)
  has_user <- !is.na(user_start) || !is.na(user_end)
  if (has_user) {
    start <- if (!is.na(user_start)) user_start else av$start
    end <- if (!is.na(user_end)) user_end else av$end
    if (!is.na(av$start) && !is.na(start)) start <- max(start, av$start)
    if (!is.na(av$end) && !is.na(end)) end <- min(end, av$end)
    source <- if (!is.na(av$start) || !is.na(av$end)) "user_intersected_with_ana_stations" else "user_dates"
    return(list(start = start, end = end, source = source, fields = av$fields))
  }
  if (!is.na(av$start) && !is.na(av$end)) {
    return(list(start = av$start, end = av$end, source = "ana_stations", fields = av$fields))
  }
  list(start = as.Date(NA), end = as.Date(NA), source = "ana_stations", fields = av$fields)
}
ana_station_product_plan <- function(station_code,
                                     products = c("daily_discharge", "daily_stage", "daily_rainfall", "discharge_measurements", "rating_curves", "cross_sections"),
                                     start_date = NULL,
                                     end_date = NULL,
                                     include_cross_sections = FALSE,
                                     station_inventory = NULL,
                                     ...) {
  if (length(products) == 1L && ana_api_is_all_product(products)) {
    products <- c("daily_discharge", "daily_stage", "daily_rainfall", "discharge_measurements", "rating_curves", "cross_sections")
  }
  row <- ana_api_station_row(station_code, station_inventory = station_inventory)
  kind <- ana_api_station_kind(row)
  dplyr::bind_rows(lapply(products, function(product) {
    applicable <- ana_api_product_applicable(product, kind, row)
    dates <- ana_api_effective_dates(row, product, start_date, end_date)
    cross_blocked <- product == "cross_sections" && !isTRUE(include_cross_sections)
    valid_dates <- !is.na(dates$start) && !is.na(dates$end) && dates$end >= dates$start
    attempt <- applicable && !cross_blocked && valid_dates
    reason <- NA_character_
    if (!applicable && kind == "rainfall") reason <- "product not applicable to rainfall station"
    if (!applicable && product == "daily_rainfall" && kind == "discharge") reason <- "rainfall product not available for this fluviometric station"
    if (is.na(reason) && !applicable) reason <- "product not applicable to station metadata"
    if (is.na(reason) && cross_blocked) reason <- "optional product; set include_cross_sections = TRUE"
    if (is.na(reason) && (is.na(dates$start) || is.na(dates$end))) reason <- "no availability dates in ana_stations for product; provide start_date and end_date to force an explicit request"
    if (is.na(reason) && !valid_dates) reason <- "requested period outside product availability"
    tibble::tibble(
      station_code = as.character(station_code)[1L],
      station_kind = kind,
      product = product,
      applicable = applicable,
      attempt = attempt,
      date_start = dates$start,
      date_end = dates$end,
      date_source = dates$source,
      date_start_field = dates$fields[[1L]],
      date_end_field = dates$fields[[2L]],
      skip_reason = reason
    )
  }))
}
ana_api_one_year_windows <- function(date_start, date_end) {
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  empty <- tibble::tibble(date_start = as.Date(character()), date_end = as.Date(character()))
  if (is.na(date_start) || is.na(date_end) || date_end < date_start) return(empty)
  out <- list()
  current <- date_start
  while (current <= date_end) {
    next_start <- seq.Date(current, by = "1 year", length.out = 2L)[2L]
    window_end <- min(next_start - 1L, date_end)
    out[[length(out) + 1L]] <- tibble::tibble(date_start = current, date_end = window_end)
    current <- window_end + 1L
  }
  dplyr::bind_rows(out)
}
ana_api_request_windows_for_product <- function(product, date_start, date_end) {
  product <- as.character(product)[[1L]]
  ana_api_one_year_windows(date_start, date_end)
}
ana_api_request_count_for_product <- function(product, date_start, date_end) {
  nrow(ana_api_request_windows_for_product(product, date_start, date_end))
}
ana_api_report_row <- function(station_code,
                               product,
                               status,
                               n_rows = 0L,
                               message = NA_character_,
                               date_start = as.Date(NA),
                               date_end = as.Date(NA),
                               date_source = NA_character_) {
  tibble::tibble(
    station_code = as.character(station_code)[1L],
    product = product,
    source = "api",
    status = status,
    success = identical(status, "success"),
    n_rows = as.integer(n_rows),
    message = as.character(message)[1L],
    date_start = as.Date(date_start),
    date_end = as.Date(date_end),
    date_source = as.character(date_source)[1L]
  )
}
ana_api_call_daily_one <- function(product, station_code, date_start, date_end, dots) {
  fun <- switch(
    product,
    daily_discharge = get_ana_daily_discharge,
    daily_stage = get_ana_daily_stage,
    daily_rainfall = get_ana_daily_rainfall,
    stop("Unsupported daily product", call. = FALSE)
  )
  args <- list(station_code = station_code, date_start = date_start, date_end = date_end)
  extra <- dots[setdiff(names(dots), c("source", "data_source", "start_date", "end_date", "include_cross_sections", "cross_sections_window_strategy", "station_inventory", "progress", "quiet", "verbose", "token", "identifier", "password", "identificador", "senha", "cpf_cnpj"))]
  do.call(fun, ana_filter_supported_args(fun, c(args, extra)))
}
ana_api_get_token_for_specialized <- function(token = NULL, dots = list()) {
  if (!is.null(token)) return(token)
  auth_names <- c("identifier", "password", "identificador", "senha", "cpf_cnpj", "base_url", "timeout", "max_attempts", "retry_sleep_seconds")
  auth_args <- dots[intersect(names(dots), auth_names)]
  credential_args <- auth_args[intersect(names(auth_args), c("identifier", "password", "identificador", "senha", "cpf_cnpj"))]
  if (!do.call(ana_has_auth_credentials, credential_args)) return(NULL)
  do.call(ana_authenticate, auth_args)
}
ana_api_call_specialized_one <- function(product, station_code, date_start, date_end, token, dots) {
  fun <- switch(
    product,
    discharge_measurements = get_ana_discharge_measurements,
    rating_curves = get_ana_rating_curves,
    cross_sections = get_ana_cross_sections,
    stop("Unsupported specialized product", call. = FALSE)
  )
  args <- list(token = token, station_code = station_code, start_date = date_start, end_date = date_end)
  extra <- dots[setdiff(names(dots), c("source", "data_source", "start_date", "end_date", "include_cross_sections", "cross_sections_window_strategy", "station_inventory", "progress", "quiet", "verbose", "token"))]
  do.call(fun, ana_filter_supported_args(fun, c(args, extra)))
}
ana_api_call_with_retry <- function(fun, max_attempts = 1L, retry_sleep_seconds = 0) {
  max_attempts <- suppressWarnings(as.integer(max_attempts)[1L])
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L
  last <- NULL
  for (i in seq_len(max_attempts)) {
    ans <- tryCatch(fun(), error = function(e) e)
    if (!inherits(ans, "error")) return(ans)
    last <- ans
    if (i < max_attempts && retry_sleep_seconds > 0) Sys.sleep(retry_sleep_seconds)
  }
  last
}
ana_api_bind_specialized <- function(product, pieces) {
  pieces <- pieces[!vapply(pieces, is.null, logical(1L))]
  if (identical(product, "cross_sections")) {
    sections <- lapply(pieces, `[[`, "sections")
    vertices <- lapply(pieces, `[[`, "vertices")
    empty <- ana_empty_cross_sections()
    return(list(sections = ana_bind_rows(c(sections, list(empty$sections))), vertices = ana_bind_rows(c(vertices, list(empty$vertices)))))
  }
  ana_bind_rows(c(pieces, list(ana_api_empty_product(product))))
}
ana_api_call_windowed <- function(product,
                                  station_code,
                                  windows,
                                  dots,
                                  token = NULL,
                                  progress_bar = NULL,
                                  progress_count = NULL) {
  pieces <- list()
  errors <- character()
  max_attempts <- if (!is.null(dots$max_attempts)) dots$max_attempts else 1L
  retry_sleep_seconds <- if (!is.null(dots$retry_sleep_seconds)) dots$retry_sleep_seconds else 0
  for (i in seq_len(nrow(windows))) {
    ans <- if (product %in% c("daily_discharge", "daily_stage", "daily_rainfall")) {
      ana_api_call_with_retry(
        function() ana_api_call_daily_one(product, station_code, windows$date_start[[i]], windows$date_end[[i]], dots),
        max_attempts = max_attempts,
        retry_sleep_seconds = retry_sleep_seconds
      )
    } else {
      ana_api_call_with_retry(
        function() ana_api_call_specialized_one(product, station_code, windows$date_start[[i]], windows$date_end[[i]], token, dots),
        max_attempts = max_attempts,
        retry_sleep_seconds = retry_sleep_seconds
      )
    }
    if (inherits(ans, "error")) {
      errors <- c(errors, conditionMessage(ans))
    } else if (ana_api_n_rows(ans) > 0L) {
      pieces[[length(pieces) + 1L]] <- ans
    }
    if (!is.null(progress_bar) && !is.null(progress_count)) {
      progress_count$value <- progress_count$value + 1L
      utils::setTxtProgressBar(progress_bar, progress_count$value)
    }
  }
  result <- if (product %in% c("daily_discharge", "daily_stage", "daily_rainfall")) {
    ana_bind_rows(c(pieces, list(ana_api_empty_product(product))))
  } else {
    ana_api_bind_specialized(product, pieces)
  }
  attr(result, "errors") <- errors
  result
}
ana_api_cross_section_windows <- function(measurements, date_start, date_end, strategy = "measurement_years") {
  strategy <- match.arg(strategy, c("measurement_years", "full_period"))
  if (identical(strategy, "full_period")) return(ana_api_one_year_windows(date_start, date_end))
  if (!is.data.frame(measurements) || !nrow(measurements) || !"measurement_datetime" %in% names(measurements)) {
    return(tibble::tibble(date_start = as.Date(character()), date_end = as.Date(character())))
  }
  measurement_dates <- as.Date(measurements$measurement_datetime)
  measurement_dates <- measurement_dates[!is.na(measurement_dates)]
  if (!length(measurement_dates)) {
    return(tibble::tibble(date_start = as.Date(character()), date_end = as.Date(character())))
  }
  years <- sort(unique(format(measurement_dates, "%Y")))
  out <- lapply(years, function(year) {
    start <- max(as.Date(paste0(year, "-01-01")), as.Date(date_start))
    end <- min(as.Date(paste0(year, "-12-31")), as.Date(date_end))
    if (is.na(start) || is.na(end) || end < start) return(NULL)
    tibble::tibble(date_start = start, date_end = end)
  })
  out <- out[!vapply(out, is.null, logical(1L))]
  if (!length(out)) return(tibble::tibble(date_start = as.Date(character()), date_end = as.Date(character())))
  dplyr::bind_rows(out)
}
ana_api_message_plan <- function(plan) {
  attempted <- plan[plan$attempt, , drop = FALSE]
  if (!nrow(attempted)) {
    if (nrow(plan)) message("ANA API: no product requests were planned for station ", plan$station_code[[1L]], ".")
    return(invisible(NULL))
  }
  header <- paste0("Usando datas de disponibilidade de ana_stations para a estacao ", attempted$station_code[[1L]], ":")
  lines <- paste0("- ", attempted$product, ": ", format(attempted$date_start), " a ", format(attempted$date_end), " [", attempted$date_source, "]")
  message(paste(c(header, lines), collapse = "\n"))
  invisible(NULL)
}
ana_api_get_all_data <- function(station_code,
                                 data_source = "api",
                                 path = NULL,
                                 start_date = NULL,
                                 end_date = NULL,
                                 token = NULL,
                                 include_cross_sections = FALSE,
                                 cross_sections_window_strategy = "measurement_years",
                                 station_inventory = NULL,
                                 progress = interactive(),
                                 quiet = FALSE,
                                 verbose = FALSE,
                                 ...) {
  if (!ana_api_is_api_source(data_source)) {
    stop("`product = 'all'` is supported only for data_source = 'api'.", call. = FALSE)
  }
  if (is.null(station_code) || length(station_code) != 1L || is.na(station_code)) {
    stop("`product = 'all'` with data_source = 'api' requires one station code.", call. = FALSE)
  }
  cross_sections_window_strategy <- match.arg(cross_sections_window_strategy, c("measurement_years", "full_period"))
  dots <- list(...)
  station_code <- as.character(station_code)[1L]
  plan <- ana_station_product_plan(
    station_code = station_code,
    start_date = start_date,
    end_date = end_date,
    include_cross_sections = include_cross_sections,
    station_inventory = station_inventory
  )
  if (!isTRUE(quiet)) ana_api_message_plan(plan)
  out <- list(
    daily_discharge = ana_api_empty_product("daily_discharge"),
    daily_stage = ana_api_empty_product("daily_stage"),
    daily_rainfall = ana_api_empty_product("daily_rainfall"),
    discharge_measurements = ana_api_empty_product("discharge_measurements"),
    rating_curves = ana_api_empty_product("rating_curves"),
    cross_sections = ana_api_empty_product("cross_sections")
  )
  reports <- list()
  request_counts <- vapply(seq_len(nrow(plan)), function(i) {
    if (!isTRUE(plan$attempt[[i]])) return(0L)
    ana_api_request_count_for_product(plan$product[[i]], plan$date_start[[i]], plan$date_end[[i]])
  }, integer(1L))
  total <- max(1L, sum(request_counts))
  pb <- NULL
  counter <- new.env(parent = emptyenv())
  counter$value <- 0L
  if (isTRUE(progress)) {
    message("ANA API: iniciando download autenticado (", total, " solicitacao(oes) planejada(s)).")
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    on.exit({ close(pb); message("ANA API: download autenticado finalizado.") }, add = TRUE)
  }
  specialized_token <- token
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, , drop = FALSE]
    product <- row$product[[1L]]
    if (!isTRUE(row$attempt[[1L]])) {
      reports[[length(reports) + 1L]] <- ana_api_report_row(station_code, product, "skipped", 0L, row$skip_reason[[1L]], row$date_start[[1L]], row$date_end[[1L]], row$date_source[[1L]])
      next
    }
    windows <- if (identical(product, "cross_sections")) {
      ana_api_cross_section_windows(out$discharge_measurements, row$date_start[[1L]], row$date_end[[1L]], strategy = cross_sections_window_strategy)
    } else {
      ana_api_request_windows_for_product(product, row$date_start[[1L]], row$date_end[[1L]])
    }
    if (!nrow(windows)) {
      reports[[length(reports) + 1L]] <- ana_api_report_row(
        station_code,
        product,
        "skipped",
        0L,
        if (identical(product, "cross_sections")) "no discharge_measurements dates available to guide optional cross_sections" else "no valid request windows",
        row$date_start[[1L]],
        row$date_end[[1L]],
        row$date_source[[1L]]
      )
      next
    }
    if (product %in% c("discharge_measurements", "rating_curves", "cross_sections") && is.null(specialized_token)) {
      specialized_token <- ana_api_get_token_for_specialized(token = token, dots = dots)
      if (is.null(specialized_token)) {
        reports[[length(reports) + 1L]] <- ana_api_report_row(station_code, product, "error", 0L, "ANA credentials are required for specialized authenticated routes.", row$date_start[[1L]], row$date_end[[1L]], row$date_source[[1L]])
        next
      }
    }
    ans <- ana_api_call_windowed(
      product = product,
      station_code = station_code,
      windows = windows,
      dots = dots,
      token = specialized_token,
      progress_bar = pb,
      progress_count = counter
    )
    out[[product]] <- ans
    errors <- attr(ans, "errors")
    n <- ana_api_n_rows(ans)
    status <- if (n > 0L) "success" else if (length(errors)) "error" else "empty"
    message <- if (length(errors)) paste0("request errors (n = ", length(errors), "); first error: ", errors[[1L]]) else NA_character_
    reports[[length(reports) + 1L]] <- ana_api_report_row(station_code, product, status, n, message, row$date_start[[1L]], row$date_end[[1L]], row$date_source[[1L]])
  }
  out$daily_data <- ana_bind_rows(list(out$daily_discharge, out$daily_stage, out$daily_rainfall))
  out$request_report <- ana_bind_rows(reports)
  out
}
ana_get_all_data <- function(data_source = "api", station_code = NULL, path = NULL, ...) {
  ana_api_get_all_data(station_code = station_code, data_source = data_source, path = path, ...)
}
ana_api_get_all_data_batch <- function(station_codes, data_source = "api", path = NULL, ...) {
  station_codes <- ana_normalize_station_code_vector(station_codes)
  results <- stats::setNames(vector("list", length(station_codes)), station_codes)
  reports <- list()
  for (i in seq_along(station_codes)) {
    code <- station_codes[[i]]
    ans <- tryCatch(ana_api_get_all_data(station_code = code, data_source = data_source, path = path, ...), error = function(e) e)
    if (inherits(ans, "error")) {
      results[[i]] <- NULL
      reports[[length(reports) + 1L]] <- ana_api_report_row(code, "all", "error", 0L, conditionMessage(ans))
    } else {
      results[[i]] <- ans
      report <- ans$request_report
      report$batch_index <- i
      reports[[length(reports) + 1L]] <- report
    }
  }
  list(results = results, request_report = ana_bind_rows(reports))
}
ana_all_get_data_batch_impl <- function(station_codes, source = "api", ...) {
  ana_api_get_all_data_batch(station_codes = station_codes, data_source = source, ...)
}
#' Obter dados ANA por produto e origem
#'
#' Funcao geral para obter dados ANA. A funcao pode ler arquivos locais ou
#' obter dados do HidroWebService, dependendo de `data_source`. O argumento
#' `station_code` aceita um codigo ou um vetor de codigos quando o produto
#' depende de estacao.
#'
#' @param product Produto desejado. Exemplos: `daily_discharge`, `daily_stage`,
#'   `daily_rainfall`, `stations`, `discharge_measurements`, `rating_curves`,
#'   `cross_sections`, `states`, `municipalities`, `basins`, `subbasins`,
#'   `rivers`, `entities` ou `all`.
#' @param data_source Origem dos dados. Use `webservice`, `api`, `hidroweb`,
#'   `xml` ou `json`.
#' @param station_code Codigo da estacao como texto, vetor de codigos, ou `NULL`
#'   para produtos que nao dependem de estacao.
#' @param path Caminho para arquivo local quando `data_source` nao for
#'   `webservice` ou `api`.
#' @param ... Argumentos adicionais repassados para a funcao especifica.
#'
#' @return Objeto R com dados padronizados.
#' @keywords internal
.hydrodatabr_original_get_ana_data <- function(product,
                         data_source = "webservice",
                         station_code = NULL,
                         path = NULL,
                         ...) {
  dots <- list(...)
  if (missing(data_source) && "source" %in% names(dots)) {
    data_source <- dots$source
    dots$source <- NULL
  }
  if (ana_api_is_all_product(product)) {
    return(do.call(ana_api_get_all_data, c(list(station_code = station_code, data_source = data_source, path = path), dots)))
  }
  product <- ana_normalize_product_name(product)
  if (ana_api_is_api_source(data_source)) data_source <- "webservice"
  data_source <- ana_normalize_data_source_name(data_source)
  if (data_source != "webservice") {
    return(do.call(ana_get_local_data, c(list(product = product, data_source = data_source, path = path, station_code = station_code), dots)))
  }
  station_code <- ana_normalize_station_code_vector(station_code)
  if (length(station_code) > 1L) {
    return(do.call(ana_get_webservice_data_batch_direct, c(list(product = product, station_code = station_code), dots)))
  }
  do.call(ana_get_webservice_data_single, c(list(product = product, station_code = station_code), dots))
}
#' Obter dados da ANA para várias estações
#'
#' Executa `get_ana_data()` para um conjunto de estações e organiza os resultados
#' em uma lista padronizada. A função é útil quando o mesmo produto deve ser
#' obtido para vários postos, mantendo um relatório único de sucesso, falha,
#' resultado vazio ou produto ignorado.
#'
#' @param product Produto desejado. Use os mesmos valores aceitos por
#'   `get_ana_data()`, incluindo `"all"` para aquisição agregada por estação.
#' @param station_code Vetor de códigos de estação.
#' @param ... Argumentos adicionais repassados a `get_ana_data()` ou à rota
#'   agregada, como `start_date`, `end_date`, `token`, `source`,
#'   `include_cross_sections`, `timeout` e opções de repetição.
#' @param station_codes Alias de compatibilidade para `station_code`.
#' @param data_source Origem dos dados. Ver `get_ana_data()`.
#' @param path Caminho de arquivo local, quando aplicável.
#'
#' @details
#' Em aquisições agregadas com `product = "all"`, códigos repetidos são tratados
#' uma única vez. O relatório informa a ordem do lote e permite separar casos de
#' sucesso, erro, resposta vazia e produtos pulados por regra de disponibilidade.
#'
#' Para downloads longos via API, especialmente com produtos especializados, é
#' recomendável começar com poucos postos e janelas curtas de tempo, avaliar o
#' `request_report` e só depois ampliar o lote.
#'
#' @return Lista com `results`, contendo os resultados por estação, e
#'   `request_report`, contendo o relatório consolidado das requisições.
#' @export
#'
#' @examples
#' # Exemplo operacional. Requer credenciais validas da ANA.
#' if (FALSE) {
#'   token <- ana_authenticate()
#'   lote <- get_ana_data_batch(
#'     product = "all",
#'     station_code = c("56460000", "40100000"),
#'     data_source = "api",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     include_cross_sections = FALSE,
#'     token = token
#'   )
#'   table_hydro_data(lote, table = "request_report")
#' }
get_ana_data_batch <- function(product,
                               station_code = NULL,
                               ...,
                               station_codes = NULL,
                               data_source = "webservice",
                               path = NULL) {
  dots <- list(...)
  if (missing(data_source) && "source" %in% names(dots)) {
    data_source <- dots$source
    dots$source <- NULL
  }
  if (is.null(station_code) && !is.null(station_codes)) station_code <- station_codes
  if (ana_api_is_all_product(product)) {
    return(do.call(ana_api_get_all_data_batch, c(list(station_codes = station_code, data_source = data_source, path = path), dots)))
  }
  if (!is.null(station_code) && length(product) > 1L && length(station_code) == 1L && ana_is_product_alias(station_code)) {
    old_station_code <- product
    product <- station_code
    station_code <- old_station_code
  }
  product <- ana_normalize_product_name(product)
  if (ana_api_is_api_source(data_source)) data_source <- "webservice"
  data_source <- ana_normalize_data_source_name(data_source)
  if (data_source != "webservice") {
    return(do.call(get_ana_data, c(list(product = product, station_code = station_code, data_source = data_source, path = path), dots)))
  }
  do.call(ana_get_webservice_data_batch_direct, c(list(product = product, station_code = station_code), dots))
}
get_ana_daily_discharge_batch <- function(station_codes, ...) {
  get_ana_data_batch("daily_discharge", station_codes = station_codes, ...)
}
get_ana_daily_stage_batch <- function(station_codes, ...) {
  get_ana_data_batch("daily_stage", station_codes = station_codes, ...)
}
get_ana_daily_rainfall_batch <- function(station_codes, ...) {
  get_ana_data_batch("daily_rainfall", station_codes = station_codes, ...)
}
