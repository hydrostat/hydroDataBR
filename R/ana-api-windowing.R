# API request-window protection for ANA authenticated routes.
# This file is intentionally source-level code, not an operational test script.
ana_api_max_window_days <- function() 366L
ana_as_date_scalar <- function(x, arg = "date") {
  if (is.null(x)) return(NULL)
  out <- as.Date(x)
  if (length(out) != 1L || is.na(out)) {
    stop(arg, " must be a single valid Date or date-like value.", call. = FALSE)
  }
  out
}
ana_api_window_n_days <- function(start_date, end_date) {
  start_date <- ana_as_date_scalar(start_date, "start_date")
  end_date <- ana_as_date_scalar(end_date, "end_date")
  as.integer(end_date - start_date) + 1L
}
ana_assert_api_window <- function(start_date, end_date, max_days = ana_api_max_window_days()) {
  n_days <- ana_api_window_n_days(start_date, end_date)
  if (n_days < 1L) {
    stop("end_date must be greater than or equal to start_date.", call. = FALSE)
  }
  if (n_days > max_days) {
    stop(
      "ANA authenticated API requests must cover at most ", max_days,
      " days per request. Use ana_split_api_windows() before requesting data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
ana_split_api_windows <- function(start_date, end_date, max_days = ana_api_max_window_days()) {
  start_date <- ana_as_date_scalar(start_date, "start_date")
  end_date <- ana_as_date_scalar(end_date, "end_date")
  if (end_date < start_date) {
    stop("end_date must be greater than or equal to start_date.", call. = FALSE)
  }
  starts <- as.Date(character())
  ends <- as.Date(character())
  current <- start_date
  while (current <= end_date) {
    current_end <- min(current + max_days - 1L, end_date)
    ana_assert_api_window(current, current_end, max_days = max_days)
    starts <- c(starts, current)
    ends <- c(ends, current_end)
    current <- current_end + 1L
  }
  data.frame(start_date = starts, end_date = ends, stringsAsFactors = FALSE)
}
ana_normalize_source_name <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  tolower(trimws(as.character(x[[1L]])))
}
ana_effective_data_source <- function(data_source = NULL, dots = list()) {
  if (!is.null(dots$source)) return(ana_normalize_source_name(dots$source))
  if (!is.null(dots$data_source)) return(ana_normalize_source_name(dots$data_source))
  ana_normalize_source_name(data_source)
}
ana_remove_source_aliases <- function(dots) {
  dots$source <- NULL
  dots$data_source <- NULL
  dots
}
ana_is_api_source <- function(data_source = NULL, dots = list()) {
  identical(ana_effective_data_source(data_source, dots), "api")
}
ana_bind_data_frames <- function(x) {
  x <- Filter(function(z) is.data.frame(z) && nrow(z) >= 0L, x)
  if (!length(x)) return(data.frame())
  cols <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(df) {
    missing_cols <- setdiff(cols, names(df))
    for (col in missing_cols) df[[col]] <- NA
    df[cols]
  })
  out <- do.call(rbind, x)
  row.names(out) <- NULL
  out
}
ana_combine_window_outputs <- function(x) {
  x <- Filter(function(z) !is.null(z), x)
  if (!length(x)) return(data.frame())
  if (all(vapply(x, is.data.frame, logical(1)))) {
    return(ana_bind_data_frames(x))
  }
  if (all(vapply(x, is.list, logical(1)))) {
    nms <- unique(unlist(lapply(x, names), use.names = FALSE))
    out <- vector("list", length(nms))
    names(out) <- nms
    for (nm in nms) {
      out[[nm]] <- ana_combine_window_outputs(lapply(x, function(z) z[[nm]]))
    }
    return(out)
  }
  x[[1L]]
}
ana_add_supported_arg <- function(fun, args, name, value) {
  if (is.null(value)) return(args)
  fml <- names(formals(fun))
  if (name %in% fml || "..." %in% fml) args[[name]] <- value
  args
}
ana_call_original_direct_windowed <- function(fun, station_code, start_date, end_date, data_source, dots) {
  windows <- ana_split_api_windows(start_date, end_date)
  dots <- ana_remove_source_aliases(dots)
  results <- vector("list", nrow(windows))
  for (i in seq_len(nrow(windows))) {
    args <- c(
      list(
        station_code = station_code,
        start_date = windows$start_date[[i]],
        end_date = windows$end_date[[i]]
      ),
      dots
    )
    args <- ana_add_supported_arg(fun, args, "data_source", data_source)
    results[[i]] <- do.call(fun, args)
  }
  ana_combine_window_outputs(results)
}
ana_call_original_get_ana_data <- function(product, data_source, station_code, path, dots) {
  dots <- ana_remove_source_aliases(dots)
  args <- c(
    list(
      product = product,
      data_source = data_source,
      station_code = station_code,
      path = path
    ),
    dots
  )
  do.call(.hydrodatabr_original_get_ana_data, args)
}
ana_extract_daily_from_aggregate <- function(x, product) {
  if (is.list(x) && !is.null(x[[product]])) return(x[[product]])
  variable <- switch(
    product,
    daily_discharge = "discharge",
    daily_stage = "stage",
    daily_rainfall = "rainfall",
    NA_character_
  )
  if (is.list(x) && !is.null(x$daily_data) && is.data.frame(x$daily_data) && "variable" %in% names(x$daily_data)) {
    return(x$daily_data[x$daily_data$variable == variable, , drop = FALSE])
  }
  data.frame()
}
ana_get_api_daily_via_aggregate <- function(product, data_source, station_code, path, dots) {
  dots$include_cross_sections <- FALSE
  aggregate <- ana_call_original_get_ana_data(
    product = "all",
    data_source = data_source,
    station_code = station_code,
    path = path,
    dots = dots
  )
  ana_extract_daily_from_aggregate(aggregate, product)
}
ana_get_api_product_windowed <- function(product, data_source, station_code, path, dots) {
  if (is.null(dots$start_date) || is.null(dots$end_date)) {
    return(ana_call_original_get_ana_data(product, data_source, station_code, path, dots))
  }
  windows <- ana_split_api_windows(dots$start_date, dots$end_date)
  results <- vector("list", nrow(windows))
  for (i in seq_len(nrow(windows))) {
    window_dots <- dots
    window_dots$start_date <- windows$start_date[[i]]
    window_dots$end_date <- windows$end_date[[i]]
    results[[i]] <- ana_call_original_get_ana_data(product, data_source, station_code, path, window_dots)
  }
  ana_combine_window_outputs(results)
}
#' Obter dados hidrológicos da ANA
#'
#' Obtém dados da ANA por uma interface única. A função cobre leituras locais,
#' consultas ao WebService legado e consultas à API autenticada, sempre que a
#' rota correspondente estiver disponível para o produto solicitado.
#'
#' Esta é a principal porta de entrada para aquisição de dados no hydroDataBR.
#' Para uso cotidiano, escolha o produto em `product`, a origem em `data_source`
#' ou `source`, e informe os demais argumentos necessários, como código da
#' estação, período ou caminho de arquivo local.
#'
#' @param product Produto solicitado. Valores usuais incluem `"daily_discharge"`,
#'   `"daily_stage"`, `"daily_rainfall"`, `"discharge_measurements"`,
#'   `"rating_curves"`, `"cross_sections"`, `"stations"`,
#'   `"states"`, `"municipalities"`, `"basins"`, `"subbasins"`,
#'   `"rivers"`, `"entities"` e `"all"`. Para arquivos locais, o produto
#'   diário pode ser inferido a partir do leitor usado.
#' @param data_source Origem dos dados. Use `"api"` para a API autenticada,
#'   `"webservice"` para o WebService online legado, `"xml"` para arquivo
#'   XML local da operação `HidroSerieHistorica`, ou `"hidroweb"` para arquivo
#'   CSV/ZIP local do HidroWeb.
#' @param station_code Código da estação ANA. Pode ser texto ou número; códigos
#'   com zeros à esquerda devem ser informados como texto.
#' @param path Caminho de arquivo local, quando `data_source` for `"xml"`,
#'   `"hidroweb"` ou outra fonte local suportada.
#' @param ... Argumentos adicionais usados pela rota escolhida. Os mais comuns
#'   são `start_date`, `end_date`, `token`, `source`, `include_cross_sections`,
#'   `variables`, `timeout`, `max_attempts` e opções de repetição de requisições.
#'
#' @details
#' Para séries diárias, o retorno segue o contrato padronizado do pacote, com as
#' colunas `station_code`, `date`, `variable`, `value`, `unit`,
#' `consistency_level`, `source_status` e `source`. Essa padronização permite
#' usar diretamente os resultados em `analyze_hydro_data()`, `plot_hydro_data()`,
#' `table_hydro_data()` e `write_hydro_data()`.
#'
#' Com `product = "all"` e `data_source = "api"`, a função monta uma aquisição
#' agregada para uma estação. Para postos fluviométricos, são avaliados dados de
#' vazão diária, cota diária, medições de descarga e curvas-chave; chuva diária
#' só é solicitada quando houver disponibilidade pluviométrica no inventário. As
#' seções transversais são opcionais e ficam desativadas por padrão com
#' `include_cross_sections = FALSE`, pois essa rota pode ser lenta ou instável.
#'
#' As rotas da API autenticada são planejadas em janelas de no máximo um ano. Em
#' períodos longos, o download pode demorar, retornar resultados vazios para
#' algumas janelas ou falhar por instabilidade temporária do serviço. Isso é
#' especialmente comum em produtos especializados, como curvas-chave e seções
#' transversais. Resultados vazios de `rating_curves` podem ser válidos e não
#' indicam, por si só, erro do pacote.
#'
#' @return O tipo de objeto depende do produto. Séries diárias retornam um
#'   `data.frame` padronizado. Produtos especializados retornam tabelas ou listas
#'   com tabelas. Aquisições agregadas retornam uma lista com os produtos obtidos,
#'   a tabela diária combinada em `daily_data` e um `request_report` com o estado
#'   de cada requisição.
#' @export
#'
#' @examples
#' # Exemplo operacional com API autenticada. Requer credenciais válidas da ANA.
#' if (FALSE) {
#'   token <- ana_authenticate(
#'     identifier = Sys.getenv("ANA_HIDROWEBSERVICE_IDENTIFIER"),
#'     password = Sys.getenv("ANA_HIDROWEBSERVICE_PASSWORD")
#'   )
#'
#'   dados <- get_ana_data(
#'     product = "daily_discharge",
#'     data_source = "api",
#'     station_code = "56460000",
#'     start_date = "2020-01-01",
#'     end_date = "2020-01-31",
#'     token = token
#'   )
#'
#'   agregado <- get_ana_data(
#'     product = "all",
#'     data_source = "api",
#'     station_code = "56460000",
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     include_cross_sections = FALSE,
#'     token = token
#'   )
#' }
#'
#' # Exemplo local com arquivo ja baixado do HidroWeb.
#' if (FALSE) {
#'   dados <- get_ana_data(
#'     product = "daily_discharge",
#'     data_source = "hidroweb",
#'     path = "Estacao_56460000.zip"
#'   )
#' }
get_ana_data <- function(product, data_source = "webservice", station_code = NULL, path = NULL, ...) {
  dots <- list(...)
  effective_source <- ana_effective_data_source(data_source, dots)
  if (!is.na(effective_source)) data_source <- effective_source
  if (ana_is_api_source(data_source, dots)) {
    product_key <- ana_normalize_source_name(product)
    if (product_key %in% c("daily_discharge", "daily_stage", "daily_rainfall")) {
      return(ana_get_api_daily_via_aggregate(product_key, data_source, station_code, path, dots))
    }
    if (product_key %in% c("discharge_measurements", "rating_curves", "cross_sections")) {
      return(ana_get_api_product_windowed(product_key, data_source, station_code, path, dots))
    }
  }
  ana_call_original_get_ana_data(product, data_source, station_code, path, dots)
}
#' Obter medicoes de descarga da ANA
#'
#' Obtem produto hidrometrico especializado de uma estacao ANA pela API
#' autenticada e retorna dados padronizados pelo pacote.
#'
#' @details
#' Para `data_source = "api"`, periodos maiores que 366 dias sao divididos
#' automaticamente em janelas menores antes da consulta. A funcao pode retornar
#' sucesso, resultado vazio ou erro de rota live; em fluxos agregados, esses
#' estados devem ser avaliados pelo `request_report`. 
#'
#' Exemplos com API autenticada nao sao executados automaticamente porque
#' exigem credenciais do usuario e acesso ao servico live da ANA.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Atualmente, o uso documentado para esta
#'   funcao e `"api"`.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, opcoes de retry ou `request_function` em testes.
#'
#' @return Tabela padronizada de medicoes de descarga.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_discharge_measurements(
#'     station_code = "56460000",
#'     start_date = "2005-01-01",
#'     end_date = "2005-12-31",
#'     data_source = "api"
#'   )
#' }
#' @noRd
get_ana_discharge_measurements <- function(station_code, start_date = NULL, end_date = NULL, data_source = "api", ...) {
  dots <- list(...)
  effective_source <- ana_effective_data_source(data_source, dots)
  if (!is.na(effective_source)) data_source <- effective_source
  if (ana_is_api_source(data_source, dots) && !is.null(start_date) && !is.null(end_date)) {
    return(ana_call_original_direct_windowed(
      fun = .hydrodatabr_original_get_ana_discharge_measurements,
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      data_source = data_source,
      dots = dots
    ))
  }
  args <- c(list(station_code = station_code, start_date = start_date, end_date = end_date), dots)
  args <- ana_add_supported_arg(.hydrodatabr_original_get_ana_discharge_measurements, args, "data_source", data_source)
  do.call(.hydrodatabr_original_get_ana_discharge_measurements, args)
}
#' Obter curvas-chave da ANA
#'
#' Obtem produto hidrometrico especializado de uma estacao ANA pela API
#' autenticada e retorna dados padronizados pelo pacote.
#'
#' @details
#' Para `data_source = "api"`, periodos maiores que 366 dias sao divididos
#' automaticamente em janelas menores antes da consulta. A funcao pode retornar
#' sucesso, resultado vazio ou erro de rota live; em fluxos agregados, esses
#' estados devem ser avaliados pelo `request_report`. Curvas-chave podem retornar tabela vazia de forma valida para algumas estacoes ou periodos.
#'
#' Exemplos com API autenticada nao sao executados automaticamente porque
#' exigem credenciais do usuario e acesso ao servico live da ANA.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Atualmente, o uso documentado para esta
#'   funcao e `"api"`.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, opcoes de retry ou `request_function` em testes.
#'
#' @return Tabela padronizada de curvas-chave.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_rating_curves(
#'     station_code = "56460000",
#'     start_date = "2005-01-01",
#'     end_date = "2005-12-31",
#'     data_source = "api"
#'   )
#' }
#' @noRd
get_ana_rating_curves <- function(station_code, start_date = NULL, end_date = NULL, data_source = "api", ...) {
  dots <- list(...)
  effective_source <- ana_effective_data_source(data_source, dots)
  if (!is.na(effective_source)) data_source <- effective_source
  if (ana_is_api_source(data_source, dots) && !is.null(start_date) && !is.null(end_date)) {
    return(ana_call_original_direct_windowed(
      fun = .hydrodatabr_original_get_ana_rating_curves,
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      data_source = data_source,
      dots = dots
    ))
  }
  args <- c(list(station_code = station_code, start_date = start_date, end_date = end_date), dots)
  args <- ana_add_supported_arg(.hydrodatabr_original_get_ana_rating_curves, args, "data_source", data_source)
  do.call(.hydrodatabr_original_get_ana_rating_curves, args)
}
#' Obter secoes transversais da ANA
#'
#' Obtem produto hidrometrico especializado de uma estacao ANA pela API
#' autenticada e retorna dados padronizados pelo pacote.
#'
#' @details
#' Para `data_source = "api"`, periodos maiores que 366 dias sao divididos
#' automaticamente em janelas menores antes da consulta. A funcao pode retornar
#' sucesso, resultado vazio ou erro de rota live; em fluxos agregados, esses
#' estados devem ser avaliados pelo `request_report`. Secoes transversais podem ser lentas ou instaveis em chamadas live. Por isso, ficam desativadas por padrao em `get_ana_data(product = "all", data_source = "api")`, a menos que `include_cross_sections = TRUE` seja informado.
#'
#' Exemplos com API autenticada nao sao executados automaticamente porque
#' exigem credenciais do usuario e acesso ao servico live da ANA.
#'
#' @param station_code Codigo da estacao ANA como texto ou numero.
#' @param start_date Data inicial no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param end_date Data final no formato `"YYYY-MM-DD"`, `"dd/mm/yyyy"` ou
#'   objeto `Date`.
#' @param data_source Fonte de dados. Atualmente, o uso documentado para esta
#'   funcao e `"api"`.
#' @param ... Argumentos adicionais repassados internamente, como `token`,
#'   `timeout`, opcoes de retry ou `request_function` em testes.
#'
#' @return Objeto com tabelas `sections` e `vertices`, quando disponiveis.
#'
#' @examples
#' if (FALSE) {
#'   dados <- get_ana_cross_sections(
#'     station_code = "56460000",
#'     start_date = "2005-01-01",
#'     end_date = "2005-12-31",
#'     data_source = "api"
#'   )
#' }
#' @noRd
get_ana_cross_sections <- function(station_code, start_date = NULL, end_date = NULL, data_source = "api", ...) {
  dots <- list(...)
  effective_source <- ana_effective_data_source(data_source, dots)
  if (!is.na(effective_source)) data_source <- effective_source
  if (ana_is_api_source(data_source, dots) && !is.null(start_date) && !is.null(end_date)) {
    return(ana_call_original_direct_windowed(
      fun = .hydrodatabr_original_get_ana_cross_sections,
      station_code = station_code,
      start_date = start_date,
      end_date = end_date,
      data_source = data_source,
      dots = dots
    ))
  }
  args <- c(list(station_code = station_code, start_date = start_date, end_date = end_date), dots)
  args <- ana_add_supported_arg(.hydrodatabr_original_get_ana_cross_sections, args, "data_source", data_source)
  do.call(.hydrodatabr_original_get_ana_cross_sections, args)
}
