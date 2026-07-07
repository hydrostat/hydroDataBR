# Obter dados da ANA para várias estações

Executa
[`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md)
para um conjunto de estações e organiza os resultados em uma lista
padronizada. A função é útil quando o mesmo produto deve ser obtido para
vários postos, mantendo um relatório único de sucesso, falha, resultado
vazio ou produto ignorado.

## Usage

``` r
get_ana_data_batch(
  product,
  station_code = NULL,
  ...,
  station_codes = NULL,
  data_source = "webservice",
  path = NULL
)
```

## Arguments

- product:

  Produto desejado. Use os mesmos valores aceitos por
  [`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md),
  incluindo `"all"` para aquisição agregada por estação.

- station_code:

  Vetor de códigos de estação.

- ...:

  Argumentos adicionais repassados a
  [`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md)
  ou à rota agregada, como `start_date`, `end_date`, `token`, `source`,
  `include_cross_sections`, `timeout` e opções de repetição.

- station_codes:

  Alias de compatibilidade para `station_code`.

- data_source:

  Origem dos dados. Ver
  [`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md).

- path:

  Caminho de arquivo local, quando aplicável.

## Value

Lista com `results`, contendo os resultados por estação, e
`request_report`, contendo o relatório consolidado das requisições.

## Details

Em aquisições agregadas com `product = "all"`, códigos repetidos são
tratados uma única vez. O relatório informa a ordem do lote e permite
separar casos de sucesso, erro, resposta vazia e produtos pulados por
regra de disponibilidade.

Para downloads longos via API, especialmente com produtos
especializados, é recomendável começar com poucos postos e janelas
curtas de tempo, avaliar o `request_report` e só depois ampliar o lote.

## Examples

``` r
# Exemplo operacional. Requer credenciais validas da ANA.
if (FALSE) {
  token <- ana_authenticate()
  lote <- get_ana_data_batch(
    product = "all",
    station_code = c("56460000", "40100000"),
    data_source = "api",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    include_cross_sections = FALSE,
    token = token
  )
  table_hydro_data(lote, table = "request_report")
}
```
