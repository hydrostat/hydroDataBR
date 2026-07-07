# Gerar tabelas hidrológicas

Organiza séries, análises, diagnósticos e relatórios do hydroDataBR em
tabelas prontas para inspeção ou exportação. A função aceita tanto
séries diárias padronizadas quanto objetos retornados por
[`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md)
e
[`get_ana_data_batch()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data_batch.md).
Para `table = "hydrometry_diagnostics"`, séries diárias e objetos
agregados podem ser diagnosticados automaticamente com apoio da base
hidrométrica interna quando referências explícitas não forem fornecidas.

## Usage

``` r
table_hydro_data(
  data,
  table = c("daily_availability", "daily_statistics", "flow_duration", "flow_indices",
    "rainfall_indices", "annual_maxima", "low_flows", "station_products",
    "request_report", "hydrometry_diagnostics", "cross_sections", "monthly_flow_indices",
    "rainfall_annual_maxima", "rainfall_diagnostics", "daily_gap_summary",
    "measurement_diagnostics", "rating_diagnostics"),
  variable = NULL,
  station_code = NULL,
  period = NULL,
  ...
)
```

## Arguments

- data:

  Objeto de dados. Pode ser série diária padronizada, objeto de
  aquisição, lote de aquisição, diagnóstico ou relatório.

- table:

  Tipo de tabela. Valores aceitos incluem `"daily_availability"`,
  `"daily_statistics"`, `"flow_duration"`, `"flow_indices"`,
  `"monthly_flow_indices"`, `"annual_maxima"`, `"low_flows"`,
  `"rainfall_indices"`, `"rainfall_annual_maxima"`,
  `"rainfall_diagnostics"`, `"daily_gap_summary"`, `"station_products"`,
  `"request_report"`, `"hydrometry_diagnostics"`,
  `"measurement_diagnostics"`, `"rating_diagnostics"` e
  `"cross_sections"`.

- variable:

  Variável diária a filtrar.

- station_code:

  Código(s) de estação a filtrar.

- period:

  Período usado em tabelas diárias, como `"annual"`, `"monthly"` ou
  `"monthly_regime"`, conforme a tabela solicitada.

- ...:

  Argumentos adicionais usados pela tabela escolhida.

## Value

`data.frame` com a tabela solicitada.

## Examples

``` r
daily <- data.frame(
  station_code = "001",
  date = as.Date("2020-01-01") + 0:29,
  variable = "discharge",
  value = seq(10, 39),
  unit = "m3/s",
  consistency_level = NA_integer_,
  source_status = NA_character_,
  source = "example"
)

table_hydro_data(daily, table = "daily_statistics")
#>   station_code  variable unit year period days_expected days_observed
#> 1          001 discharge m3/s 2020   2020            30            30
#>   days_with_value days_missing availability_pct missing_pct mean_value
#> 1              30            0              100           0       24.5
#>   min_value max_value total_value
#> 1        10        39         735
table_hydro_data(daily, table = "daily_availability")
#>   station_code  variable year period days_expected days_observed
#> 1          001 discharge 2020   2020            30            30
#>   days_with_value days_missing availability_pct missing_pct
#> 1              30            0              100           0
```
