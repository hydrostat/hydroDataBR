# Diagnosticar medições de descarga e curvas-chave

Avalia a consistência geral entre medições de descarga e curvas-chave de
uma estação. A função calcula indicadores de qualidade, verifica valores
nulos ou negativos, identifica grupos repetidos, pareia medições com
segmentos de curva-chave e, quando solicitado, calcula resíduos e
envelopes empíricos.

## Usage

``` r
diagnose_station_hydrometry(
  measurements = data.frame(),
  rating_curves = data.frame(),
  params = NULL,
  detailed = TRUE,
  rating_curve_summary = data.frame(),
  station_code = NULL,
  use_internal_database = TRUE
)
```

## Arguments

- measurements:

  Tabela de medições de descarga, ou objeto agregado retornado por
  `get_ana_data(product = "all")` contendo o elemento
  `discharge_measurements`.

- rating_curves:

  Tabela de curvas-chave. Se `measurements` for um objeto agregado e
  este argumento estiver vazio, a função tenta usar o elemento
  `rating_curves` do próprio objeto.

- params:

  Lista opcional de parâmetros diagnósticos. Valores omitidos usam os
  padrões internos.

- detailed:

  Se `TRUE`, calcula pareamento, resíduos, envelopes e triagem temporal.
  Se `FALSE`, calcula apenas métricas leves.

- rating_curve_summary:

  Tabela opcional de resumo das curvas-chave.

- station_code:

  Código(s) de estação a filtrar. Também permite consultar a base
  interna quando `measurements` estiver vazio.

- use_internal_database:

  Se `TRUE`, usa a base hidrométrica interna do pacote quando não houver
  referência fornecida pelo usuário nem no objeto de aquisição.

## Value

Lista com tabelas de resumo, índices, flags de medições, metadados das
curvas, pareamentos e resultados de resíduos quando `detailed = TRUE`.

## Details

Quando medições de descarga ou curvas-chave não são fornecidas pelo
usuário nem estão presentes em um objeto agregado de
`get_ana_data(product = "all")`, a função consulta a base hidrométrica
interna do pacote, correspondente a um retrato da ANA de junho de 2026.
O objeto retornado informa a origem da referência hidrométrica usada.

Os resultados são exploratórios e servem para apoiar revisão
hidrométrica. Eles não substituem a avaliação técnica de consistência,
nem a revisão especializada de uma curva-chave.

## Examples

``` r
measurements <- data.frame(
  station_code = "001",
  measurement_date = as.Date(c("2020-01-01", "2020-02-01")),
  stage_cm = c(100, 120),
  discharge_m3s = c(10, 15)
)

curves <- data.frame(
  station_code = "001",
  rating_curve_id = "rc1",
  rating_curve_segment_id = "seg1",
  segment_number = 1,
  valid_from = as.Date("2019-01-01"),
  valid_to = as.Date("2021-12-31"),
  stage_min_cm = 50,
  stage_max_cm = 200,
  coefficient_a = 10,
  coefficient_h0_cm = 0,
  coefficient_n = 1
)

diagnose_station_hydrometry(measurements, curves, detailed = FALSE)$summary
#>   station_code n_measurements n_valid_measurements n_stage_zero_or_negative
#> 1          001              2                    2                        0
#>   pct_stage_zero_or_negative n_discharge_zero_or_negative
#> 1                          0                            0
#>   pct_discharge_zero_or_negative n_repeated_stage_variable_discharge_points
#> 1                              0                                          0
#>   pct_repeated_stage_variable_discharge_points
#> 1                                            0
#>   n_repeated_discharge_variable_stage_points
#> 1                                          0
#>   pct_repeated_discharge_variable_stage_points n_rating_curves
#> 1                                            0               1
#>   n_rating_curve_segments rating_match_fraction median_abs_rating_log_residual
#> 1                       1                     0                             NA
#>   outside_residual_envelope_fraction n_temporal_regimes
#> 1                                 NA                 NA
#>   temporal_regime_evidence_class baseline_power_equation baseline_power_h0_m
#> 1                  not_available                    <NA>                  NA
#>   baseline_power_a baseline_power_b diagnostic_attention_score
#> 1               NA               NA                          1
#>   diagnostic_attention_class diagnostic_detail_level
#> 1              low_attention   light_station_summary
#>   hydrometry_reference_source hydrometry_reference_snapshot
#> 1               user_supplied                          <NA>
#>   n_reference_measurements first_reference_measurement_date
#> 1                        2                       2020-01-01
#>   last_reference_measurement_date n_reference_measurement_years
#> 1                      2020-02-01                             1
#>   min_reference_measured_stage_cm max_reference_measured_stage_cm
#> 1                             100                             120
#>   min_reference_measured_discharge_m3s max_reference_measured_discharge_m3s
#> 1                                   10                                   15
#>   n_reference_rating_curves n_reference_rating_curve_segments
#> 1                         1                                 1
#>   first_reference_rating_curve_date last_reference_rating_curve_date
#> 1                        2019-01-01                       2021-12-31
#>   min_reference_rating_stage_cm max_reference_rating_stage_cm
#> 1                            50                           200
```
