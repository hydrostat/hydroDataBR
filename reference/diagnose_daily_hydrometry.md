# Diagnosticar consistência diária entre vazão, cota e curva-chave

Compara séries diárias de vazão e cota com os segmentos de curva-chave
disponíveis. A função identifica lacunas cruzadas entre vazão e cota,
valores não positivos, períodos sem curva aplicável, cotas fora da faixa
de validade da curva e diferenças entre a vazão observada e a vazão
estimada pela curva-chave.

## Usage

``` r
diagnose_daily_hydrometry(
  daily_data,
  rating_curves = data.frame(),
  relative_error_threshold_pct = 20,
  measurements = data.frame(),
  rating_curve_summary = data.frame(),
  station_code = NULL,
  use_internal_database = TRUE
)
```

## Arguments

- daily_data:

  Série diária no contrato padronizado do pacote. Deve conter pelo menos
  `station_code`, `date`, `variable` e `value`. Linhas com
  `variable = "discharge"` são tratadas como vazão em m3/s; linhas com
  `variable = "stage"` são tratadas como cota em cm.

- rating_curves:

  Tabela de curvas-chave com datas de validade, limites de cota e
  coeficientes da equação. A coluna `coefficient_h0_m` também é aceita
  por compatibilidade.

- relative_error_threshold_pct:

  Limiar absoluto, em porcentagem, para sinalizar diferença elevada
  entre vazão observada e vazão calculada pela curva-chave.

- measurements:

  Tabela opcional de medições de descarga. Quando ausente, a função
  tenta usar medições presentes em objetos agregados e, por fim, a base
  hidrométrica interna do pacote.

- rating_curve_summary:

  Tabela opcional de resumo das curvas-chave.

- station_code:

  Código(s) de estação a filtrar.

- use_internal_database:

  Se `TRUE`, usa a base hidrométrica interna do pacote quando não houver
  referência fornecida pelo usuário nem no objeto de aquisição.

## Value

Lista com as tabelas `summary`, `indices`, `daily_flags`,
`rating_matches`, `rating_curve_coverage`, `measurement_year_summary`,
`hydrometry_reference_summary` e metadados da referência usada.

## Details

Quando medições de descarga ou curvas-chave não são fornecidas pelo
usuário nem estão presentes em um objeto agregado de
`get_ana_data(product = "all")`, a função consulta a base hidrométrica
interna do pacote, correspondente a um retrato da ANA de junho de 2026.
O objeto retornado informa a origem da referência hidrométrica usada.

## Examples

``` r
daily_data <- data.frame(
  station_code = "001",
  date = as.Date(c("2020-01-01", "2020-01-01")),
  variable = c("discharge", "stage"),
  value = c(10, 100)
)

rating_curves <- data.frame(
  station_code = "001",
  valid_from = as.Date("2019-01-01"),
  valid_to = as.Date("2021-12-31"),
  stage_min_cm = 50,
  stage_max_cm = 200,
  coefficient_a = 10,
  coefficient_h0_cm = 0,
  coefficient_n = 1
)

diagnostico <- diagnose_daily_hydrometry(daily_data, rating_curves)
diagnostico$summary
#>   station_code start_date   end_date expected_days discharge_observed_days
#> 1          001 2020-01-01 2020-01-01             1                       1
#>   stage_observed_days discharge_valid_days stage_valid_days
#> 1                   1                    1                1
#>   min_daily_discharge_m3s max_daily_discharge_m3s min_daily_stage_cm
#> 1                      10                      10                100
#>   max_daily_stage_cm discharge_without_stage_days stage_without_discharge_days
#> 1                100                            0                            0
#>   both_missing_days non_positive_discharge_days non_positive_stage_days
#> 1                 0                           0                       0
#>   days_with_rating_curve_date_coverage days_without_rating_curve_date_coverage
#> 1                                    1                                       0
#>   days_without_applicable_rating_segment days_with_stage_outside_curve_range
#> 1                                      0                                   0
#>   days_with_multiple_applicable_segments days_with_generated_discharge
#> 1                                      0                             1
#>   days_with_relative_error relative_error_threshold_pct
#> 1                        1                           20
#>   days_exceeding_relative_error_threshold mean_abs_relative_error_pct
#> 1                                       0                           0
#>   median_abs_relative_error_pct diagnostic_problem_days
#> 1                             0                       0
#>   hydrometry_reference_source hydrometry_reference_snapshot
#> 1               user_supplied                          <NA>
#>   n_reference_measurements first_reference_measurement_date
#> 1                        0                             <NA>
#>   last_reference_measurement_date n_reference_measurement_years
#> 1                            <NA>                             0
#>   min_reference_measured_stage_cm max_reference_measured_stage_cm
#> 1                              NA                              NA
#>   min_reference_measured_discharge_m3s max_reference_measured_discharge_m3s
#> 1                                   NA                                   NA
#>   n_reference_rating_curves n_reference_rating_curve_segments
#> 1                         1                                 1
#>   first_reference_rating_curve_date last_reference_rating_curve_date
#> 1                        2019-01-01                       2021-12-31
#>   min_reference_rating_stage_cm max_reference_rating_stage_cm
#> 1                            50                           200
```
