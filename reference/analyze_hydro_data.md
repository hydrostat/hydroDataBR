# Analisar dados hidrológicos padronizados

Calcula estatísticas, indicadores e diagnósticos básicos a partir de
séries diárias padronizadas ou de objetos retornados por funções de
aquisição do pacote. A função não baixa dados e não acessa serviços
externos; ela trabalha apenas com objetos já carregados na sessão.

## Usage

``` r
analyze_hydro_data(
  data,
  analysis = c("daily_statistics", "daily_availability", "flow_duration", "flow_indices",
    "monthly_flow_indices", "rainfall_indices", "annual_maxima",
    "rainfall_annual_maxima", "low_flows", "rainfall_diagnostics", "daily_gap_summary",
    "hydrometry_diagnostics", "measurement_diagnostics", "rating_diagnostics"),
  variable = NULL,
  station_code = NULL,
  period = "annual",
  year_start_month = 1L,
  durations = c(1L, 3L, 7L, 15L, 30L),
  wet_day_threshold = 1,
  exceedance_probabilities = 1:99,
  complete_years_only = TRUE,
  ...
)
```

## Arguments

- data:

  Objeto de dados. Pode ser uma série diária padronizada, um objeto
  retornado por
  [`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md),
  ou um lote retornado por
  [`get_ana_data_batch()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data_batch.md).

- analysis:

  Tipo de análise. Valores aceitos incluem `"daily_statistics"`,
  `"daily_availability"`, `"flow_duration"`, `"flow_indices"`,
  `"monthly_flow_indices"`, `"annual_maxima"`, `"low_flows"`,
  `"rainfall_indices"`, `"rainfall_annual_maxima"`,
  `"rainfall_diagnostics"`, `"daily_gap_summary"`,
  `"hydrometry_diagnostics"`, `"measurement_diagnostics"` e
  `"rating_diagnostics"`.

- variable:

  Variável diária a filtrar, como `"discharge"`, `"stage"` ou
  `"rainfall"`. Quando `NULL`, a função usa a variável adequada para a
  análise escolhida.

- station_code:

  Código(s) de estação a filtrar.

- period:

  Período usado em algumas análises agregadas.

- year_start_month:

  Mês inicial do ano de análise. Use 1 para ano civil e 10 para ano
  hidrológico de outubro a setembro.

- durations:

  Durações, em dias, usadas para mínimas móveis anuais.

- wet_day_threshold:

  Limiar, em mm, para definir dia chuvoso nos índices pluviométricos.

- exceedance_probabilities:

  Probabilidades de permanência, em porcentagem, usadas na curva de
  permanência. O padrão retorna 1 a 99%.

- complete_years_only:

  Se `TRUE`, remove anos incompletos em análises de máximos anuais e
  mínimas anuais.

- ...:

  Argumentos adicionais usados por análises específicas.

## Value

Em geral, um `data.frame`. Algumas análises técnicas podem retornar
listas com tabelas auxiliares.

## Details

A função foi pensada para o fluxo típico de engenharia hidrológica:
obter ou ler os dados, padronizar a série diária e então calcular
indicadores de disponibilidade, regime, permanência, extremos e
consistência. Para vazões, as análises incluem curva de permanência,
QMLT, Q90, Q95, índices mensais, máximos anuais e mínimas móveis. Para
chuva, incluem índices anuais, máximos anuais e diagnósticos de
falhas/consistência. Diagnósticos fluviométricos podem usar a base
hidrométrica interna do pacote quando as referências não são fornecidas
explicitamente.

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

analyze_hydro_data(daily, analysis = "flow_indices")
#>   station_code  variable unit n_days n_values missing_days qmlt      q90   q95
#> 1          001 discharge m3/s     30       30            0 24.5 12.36667 10.85
#>   min_value max_value
#> 1        10        39
analyze_hydro_data(daily, analysis = "daily_availability")
#>   station_code  variable year period days_expected days_observed
#> 1          001 discharge 2020   2020            30            30
#>   days_with_value days_missing availability_pct missing_pct
#> 1              30            0              100           0

# A mesma interface tambem pode ser usada com objetos de aquisicao.
objeto <- list(daily_data = daily)
analyze_hydro_data(objeto, analysis = "flow_duration")
#>    station_code  variable unit permanence_pct exceedance_probability
#> 1           001 discharge m3/s              1                      1
#> 2           001 discharge m3/s              2                      2
#> 3           001 discharge m3/s              3                      3
#> 4           001 discharge m3/s              4                      4
#> 5           001 discharge m3/s              5                      5
#> 6           001 discharge m3/s              6                      6
#> 7           001 discharge m3/s              7                      7
#> 8           001 discharge m3/s              8                      8
#> 9           001 discharge m3/s              9                      9
#> 10          001 discharge m3/s             10                     10
#> 11          001 discharge m3/s             11                     11
#> 12          001 discharge m3/s             12                     12
#> 13          001 discharge m3/s             13                     13
#> 14          001 discharge m3/s             14                     14
#> 15          001 discharge m3/s             15                     15
#> 16          001 discharge m3/s             16                     16
#> 17          001 discharge m3/s             17                     17
#> 18          001 discharge m3/s             18                     18
#> 19          001 discharge m3/s             19                     19
#> 20          001 discharge m3/s             20                     20
#> 21          001 discharge m3/s             21                     21
#> 22          001 discharge m3/s             22                     22
#> 23          001 discharge m3/s             23                     23
#> 24          001 discharge m3/s             24                     24
#> 25          001 discharge m3/s             25                     25
#> 26          001 discharge m3/s             26                     26
#> 27          001 discharge m3/s             27                     27
#> 28          001 discharge m3/s             28                     28
#> 29          001 discharge m3/s             29                     29
#> 30          001 discharge m3/s             30                     30
#> 31          001 discharge m3/s             31                     31
#> 32          001 discharge m3/s             32                     32
#> 33          001 discharge m3/s             33                     33
#> 34          001 discharge m3/s             34                     34
#> 35          001 discharge m3/s             35                     35
#> 36          001 discharge m3/s             36                     36
#> 37          001 discharge m3/s             37                     37
#> 38          001 discharge m3/s             38                     38
#> 39          001 discharge m3/s             39                     39
#> 40          001 discharge m3/s             40                     40
#> 41          001 discharge m3/s             41                     41
#> 42          001 discharge m3/s             42                     42
#> 43          001 discharge m3/s             43                     43
#> 44          001 discharge m3/s             44                     44
#> 45          001 discharge m3/s             45                     45
#> 46          001 discharge m3/s             46                     46
#> 47          001 discharge m3/s             47                     47
#> 48          001 discharge m3/s             48                     48
#> 49          001 discharge m3/s             49                     49
#> 50          001 discharge m3/s             50                     50
#> 51          001 discharge m3/s             51                     51
#> 52          001 discharge m3/s             52                     52
#> 53          001 discharge m3/s             53                     53
#> 54          001 discharge m3/s             54                     54
#> 55          001 discharge m3/s             55                     55
#> 56          001 discharge m3/s             56                     56
#> 57          001 discharge m3/s             57                     57
#> 58          001 discharge m3/s             58                     58
#> 59          001 discharge m3/s             59                     59
#> 60          001 discharge m3/s             60                     60
#> 61          001 discharge m3/s             61                     61
#> 62          001 discharge m3/s             62                     62
#> 63          001 discharge m3/s             63                     63
#> 64          001 discharge m3/s             64                     64
#> 65          001 discharge m3/s             65                     65
#> 66          001 discharge m3/s             66                     66
#> 67          001 discharge m3/s             67                     67
#> 68          001 discharge m3/s             68                     68
#> 69          001 discharge m3/s             69                     69
#> 70          001 discharge m3/s             70                     70
#> 71          001 discharge m3/s             71                     71
#> 72          001 discharge m3/s             72                     72
#> 73          001 discharge m3/s             73                     73
#> 74          001 discharge m3/s             74                     74
#> 75          001 discharge m3/s             75                     75
#> 76          001 discharge m3/s             76                     76
#> 77          001 discharge m3/s             77                     77
#> 78          001 discharge m3/s             78                     78
#> 79          001 discharge m3/s             79                     79
#> 80          001 discharge m3/s             80                     80
#> 81          001 discharge m3/s             81                     81
#> 82          001 discharge m3/s             82                     82
#> 83          001 discharge m3/s             83                     83
#> 84          001 discharge m3/s             84                     84
#> 85          001 discharge m3/s             85                     85
#> 86          001 discharge m3/s             86                     86
#> 87          001 discharge m3/s             87                     87
#> 88          001 discharge m3/s             88                     88
#> 89          001 discharge m3/s             89                     89
#> 90          001 discharge m3/s             90                     90
#> 91          001 discharge m3/s             91                     91
#> 92          001 discharge m3/s             92                     92
#> 93          001 discharge m3/s             93                     93
#> 94          001 discharge m3/s             94                     94
#> 95          001 discharge m3/s             95                     95
#> 96          001 discharge m3/s             96                     96
#> 97          001 discharge m3/s             97                     97
#> 98          001 discharge m3/s             98                     98
#> 99          001 discharge m3/s             99                     99
#>    non_exceedance_probability value n_values interpolation_method
#> 1                          99 39.00       30 linear_empirical_fdc
#> 2                          98 39.00       30 linear_empirical_fdc
#> 3                          97 39.00       30 linear_empirical_fdc
#> 4                          96 38.76       30 linear_empirical_fdc
#> 5                          95 38.45       30 linear_empirical_fdc
#> 6                          94 38.14       30 linear_empirical_fdc
#> 7                          93 37.83       30 linear_empirical_fdc
#> 8                          92 37.52       30 linear_empirical_fdc
#> 9                          91 37.21       30 linear_empirical_fdc
#> 10                         90 36.90       30 linear_empirical_fdc
#> 11                         89 36.59       30 linear_empirical_fdc
#> 12                         88 36.28       30 linear_empirical_fdc
#> 13                         87 35.97       30 linear_empirical_fdc
#> 14                         86 35.66       30 linear_empirical_fdc
#> 15                         85 35.35       30 linear_empirical_fdc
#> 16                         84 35.04       30 linear_empirical_fdc
#> 17                         83 34.73       30 linear_empirical_fdc
#> 18                         82 34.42       30 linear_empirical_fdc
#> 19                         81 34.11       30 linear_empirical_fdc
#> 20                         80 33.80       30 linear_empirical_fdc
#> 21                         79 33.49       30 linear_empirical_fdc
#> 22                         78 33.18       30 linear_empirical_fdc
#> 23                         77 32.87       30 linear_empirical_fdc
#> 24                         76 32.56       30 linear_empirical_fdc
#> 25                         75 32.25       30 linear_empirical_fdc
#> 26                         74 31.94       30 linear_empirical_fdc
#> 27                         73 31.63       30 linear_empirical_fdc
#> 28                         72 31.32       30 linear_empirical_fdc
#> 29                         71 31.01       30 linear_empirical_fdc
#> 30                         70 30.70       30 linear_empirical_fdc
#> 31                         69 30.39       30 linear_empirical_fdc
#> 32                         68 30.08       30 linear_empirical_fdc
#> 33                         67 29.77       30 linear_empirical_fdc
#> 34                         66 29.46       30 linear_empirical_fdc
#> 35                         65 29.15       30 linear_empirical_fdc
#> 36                         64 28.84       30 linear_empirical_fdc
#> 37                         63 28.53       30 linear_empirical_fdc
#> 38                         62 28.22       30 linear_empirical_fdc
#> 39                         61 27.91       30 linear_empirical_fdc
#> 40                         60 27.60       30 linear_empirical_fdc
#> 41                         59 27.29       30 linear_empirical_fdc
#> 42                         58 26.98       30 linear_empirical_fdc
#> 43                         57 26.67       30 linear_empirical_fdc
#> 44                         56 26.36       30 linear_empirical_fdc
#> 45                         55 26.05       30 linear_empirical_fdc
#> 46                         54 25.74       30 linear_empirical_fdc
#> 47                         53 25.43       30 linear_empirical_fdc
#> 48                         52 25.12       30 linear_empirical_fdc
#> 49                         51 24.81       30 linear_empirical_fdc
#> 50                         50 24.50       30 linear_empirical_fdc
#> 51                         49 24.19       30 linear_empirical_fdc
#> 52                         48 23.88       30 linear_empirical_fdc
#> 53                         47 23.57       30 linear_empirical_fdc
#> 54                         46 23.26       30 linear_empirical_fdc
#> 55                         45 22.95       30 linear_empirical_fdc
#> 56                         44 22.64       30 linear_empirical_fdc
#> 57                         43 22.33       30 linear_empirical_fdc
#> 58                         42 22.02       30 linear_empirical_fdc
#> 59                         41 21.71       30 linear_empirical_fdc
#> 60                         40 21.40       30 linear_empirical_fdc
#> 61                         39 21.09       30 linear_empirical_fdc
#> 62                         38 20.78       30 linear_empirical_fdc
#> 63                         37 20.47       30 linear_empirical_fdc
#> 64                         36 20.16       30 linear_empirical_fdc
#> 65                         35 19.85       30 linear_empirical_fdc
#> 66                         34 19.54       30 linear_empirical_fdc
#> 67                         33 19.23       30 linear_empirical_fdc
#> 68                         32 18.92       30 linear_empirical_fdc
#> 69                         31 18.61       30 linear_empirical_fdc
#> 70                         30 18.30       30 linear_empirical_fdc
#> 71                         29 17.99       30 linear_empirical_fdc
#> 72                         28 17.68       30 linear_empirical_fdc
#> 73                         27 17.37       30 linear_empirical_fdc
#> 74                         26 17.06       30 linear_empirical_fdc
#> 75                         25 16.75       30 linear_empirical_fdc
#> 76                         24 16.44       30 linear_empirical_fdc
#> 77                         23 16.13       30 linear_empirical_fdc
#> 78                         22 15.82       30 linear_empirical_fdc
#> 79                         21 15.51       30 linear_empirical_fdc
#> 80                         20 15.20       30 linear_empirical_fdc
#> 81                         19 14.89       30 linear_empirical_fdc
#> 82                         18 14.58       30 linear_empirical_fdc
#> 83                         17 14.27       30 linear_empirical_fdc
#> 84                         16 13.96       30 linear_empirical_fdc
#> 85                         15 13.65       30 linear_empirical_fdc
#> 86                         14 13.34       30 linear_empirical_fdc
#> 87                         13 13.03       30 linear_empirical_fdc
#> 88                         12 12.72       30 linear_empirical_fdc
#> 89                         11 12.41       30 linear_empirical_fdc
#> 90                         10 12.10       30 linear_empirical_fdc
#> 91                          9 11.79       30 linear_empirical_fdc
#> 92                          8 11.48       30 linear_empirical_fdc
#> 93                          7 11.17       30 linear_empirical_fdc
#> 94                          6 10.86       30 linear_empirical_fdc
#> 95                          5 10.55       30 linear_empirical_fdc
#> 96                          4 10.24       30 linear_empirical_fdc
#> 97                          3 10.00       30 linear_empirical_fdc
#> 98                          2 10.00       30 linear_empirical_fdc
#> 99                          1 10.00       30 linear_empirical_fdc
```
