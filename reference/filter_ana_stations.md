# Filtrar o inventário embutido de estações ANA

Filtra o conjunto `ana_stations`, incluído no pacote, ou uma tabela de
estações fornecida pelo usuário. A função ajuda a localizar postos por
código, estado, município, tipo de estação, bacia, nome, situação
operacional e disponibilidade de produtos hidrológicos.

## Usage

``` r
filter_ana_stations(
  station_data = NULL,
  station_code = NULL,
  state_code = NULL,
  municipality = NULL,
  station_type = NULL,
  basin_code = NULL,
  name_pattern = NULL,
  product = NULL,
  is_operating = NULL
)
```

## Arguments

- station_data:

  Tabela de metadados de estações. Se `NULL`, usa o inventário embutido
  `ana_stations`.

- station_code:

  Código(s) de estação.

- state_code:

  Sigla(s) de unidade federativa.

- municipality:

  Nome(s) de município para filtro exato.

- station_type:

  Tipo(s) de estação.

- basin_code:

  Código(s) de bacia.

- name_pattern:

  Texto ou expressão regular para busca no nome da estação.

- product:

  Produto cuja disponibilidade é exigida. Valores aceitos:
  `"discharge"`, `"stage"`, `"rainfall"`, `"telemetry"` e
  `"discharge_measurements"`.

- is_operating:

  Valor lógico para filtrar estações em operação.

## Value

`data.frame` com as estações que atendem aos filtros informados.

## Examples

``` r
estacoes_mg <- filter_ana_stations(
  state_code = "MG",
  product = "discharge"
)
head(estacoes_mg)
#>   station_code     station_name  station_type state_code       municipality
#> 1     40025000    VARGEM BONITA Fluviometrica         MG      VARGEM BONITA
#> 2     40027000         IGUATAMA Fluviometrica         MG           IGUATAMA
#> 3     40030000 FAZENDA DA BARCA Fluviometrica         MG SÃO ROQUE DE MINAS
#> 4     40032000  FAZENDA SAMBURÁ Fluviometrica         MG SÃO ROQUE DE MINAS
#> 5     40034000 FAZENDA DA BARRA Fluviometrica         MG SÃO ROQUE DE MINAS
#> 6     40035000  FAZENDA SAMBURA Fluviometrica         MG SÃO ROQUE DE MINAS
#>   basin_code        basin_name  latitude longitude altitude_m drainage_area_km2
#> 1          4 RIO SÃO FRANCISCO -20.32720 -46.36610        744               299
#> 2          4 RIO SÃO FRANCISCO -20.17237 -45.71617         NA                NA
#> 3          4 RIO SÃO FRANCISCO -20.10000 -46.31667        762               725
#> 4          4 RIO SÃO FRANCISCO -20.15080 -46.30330        734               754
#> 5          4 RIO SÃO FRANCISCO -20.18333 -46.21667         NA               830
#> 6          4 RIO SÃO FRANCISCO -20.15000 -46.33333        703               542
#>   operator responsible_agency is_operating discharge_start_date
#> 1 SGB-CPRM                ANA         TRUE           1939-06-01
#> 2  IGAM-MG            IGAM-MG         TRUE           2019-01-01
#> 3      ANA                ANA        FALSE           1939-06-01
#> 4 SGB-CPRM                ANA         TRUE           1964-10-01
#> 5      ANA                ANA        FALSE           1949-10-01
#> 6      ANA                ANA        FALSE           1939-06-01
#>   discharge_end_date telemetric_start_date telemetric_end_date stage_start_date
#> 1               <NA>                  <NA>                <NA>       1939-06-01
#> 2               <NA>                  <NA>                <NA>             <NA>
#> 3         1968-10-01                  <NA>                <NA>       1939-06-01
#> 4               <NA>                  <NA>                <NA>       1964-10-01
#> 5         1965-01-01                  <NA>                <NA>       1949-10-01
#> 6         1964-09-01                  <NA>                <NA>       1939-06-01
#>   stage_end_date rainfall_start_date rainfall_end_date
#> 1           <NA>                <NA>              <NA>
#> 2           <NA>                <NA>              <NA>
#> 3     1968-10-01                <NA>              <NA>
#> 4           <NA>                <NA>              <NA>
#> 5     1965-01-01                <NA>              <NA>
#> 6     1964-09-01                <NA>              <NA>
#>   has_discharge_measurements has_telemetry has_stage_data has_rainfall_data
#> 1                       TRUE         FALSE           TRUE             FALSE
#> 2                       TRUE         FALSE          FALSE             FALSE
#> 3                       TRUE         FALSE           TRUE             FALSE
#> 4                       TRUE         FALSE           TRUE             FALSE
#> 5                       TRUE         FALSE           TRUE             FALSE
#> 6                       TRUE         FALSE           TRUE             FALSE
#>   last_update
#> 1  2024-10-15
#> 2  2025-10-10
#> 3  2010-02-22
#> 4  2019-11-05
#> 5  2010-02-22
#> 6  2010-02-22
```
