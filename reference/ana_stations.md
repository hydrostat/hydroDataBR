# Inventario embutido de estacoes hidrometeorologicas da ANA

Snapshot local de metadados principais de estacoes, derivado da tabela
`stations_minimal` do banco `shiny_minimal.duckdb` do projeto
`ana_api_get_clean`. O objeto serve para consultas locais rapidas,
filtros por posto e apoio a funcoes de analise e visualizacao. Para
obter dados atualizados diretamente do servico da ANA, use
[`get_ana_stations()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_stations.md)
ou
[`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md).

## Usage

``` r
data(ana_stations)
```

## Format

Um data frame com 37.584 linhas e 27 colunas:

- station_code:

  Codigo da estacao.

- station_name:

  Nome da estacao.

- station_type:

  Tipo da estacao.

- state_code:

  Sigla da unidade federativa.

- municipality:

  Nome do municipio.

- basin_code:

  Codigo da bacia hidrografica.

- basin_name:

  Nome da bacia hidrografica.

- latitude:

  Latitude da estacao, em graus decimais.

- longitude:

  Longitude da estacao, em graus decimais.

- altitude_m:

  Altitude da estacao, em metros, quando disponivel.

- drainage_area_km2:

  Area de drenagem, em km2, quando disponivel.

- operator:

  Entidade operadora registrada.

- responsible_agency:

  Entidade responsavel registrada.

- is_operating:

  Indicador logico de operacao da estacao.

- discharge_start_date:

  Data inicial disponivel para vazao diaria.

- discharge_end_date:

  Data final disponivel para vazao diaria.

- telemetric_start_date:

  Data inicial disponivel para dados telemetricos.

- telemetric_end_date:

  Data final disponivel para dados telemetricos.

- stage_start_date:

  Data inicial disponivel para cota diaria.

- stage_end_date:

  Data final disponivel para cota diaria.

- rainfall_start_date:

  Data inicial disponivel para chuva diaria.

- rainfall_end_date:

  Data final disponivel para chuva diaria.

- has_discharge_measurements:

  Indica presenca de medicoes de descarga.

- has_telemetry:

  Indica presenca de dados telemetricos.

- has_stage_data:

  Indica presenca de dados de cota.

- has_rainfall_data:

  Indica presenca de dados de chuva.

- last_update:

  Data da ultima atualizacao registrada no snapshot.

## Source

Banco `shiny_minimal.duckdb`, tabela `stations_minimal`, do projeto
`ana_api_get_clean`.
