# Obter inventario de estacoes da ANA

Consulta o inventario de estacoes no HidroWebService da ANA e retorna
uma tabela padronizada com campos essenciais de identificacao,
localizacao e classificacao das estacoes.

## Usage

``` r
ana_get_stations_impl(
  token = NULL,
  station_code = NULL,
  state_code = NULL,
  basin_code = NULL,
  updated_from = NULL,
  updated_to = NULL
)
```

## Arguments

- token:

  Objeto de token retornado por
  [`ana_authenticate()`](https://hydrostat.github.io/hydroDataBR/reference/ana_authenticate.md).

- station_code:

  Codigo da estacao. Mantido como texto.

- state_code:

  Sigla da unidade federativa, como `MG` ou `SP`.

- basin_code:

  Codigo da bacia hidrografica usado pelo HidroWebService.

- updated_from:

  Data inicial de atualizacao no formato `YYYY-MM-DD`.

- updated_to:

  Data final de atualizacao no formato `YYYY-MM-DD`.

## Value

Um tibble com campos padronizados de inventario de estacoes.

## Details

Pelo menos um filtro deve ser informado: `station_code`, `state_code` ou
`basin_code`.
