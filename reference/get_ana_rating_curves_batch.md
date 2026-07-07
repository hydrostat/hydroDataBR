# Obter curvas-chave da ANA em lote

Consulta curvas-chave para varias estacoes e retorna os dados
padronizados junto com um relatorio de requisicoes.

## Usage

``` r
get_ana_rating_curves_batch(
  station_codes,
  token = NULL,
  start_date = NULL,
  end_date = NULL,
  consistency_level = NULL
)
```

## Arguments

- station_codes:

  Vetor de codigos de estacoes.

- token:

  Objeto de token retornado por
  [`ana_authenticate()`](https://hydrostat.github.io/hydroDataBR/reference/ana_authenticate.md).

- start_date:

  Data inicial no formato `YYYY-MM-DD`.

- end_date:

  Data final no formato `YYYY-MM-DD`.

- consistency_level:

  Nivel de consistencia solicitado ao servico, quando aplicavel.

## Value

Uma lista com `data` e `request_report`.
