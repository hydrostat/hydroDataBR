# Obter secoes transversais da ANA em lote

Consulta secoes transversais para varias estacoes e retorna tabelas
padronizadas de secoes e vertices junto com um relatorio de requisicoes.

## Usage

``` r
get_ana_cross_sections_batch(
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

Uma lista com `data` e `request_report`. O elemento `data` contem os
tibbles `sections` e `vertices`.
