# Obter dados ANA por produto e origem

Funcao geral para obter dados ANA. A funcao pode ler arquivos locais ou
obter dados do HidroWebService, dependendo de `data_source`. O argumento
`station_code` aceita um codigo ou um vetor de codigos quando o produto
depende de estacao.

## Usage

``` r
.hydrodatabr_original_get_ana_data(
  product,
  data_source = "webservice",
  station_code = NULL,
  path = NULL,
  ...
)
```

## Arguments

- product:

  Produto desejado. Exemplos: `daily_discharge`, `daily_stage`,
  `daily_rainfall`, `stations`, `discharge_measurements`,
  `rating_curves`, `cross_sections`, `states`, `municipalities`,
  `basins`, `subbasins`, `rivers`, `entities` ou `all`.

- data_source:

  Origem dos dados. Use `webservice`, `api`, `hidroweb`, `xml` ou
  `json`.

- station_code:

  Codigo da estacao como texto, vetor de codigos, ou `NULL` para
  produtos que nao dependem de estacao.

- path:

  Caminho para arquivo local quando `data_source` nao for `webservice`
  ou `api`.

- ...:

  Argumentos adicionais repassados para a funcao especifica.

## Value

Objeto R com dados padronizados.
