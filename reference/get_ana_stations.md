# Consultar o inventário de estações da ANA

Consulta o inventário de estações pelo serviço autenticado da ANA e
retorna metadados padronizados dos postos encontrados. A função pode ser
usada para buscar uma estação específica, estações de uma unidade
federativa ou estações associadas a uma bacia.

## Usage

``` r
get_ana_stations(
  station_code = NULL,
  state_code = NULL,
  basin_code = NULL,
  token = NULL,
  ...,
  data_source = "api",
  source = data_source
)
```

## Arguments

- station_code:

  Código(s) de estação ANA, como texto ou número.

- state_code:

  Sigla(s) de unidade federativa, como `"MG"` ou `"SP"`.

- basin_code:

  Código(s) de bacia.

- token:

  Token retornado por
  [`ana_authenticate()`](https://hydrostat.github.io/hydroDataBR/reference/ana_authenticate.md).
  Se `NULL`, a função tenta autenticar com credenciais disponíveis nos
  argumentos ou no ambiente.

- ...:

  Argumentos adicionais usados internamente, como `timeout`,
  `max_attempts`, `retry_sleep_seconds` e opções de teste.

- data_source:

  Fonte dos dados. Na prática, esta função usa a rota autenticada de
  inventário da ANA.

- source:

  Alias de compatibilidade para `data_source`.

## Value

`data.frame` com metadados padronizados das estações retornadas pela
ANA.

## Examples

``` r
# Exemplo operacional. Requer credenciais validas da ANA.
if (FALSE) {
  token <- ana_authenticate()
  estacoes <- get_ana_stations(state_code = "MG", token = token)
  head(estacoes)
}
```
