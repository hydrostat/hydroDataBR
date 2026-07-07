# Ler séries diárias de JSON da ANA

Lê um arquivo JSON local com registros de série histórica da ANA,
incluindo respostas que usam o campo `items`, e converte os dados para o
contrato diário padronizado do hydroDataBR.

## Usage

``` r
read_ana_json(
  path,
  variables = c("all", "discharge", "stage", "rainfall"),
  merge_consistency = TRUE
)
```

## Arguments

- path:

  Caminho local para um arquivo JSON.

- variables:

  Variáveis a retornar: `"all"`, `"discharge"`, `"stage"` ou
  `"rainfall"`. É possível informar mais de uma variável.

- merge_consistency:

  Se `TRUE`, mantém uma linha por estação, data e variável, priorizando
  dados consistidos quando existirem.

## Value

`data.frame` com a série diária padronizada.

## Examples

``` r
# Exemplo com arquivo JSON local.
if (FALSE) {
  dados <- read_ana_json("serie_historica_56460000.json")
  analyze_hydro_data(dados, analysis = "daily_availability")
}
```
