# Ler séries diárias de XML da ANA

Lê um arquivo XML local da operação `HidroSerieHistorica` do WebService
legado da ANA e retorna uma série diária no contrato padronizado do
pacote. A função é indicada quando o usuário já possui o XML salvo em
disco.

## Usage

``` r
read_ana_xml(
  path,
  variables = c("all", "discharge", "stage", "rainfall"),
  merge_consistency = TRUE
)
```

## Arguments

- path:

  Caminho local para um arquivo XML.

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
# Exemplo com arquivo XML local.
if (FALSE) {
  dados <- read_ana_xml("HidroSerieHistorica_56460000.xml")
  table_hydro_data(dados, table = "daily_availability")
}
```
