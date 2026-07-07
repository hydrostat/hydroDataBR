# Ler séries diárias de arquivos HidroWeb

Lê arquivos locais CSV ou ZIP baixados do HidroWeb e converte as séries
diárias para o contrato padronizado do hydroDataBR. A função não acessa
a internet; ela trabalha apenas com arquivos já disponíveis no
computador.

## Usage

``` r
read_hidroweb(
  path,
  variables = c("all", "discharge", "stage", "rainfall"),
  merge_consistency = TRUE
)
```

## Arguments

- path:

  Caminho local para um arquivo CSV ou ZIP do HidroWeb.

- variables:

  Variáveis a retornar: `"all"`, `"discharge"`, `"stage"` ou
  `"rainfall"`. É possível informar mais de uma variável.

- merge_consistency:

  Se `TRUE`, mantém uma linha por estação, data e variável, priorizando
  dados consistidos (`NivelConsistencia = 2`) quando existirem. Na
  ausência de dado consistido, usa o dado não consistido disponível.

## Value

`data.frame` com as colunas `station_code`, `date`, `variable`, `value`,
`unit`, `consistency_level`, `source_status` e `source`.

## Examples

``` r
# Exemplo com arquivo local ja baixado do HidroWeb.
if (FALSE) {
  dados <- read_hidroweb("Estacao_56460000.zip")
  plot_hydro_data(dados, plot = "daily_series")
}
```
