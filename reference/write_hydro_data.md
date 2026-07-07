# Exportar dados, tabelas e gráficos hidrológicos

Exporta produtos gerados pelo hydroDataBR para arquivos locais. A função
pode gravar séries padronizadas, tabelas finais, relatórios de
requisição, resultados de análise e gráficos `ggplot`, dependendo do
objeto informado e do formato escolhido.

## Usage

``` r
write_hydro_data(
  x,
  path,
  format = c("csv", "rds", "png", "pdf"),
  components = "all",
  overwrite = FALSE,
  manifest = FALSE,
  hydrological_year_start = 10L,
  low_flow_durations = c(3L, 7L, 15L, 30L),
  ...
)
```

## Arguments

- x:

  Objeto a exportar. Pode ser série diária padronizada, objeto de
  aquisição, lote, tabela, lista de tabelas ou gráfico.

- path:

  Caminho de saída. Para múltiplos componentes, informe um diretório.
  Para um único arquivo, informe o caminho completo com extensão.

- format:

  Formato de exportação: `"csv"`, `"rds"`, `"png"` ou `"pdf"`. Se
  omitido, a extensão de `path` é usada quando reconhecida; caso
  contrário, o padrão é `"csv"`.

- components:

  Componentes a exportar. Use `"all"` para gravar todos os componentes
  disponíveis, ou informe nomes/aliases como `Tab01`, `Tab02` e outros
  componentes definidos pelo pacote.

- overwrite:

  Se `TRUE`, sobrescreve arquivos existentes.

- manifest:

  Se `TRUE` e `path` for um diretório, grava também um arquivo
  `hydro_export_manifest.csv` com os arquivos produzidos.

- hydrological_year_start:

  Mês inicial do ano hidrológico usado em tabelas de máximos anuais. O
  padrão é 10, representando outubro.

- low_flow_durations:

  Durações, em dias, usadas em tabelas de mínimas móveis anuais.

- ...:

  Argumentos adicionais. Para gráficos, são repassados a
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  quando aplicável.

## Value

Invisivelmente, um `data.frame` com os arquivos gravados.

## Details

A função procura componentes exportáveis dentro do objeto informado. Em
séries diárias, aplica a regra usual do pacote: dado consistido tem
prioridade; na ausência dele, é usado o dado não consistido disponível.
Para objetos de aquisição agregada, a função pode exportar séries,
tabelas derivadas e relatórios. Para gráficos, use formatos como `png`
ou `pdf`.

## Examples

``` r
daily <- data.frame(
  station_code = "001",
  date = as.Date("2020-01-01") + 0:29,
  variable = "discharge",
  value = seq(10, 39),
  unit = "m3/s",
  consistency_level = NA_integer_,
  source_status = NA_character_,
  source = "example"
)

# Exemplo de exportacao local.
if (FALSE) {
  write_hydro_data(
    daily,
    path = "saida_hydrodatabr",
    components = "all",
    overwrite = TRUE,
    manifest = TRUE
  )
}
```
