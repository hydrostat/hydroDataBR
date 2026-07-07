# Ler seções transversais de arquivos HidroWeb

Lê o arquivo `PerfilTransversal.csv` exportado pelo HidroWeb,
diretamente ou dentro de um ZIP, e retorna duas tabelas padronizadas:
uma com as seções e outra com os vértices do perfil transversal.

## Usage

``` r
read_hidroweb_cross_sections(file)
```

## Arguments

- file:

  Caminho para um arquivo `.zip` do HidroWeb ou para um arquivo
  `PerfilTransversal.csv` já extraído.

## Value

Lista com os elementos `sections` e `vertices`.

## Examples

``` r
# Exemplo com arquivo local do HidroWeb.
if (FALSE) {
  secoes <- read_hidroweb_cross_sections("Estacao_56460000.zip")
  plot_hydro_data(secoes, plot = "cross_section_profile")
}
```
