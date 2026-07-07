# Baixar vértices opcionais de seções transversais

Baixa o arquivo opcional com os vértices completos de seções
transversais da ANA incluído no repositório GitHub do pacote, mas não
instalado junto com o pacote principal.

## Usage

``` r
download_ana_cross_section_vertices(
  path = tools::R_user_dir("hydroDataBR", "cache"),
  version = "2026-06",
  read = TRUE,
  overwrite = FALSE,
  quiet = FALSE,
  url = NULL,
  metadata_url = NULL
)
```

## Arguments

- path:

  Caminho de diretório onde o arquivo será salvo. Por padrão, usa o
  diretório de cache do usuário para o pacote.

- version:

  Versão do retrato de dados. Atualmente apenas `"2026-06"` é suportado.

- read:

  Valor lógico. Se `TRUE`, lê e retorna o objeto R após baixar ou
  localizar o arquivo em cache. Se `FALSE`, retorna apenas o caminho
  local do arquivo.

- overwrite:

  Valor lógico. Se `TRUE`, força novo download mesmo quando o arquivo já
  existe no cache.

- quiet:

  Valor lógico. Se `TRUE`, reduz mensagens durante o download.

- url:

  URL alternativa ou caminho local alternativo para o arquivo `.rds`.
  Este argumento é principalmente útil para testes.

- metadata_url:

  URL alternativa ou caminho local alternativo para o arquivo de
  metadados `.csv`. Este argumento é principalmente útil para testes.

## Value

Se `read = TRUE`, um `data.frame` com os vértices de seções
transversais. Se `read = FALSE`, o caminho local do arquivo baixado.

## Details

O pacote principal inclui metadados e resumos de seções transversais,
mas não inclui os vértices completos dos perfis. Esta função permite
baixar esses vértices quando o usuário precisar reconstruir ou analisar
os perfis transversais completos.

O download usa cache local do usuário. A função não consulta serviços
vivos da ANA e não exige credenciais.

## Examples

``` r
if (FALSE) {
  vertices <- download_ana_cross_section_vertices()
  head(vertices)

  path <- download_ana_cross_section_vertices(read = FALSE)
  path
}
```
