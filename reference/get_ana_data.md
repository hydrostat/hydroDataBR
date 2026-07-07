# Obter dados hidrológicos da ANA

Obtém dados da ANA por uma interface única. A função cobre leituras
locais, consultas ao WebService legado e consultas à API autenticada,
sempre que a rota correspondente estiver disponível para o produto
solicitado.

## Usage

``` r
get_ana_data(
  product,
  data_source = "webservice",
  station_code = NULL,
  path = NULL,
  ...
)
```

## Arguments

- product:

  Produto solicitado. Valores usuais incluem `"daily_discharge"`,
  `"daily_stage"`, `"daily_rainfall"`, `"discharge_measurements"`,
  `"rating_curves"`, `"cross_sections"`, `"stations"`, `"states"`,
  `"municipalities"`, `"basins"`, `"subbasins"`, `"rivers"`,
  `"entities"` e `"all"`. Para arquivos locais, o produto diário pode
  ser inferido a partir do leitor usado.

- data_source:

  Origem dos dados. Use `"api"` para a API autenticada, `"webservice"`
  para o WebService online legado, `"xml"` para arquivo XML local da
  operação `HidroSerieHistorica`, ou `"hidroweb"` para arquivo CSV/ZIP
  local do HidroWeb.

- station_code:

  Código da estação ANA. Pode ser texto ou número; códigos com zeros à
  esquerda devem ser informados como texto.

- path:

  Caminho de arquivo local, quando `data_source` for `"xml"`,
  `"hidroweb"` ou outra fonte local suportada.

- ...:

  Argumentos adicionais usados pela rota escolhida. Os mais comuns são
  `start_date`, `end_date`, `token`, `source`, `include_cross_sections`,
  `variables`, `timeout`, `max_attempts` e opções de repetição de
  requisições.

## Value

O tipo de objeto depende do produto. Séries diárias retornam um
`data.frame` padronizado. Produtos especializados retornam tabelas ou
listas com tabelas. Aquisições agregadas retornam uma lista com os
produtos obtidos, a tabela diária combinada em `daily_data` e um
`request_report` com o estado de cada requisição.

## Details

Esta é a principal porta de entrada para aquisição de dados no
hydroDataBR. Para uso cotidiano, escolha o produto em `product`, a
origem em `data_source` ou `source`, e informe os demais argumentos
necessários, como código da estação, período ou caminho de arquivo
local.

Para séries diárias, o retorno segue o contrato padronizado do pacote,
com as colunas `station_code`, `date`, `variable`, `value`, `unit`,
`consistency_level`, `source_status` e `source`. Essa padronização
permite usar diretamente os resultados em
[`analyze_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/analyze_hydro_data.md),
[`plot_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/plot_hydro_data.md),
[`table_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/table_hydro_data.md)
e
[`write_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/write_hydro_data.md).

Com `product = "all"` e `data_source = "api"`, a função monta uma
aquisição agregada para uma estação. Para postos fluviométricos, são
avaliados dados de vazão diária, cota diária, medições de descarga e
curvas-chave; chuva diária só é solicitada quando houver disponibilidade
pluviométrica no inventário. As seções transversais são opcionais e
ficam desativadas por padrão com `include_cross_sections = FALSE`, pois
essa rota pode ser lenta ou instável.

As rotas da API autenticada são planejadas em janelas de no máximo um
ano. Em períodos longos, o download pode demorar, retornar resultados
vazios para algumas janelas ou falhar por instabilidade temporária do
serviço. Isso é especialmente comum em produtos especializados, como
curvas-chave e seções transversais. Resultados vazios de `rating_curves`
podem ser válidos e não indicam, por si só, erro do pacote.

## Examples

``` r
# Exemplo operacional com API autenticada. Requer credenciais válidas da ANA.
if (FALSE) {
  token <- ana_authenticate(
    identifier = Sys.getenv("ANA_HIDROWEBSERVICE_IDENTIFIER"),
    password = Sys.getenv("ANA_HIDROWEBSERVICE_PASSWORD")
  )

  dados <- get_ana_data(
    product = "daily_discharge",
    data_source = "api",
    station_code = "56460000",
    start_date = "2020-01-01",
    end_date = "2020-01-31",
    token = token
  )

  agregado <- get_ana_data(
    product = "all",
    data_source = "api",
    station_code = "56460000",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    include_cross_sections = FALSE,
    token = token
  )
}

# Exemplo local com arquivo ja baixado do HidroWeb.
if (FALSE) {
  dados <- get_ana_data(
    product = "daily_discharge",
    data_source = "hidroweb",
    path = "Estacao_56460000.zip"
  )
}
```
