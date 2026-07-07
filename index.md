# hydroDataBR

`hydroDataBR` é um pacote R para obter, padronizar, analisar, visualizar
e exportar dados hidrológicos da Agência Nacional de Águas e Saneamento
Básico (ANA), com foco em séries diárias, produtos fluviométricos e
fluxos de trabalho reprodutíveis em R.

O pacote oferece uma API pública enxuta. As rotas de aquisição da ANA
ficam concentradas em
[`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md)
e
[`get_ana_data_batch()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data_batch.md),
enquanto as etapas de análise, gráficos, tabelas e exportação usam
funções fonte-neutras:
[`analyze_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/analyze_hydro_data.md),
[`plot_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/plot_hydro_data.md),
[`table_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/table_hydro_data.md)
e
[`write_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/write_hydro_data.md).

Este repositório é mantido pelo grupo de pesquisa **hydro_stat**.

## Instalação

Instale a versão de desenvolvimento a partir do GitHub:

``` r

# install.packages("remotes")
remotes::install_github("hydrostat/hydroDataBR")
```

## Principais recursos

- Leitura de arquivos locais do HidroWeb, XML legado da ANA e JSON.
- Aquisição de dados por rotas ANA quando o usuário fornece autenticação
  própria.
- Padronização de séries diárias de vazão, cota e chuva.
- Inventário interno de estações e disponibilidade de produtos.
- Banco fluviométrico interno para diagnósticos offline.
- Análises básicas de disponibilidade, estatísticas, curva de
  permanência, índices de vazão, máximos anuais, mínimas móveis, índices
  de chuva e diagnósticos.
- Gráficos, tabelas e exportação a partir de objetos padronizados.

## Banco interno da ANA

O pacote inclui um retrato estático de dados da ANA obtido em junho de
2026. Esse banco interno é usado para consultas rápidas, exemplos e
diagnósticos offline, mas não substitui os serviços online da ANA. Os
dados online atuais podem divergir do retrato incluído no pacote.

Objetos internos incluídos:

``` text
ana_stations
ana_discharge_measurements
ana_rating_curves
ana_rating_curve_summary
ana_cross_sections
ana_cross_section_summary
```

Os vértices completos de seções transversais não fazem parte do pacote
principal.

## Fluxo rápido com dados internos

``` r

library(hydroDataBR)

# Filtrar estações com dados fluviométricos disponíveis
stations <- filter_ana_stations(
  station_type = "fluviometric",
  has_discharge_data = TRUE
)

head(stations)
```

## Leitura de arquivos locais

``` r

library(hydroDataBR)

# Arquivo ZIP/CSV do HidroWeb salvo localmente
daily <- read_hidroweb("caminho/para/arquivo_hidroweb.zip")

# Série diária padronizada
head(daily)
```

Também é possível usar
[`read_ana_xml()`](https://hydrostat.github.io/hydroDataBR/reference/read_ana_xml.md)
para arquivos XML legados e
[`read_ana_json()`](https://hydrostat.github.io/hydroDataBR/reference/read_ana_json.md)
para respostas JSON salvas localmente.

## Análises, gráficos e tabelas

``` r

library(hydroDataBR)

# Exemplo assumindo que `daily` é uma série diária padronizada
stats <- analyze_hydro_data(daily, analysis = "daily_statistics")
availability <- analyze_hydro_data(daily, analysis = "daily_availability")

p <- plot_hydro_data(daily, plot = "daily_series")

tab <- table_hydro_data(daily, table = "daily_availability")
```

As funções de gráfico retornam objetos `ggplot`, que podem ser
modificados com camadas usuais do `ggplot2`.

## Diagnóstico fluviométrico

Os diagnósticos fluviométricos usam, quando necessário, referências
hidrométricas internas do pacote. A prioridade é:

``` text
dados fornecidos pelo usuário > objetos baixados na mesma chamada > banco interno > diagnóstico básico
```

Quando o banco interno é usado, o resultado registra a origem da
referência e o retrato `2026-06`.

``` r

hydro_diag <- diagnose_daily_hydrometry(daily)

hydro_diag$hydrometry_reference_source
hydro_diag$hydrometry_reference_snapshot

plot_hydro_data(hydro_diag, plot = "hydrometry_diagnostics")
table_hydro_data(hydro_diag, table = "hydrometry_diagnostics")
```

## Aquisição autenticada na ANA

A aquisição autenticada exige credenciais próprias do usuário junto à
ANA. O pacote não armazena, imprime ou registra CPF/CNPJ, senha, tokens,
cabeçalhos `Authorization` ou cabeçalhos sensíveis.

Exemplo operacional, não executado automaticamente:

``` r

if (FALSE) {
  token <- ana_authenticate(
    identifier = Sys.getenv("ANA_HIDROWEBSERVICE_IDENTIFIER"),
    password = Sys.getenv("ANA_HIDROWEBSERVICE_PASSWORD")
  )

  x <- get_ana_data(
    product = "daily_discharge",
    data_source = "api",
    station_code = "00000000",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    token = token
  )
}
```

Chamadas de exemplo, testes e checks do pacote não dependem de
credenciais nem de serviços online da ANA.

## Dados opcionais

Os vértices completos de seções transversais não são instalados com o
pacote principal, mas estão disponíveis como dado opcional no
repositório GitHub.

``` r

if (FALSE) {
  vertices <- download_ana_cross_section_vertices()
  head(vertices)
}
```

## API pública

``` text
ana_authenticate
analyze_hydro_data
diagnose_daily_hydrometry
diagnose_station_hydrometry
filter_ana_stations
get_ana_data
get_ana_data_batch
get_ana_stations
plot_hydro_data
read_ana_json
read_ana_xml
read_hidroweb
read_hidroweb_cross_sections
download_ana_cross_section_vertices
table_hydro_data
write_hydro_data
```

## Licença

Este projeto é distribuído sob a licença MIT. Consulte
[`LICENSE.md`](https://hydrostat.github.io/hydroDataBR/LICENSE.md).

O arquivo `LICENSE` é mantido no formato esperado por pacotes R com
`License: MIT + file LICENSE`.
