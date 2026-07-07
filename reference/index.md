# Package index

## Visão geral

- [`hydroDataBR-package`](https://hydrostat.github.io/hydroDataBR/reference/hydroDataBR-package.md)
  [`hydroDataBR`](https://hydrostat.github.io/hydroDataBR/reference/hydroDataBR-package.md)
  : hydroDataBR: dados hidrológicos brasileiros em uma interface enxuta

## Autenticação e aquisição ANA

- [`ana_authenticate()`](https://hydrostat.github.io/hydroDataBR/reference/ana_authenticate.md)
  : Autenticar no HidroWebService da ANA
- [`get_ana_data()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data.md)
  : Obter dados hidrológicos da ANA
- [`get_ana_data_batch()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_data_batch.md)
  : Obter dados da ANA para várias estações
- [`get_ana_stations()`](https://hydrostat.github.io/hydroDataBR/reference/get_ana_stations.md)
  : Consultar o inventário de estações da ANA

## Leitura de arquivos locais

- [`read_hidroweb()`](https://hydrostat.github.io/hydroDataBR/reference/read_hidroweb.md)
  : Ler séries diárias de arquivos HidroWeb
- [`read_hidroweb_cross_sections()`](https://hydrostat.github.io/hydroDataBR/reference/read_hidroweb_cross_sections.md)
  : Ler seções transversais de arquivos HidroWeb
- [`read_ana_xml()`](https://hydrostat.github.io/hydroDataBR/reference/read_ana_xml.md)
  : Ler séries diárias de XML da ANA
- [`read_ana_json()`](https://hydrostat.github.io/hydroDataBR/reference/read_ana_json.md)
  : Ler séries diárias de JSON da ANA

## Banco interno ANA

- [`filter_ana_stations()`](https://hydrostat.github.io/hydroDataBR/reference/filter_ana_stations.md)
  : Filtrar o inventário embutido de estações ANA
- [`ana_stations`](https://hydrostat.github.io/hydroDataBR/reference/ana_stations.md)
  : Inventario embutido de estacoes hidrometeorologicas da ANA
- [`ana_hydrometric_database`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  [`ana_discharge_measurements`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  [`ana_rating_curves`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  [`ana_rating_curve_summary`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  [`ana_cross_sections`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  [`ana_cross_section_summary`](https://hydrostat.github.io/hydroDataBR/reference/ana_hydrometric_database.md)
  : Banco hidrométrico interno da ANA

## Análises e diagnósticos

- [`analyze_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/analyze_hydro_data.md)
  : Analisar dados hidrológicos padronizados
- [`diagnose_daily_hydrometry()`](https://hydrostat.github.io/hydroDataBR/reference/diagnose_daily_hydrometry.md)
  : Diagnosticar consistência diária entre vazão, cota e curva-chave
- [`diagnose_station_hydrometry()`](https://hydrostat.github.io/hydroDataBR/reference/diagnose_station_hydrometry.md)
  : Diagnosticar medições de descarga e curvas-chave

## Gráficos, tabelas e exportação

- [`plot_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/plot_hydro_data.md)
  : Gerar gráficos hidrológicos
- [`table_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/table_hydro_data.md)
  : Gerar tabelas hidrológicas
- [`write_hydro_data()`](https://hydrostat.github.io/hydroDataBR/reference/write_hydro_data.md)
  : Exportar dados, tabelas e gráficos hidrológicos
