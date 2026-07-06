#' hydroDataBR: dados hidrológicos brasileiros em uma interface enxuta
#'
#' O `hydroDataBR` reúne ferramentas para obtenção, leitura, padronização,
#' análise, visualização e exportação de dados hidrológicos da ANA. O pacote
#' foi desenhado para apoiar rotinas de engenharia hidrológica com uma API
#' curta: poucas funções públicas, contratos de dados previsíveis e resultados
#' que podem ser usados diretamente em análises, gráficos, tabelas e arquivos
#' de saída.
#'
#' @section Fluxo recomendado:
#' O uso cotidiano parte de [get_ana_data()] ou [get_ana_data_batch()] para
#' obter dados, [analyze_hydro_data()] para calcular indicadores e resumos,
#' [plot_hydro_data()] para gerar gráficos, [table_hydro_data()] para montar
#' tabelas e [write_hydro_data()] para exportar os resultados.
#'
#' @section Banco interno:
#' O pacote inclui um banco interno de consulta com inventário de estações,
#' medições de descarga, curvas-chave, resumo de curvas-chave e metadados de
#' seções transversais. Esses dados foram obtidos da ANA em junho de 2026.
#' Eles são uma fotografia estática do acervo naquele momento; por isso,
#' informações atuais consultadas online podem divergir do banco interno.
#' Use `?ana_hydrometric_database` para ver a documentação detalhada dos
#' objetos de dados.
#'
#' @section Escopo e limitações:
#' O pacote prioriza dados convencionais da ANA. Chamadas online dependem da
#' disponibilidade dos serviços externos e, quando usam a API autenticada, de
#' credenciais fornecidas pelo usuário. Produtos especializados, como
#' curvas-chave e seções transversais, podem retornar resultados vazios,
#' demorar mais que as séries diárias ou falhar em algumas janelas de consulta.
#' As seções transversais completas, com todos os vértices dos perfis, não são
#' distribuídas no banco interno do pacote principal.
#'
#' @name hydroDataBR-package
#' @aliases hydroDataBR
"_PACKAGE"
