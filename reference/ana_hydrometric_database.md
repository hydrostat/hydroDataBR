# Banco hidrométrico interno da ANA

O `hydroDataBR` inclui um conjunto de dados internos para consulta
offline e apoio a análises hidrométricas. O banco foi derivado do banco
usado no HydroStat Data Explorer e contém uma fotografia do acervo da
ANA obtida em junho de 2026. Como os sistemas da ANA são atualizados ao
longo do tempo, os dados internos devem ser interpretados como uma
referência estática, não como substituto de uma consulta online atual.

## Source

Agência Nacional de Águas e Saneamento Básico (ANA), dados obtidos em
junho de 2026 e processados para o HydroStat Data Explorer e para o
`hydroDataBR`.

## Objetos incluídos

- `ana_stations`: inventário de estações ANA incluído no pacote.

- `ana_discharge_measurements`: medições de descarga convencionais.

- `ana_rating_curves`: segmentos de curvas-chave.

- `ana_rating_curve_summary`: resumo por curva-chave completa.

- `ana_cross_sections`: metadados de seções transversais, sem vértices.

- `ana_cross_section_summary`: resumo de seções transversais por
  estação.

## `ana_discharge_measurements`

Tabela de medições de descarga com uma linha por medição. As principais
colunas são `station_code`, `measurement_datetime`, `measurement_date`,
`consistency_level`, `stage_cm`, `discharge_m3s`, `wetted_area_m2`,
`width_m`, `mean_depth_m`, `mean_velocity_ms`, `source`, `source_route`,
`downloaded_at` e `processed_at`.

## `ana_rating_curves`

Tabela de segmentos de curvas-chave. Uma curva-chave pode ter um ou mais
segmentos, cada um associado a uma faixa de cota. As principais colunas
são `station_code`, `rating_curve_id`, `rating_curve_segment_id`,
`segment_number`, `valid_from`, `valid_to`, `consistency_level`,
`stage_min_cm`, `stage_max_cm`, `coefficient_a`, `coefficient_h0_cm`,
`coefficient_h0_m`, `coefficient_n`, `curve_type`, `equation_type`,
`source`, `downloaded_at` e `processed_at`.

## `ana_rating_curve_summary`

Tabela resumida por curva-chave completa. As principais colunas são
`station_code`, `rating_curve_id`, `valid_from`, `valid_to`,
`consistency_level`, `n_segments`, `n_distinct_segment_numbers`,
`n_segments_reported_max`, `stage_min_cm`, `stage_max_cm`, `source`,
`first_downloaded_at`, `last_downloaded_at` e `processed_at`.

## `ana_cross_sections`

Tabela de metadados de seções transversais, com uma linha por seção. Ela
informa a estação, data da medição, nível de consistência, número de
vértices e faixas de distância e cota do perfil. Os vértices individuais
dos perfis não são distribuídos no pacote principal.

## `ana_cross_section_summary`

Tabela resumida por estação com número de seções transversais, número
total de vértices no banco original, primeira e última seção disponível,
contagens por nível de consistência e faixas geométricas observadas.
