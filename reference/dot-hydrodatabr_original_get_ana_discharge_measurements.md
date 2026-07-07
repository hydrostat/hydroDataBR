# Obter medicoes de descarga liquida da ANA

Consulta as medicoes de descarga liquida de uma estacao no
HidroWebService da ANA e retorna uma tabela padronizada.

## Usage

``` r
.hydrodatabr_original_get_ana_discharge_measurements(
  token = NULL,
  station_code,
  start_date = NULL,
  end_date = NULL,
  consistency_level = NULL,
  timeout = 120
)
```

## Arguments

- token:

  Objeto de token retornado por
  [`ana_authenticate()`](https://hydrostat.github.io/hydroDataBR/reference/ana_authenticate.md).

- station_code:

  Codigo da estacao. Mantido como texto.

- start_date:

  Data inicial no formato `YYYY-MM-DD`.

- end_date:

  Data final no formato `YYYY-MM-DD`.

- consistency_level:

  Nivel de consistencia usado para filtrar o resultado padronizado,
  quando aplicavel.

- timeout:

  Tempo maximo da requisicao em segundos.

## Value

Um tibble com medicoes de descarga liquida padronizadas.
