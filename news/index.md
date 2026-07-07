# Changelog

## hydroDataBR 1.1.0

### Novo recurso

- Adiciona
  [`download_ana_cross_section_vertices()`](https://hydrostat.github.io/hydroDataBR/reference/download_ana_cross_section_vertices.md)
  para baixar opcionalmente os vértices completos de seções transversais
  hospedados no próprio repositório GitHub do pacote.
- O arquivo de vértices permanece fora do pacote instalado e é
  reutilizado a partir do cache local do usuário.
- A nova função não consulta serviços vivos da ANA e não exige
  credenciais.
