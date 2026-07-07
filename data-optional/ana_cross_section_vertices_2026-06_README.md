# Optional ANA cross-section vertices

Snapshot: `2026-06`

This directory contains optional data files for `hydroDataBR`.
These files are stored in the GitHub repository but are excluded from the installed R package.

## File

- `ana_cross_section_vertices_2026-06.rds`

## Contents

Complete cross-section profile vertices from the ANA/HydroStat June 2026 snapshot.
The main package includes cross-section metadata and station-level summaries, but not complete vertices.

## Metadata

- Rows: 2,170,742
- Columns: 24
- Stations: 2842
- Size: 13.979 MB
- SHA256: `f5f3df522a6ca8317e617c8628418dce68ba505bea9b4e96b6ad661bf8d7142a`

## Example

```r
vertices <- readRDS("data-optional/ana_cross_section_vertices_2026-06.rds")
head(vertices)
```

These data are static and may diverge from current ANA online services.
