# Siniestralidad Vial Bogotá — R Project

<!-- AUTO-MANAGED: project-description -->
Analysis and interactive dashboard for Bogotá road accident data (Siniestralidad Vial 2024).
Data source: https://datosabiertos.bogota.gov.co/dataset/anuario-siniestralidad

Three datasets: Siniestros (accidents), Actor_vial (road actors), Vehiculos (vehicles).
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: architecture -->
## File Structure

- `main.R` — entry point, runs the full pipeline
- `config.R` — centralized constants (paths, labels, palettes, column lists)
- `etl_limpieza.R` — ETL and data cleaning pipeline
- `analisis_exploratorio.R` — exploratory analysis
- `visualizaciones.R` — static chart generation
- `dashboard.r` — interactive Shiny dashboard (18 charts)
- `Files/` — data directory
  - `Files/Cleaned/` — cleaned Excel outputs (Actor_vial_limpio_R.xlsx, Siniestros_limpio_R.xlsx, Vehiculos_limpio_R.xlsx)
  - `Files/Charts/` — exported chart images
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: build-commands -->
## Run Commands

```r
# Run full pipeline (ETL + analysis + visualizations)
source("main.R")

# Launch interactive dashboard only
source("dashboard.r")
run_dashboard()

# ETL only
source("etl_limpieza.R")
```
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: dependencies -->
## R Package Dependencies

- `shiny`, `shinydashboard` — Shiny app framework
- `ggplot2`, `dplyr`, `scales` — data manipulation and static charts
- `plotly` — interactive charts (ggplotly)
- `leaflet`, `leaflet.extras` — interactive map with cluster/heatmap toggle
- `readxl` — reading Excel input files
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: conventions -->
## Conventions

- All shared constants live in `config.R` — never hardcode paths, labels, or palettes in other files
- `source("config.R")` must be called at the top of any file that needs project constants
- Dataset column names are case-sensitive; coordinate columns are detected dynamically via `intersect()`
- Spanish locale used throughout: month names (MESES_ES), weekday names (DIAS_SEMANA_ORDER), UI labels
- Files are executed from project root via `main.R` — no `rstudioapi` dependency
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: patterns -->
## Detected Patterns

- **Reusable UI box helpers**: `box_top(id, titulo, slider_id, ...)` for top-N slider + chart; `cbox(id, titulo, descripcion, ...)` for standard chart boxes — both defined inside `run_dashboard()` scope
- **Base ggplot theme**: `tema()` function defined inside `run_dashboard()` scope — minimal theme, white background, suppressed minor grid, styled subtitle/caption
- **Global sidebar filters**: year mode (Interactivo vs General), severity toggle (soloSevero), locality selector — applied across all 18 charts
- **Color palettes**: `PAL_GRAV` (Solo Daños / Con Heridos / Con Muertos), `PAL_SEXO` (MASCULINO / FEMENINO / No Identificado) defined in `run_dashboard()` scope
- **Coordinate validation**: Bogotá bounding box lat [4.44, 4.84], lon [-74.26, -73.98] applied after parsing
- **Age grouping**: `cut()` with `EDAD_BREAKS` / `EDAD_LABELS` from config.R — 6 groups from 0 to 120+
- **Severo derivation**: `ifelse(Gravedad %in% c("Con Heridos", "Con Muertos"), "Severo", "No Severo")` applied in `load_dashboard_data()`
- **Locality auto-zoom**: Leaflet map uses `fitBounds()` with 0.015 padding when a specific locality is selected; falls back to `setView(lng=-74.0817, lat=4.7110, zoom=11)` for "Todas"
<!-- END AUTO-MANAGED -->

<!-- AUTO-MANAGED: git-insights -->
## Git Insights

- `9fd5774` — Implementacion de dashboard interactivo: added full 8-tab Shiny dashboard with 18 interactive charts and Leaflet map
- `dce3d25` — chore: Text more size: minor text sizing adjustment in dashboard UI
- `5dbd403` — Fix: font size adjustments and improved chart description text across dashboard tabs
<!-- END AUTO-MANAGED -->
