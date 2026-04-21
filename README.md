# ProyectoAnaliticaDatosR

Pipeline en R para analizar la siniestralidad vial en Bogotá (2024) a partir de archivos Excel públicos.

El proyecto integra cuatro capas:

1. ETL: extracción, limpieza y guardado de datos.
2. EDA: estadísticas descriptivas, outliers, variables derivadas y correlaciones.
3. Visualizaciones: generación automática de 14 gráficos en PNG y (si está disponible) SVG.
4. Dashboard: aplicación Shiny con 18 visualizaciones interactivas y filtros globales.

## Fuente de datos

Portal de datos abiertos de Bogotá:

- https://datosabiertos.bogota.gov.co/dataset/anuario-siniestralidad

## Estructura del proyecto

```text
.
├─ main.R
├─ config.R
├─ etl_limpieza.R
├─ analisis_exploratorio.R
├─ visualizaciones.R
├─ dashboard.r
├─ README.md
└─ Files/
	├─ Actor_vial.xlsx            # Entrada (crudo)
	├─ Siniestros.xlsx            # Entrada (crudo)
	├─ Vehiculos.xlsx             # Entrada (crudo)
	├─ Cleaned/
	│  ├─ Actor_vial_limpio_R.xlsx
	│  ├─ Siniestros_limpio_R.xlsx
	│  └─ Vehiculos_limpio_R.xlsx
	└─ Charts/
		├─ PNG/
		└─ SVG/
```

## Requisitos

- R (recomendado >= 4.2).
- Conexión a internet solo la primera vez (para instalar paquetes faltantes).

Paquetes que usa el proyecto:

- readxl
- writexl
- dplyr
- tidyr
- ggplot2
- scales
- corrplot
- shiny
- shinydashboard
- plotly
- leaflet
- leaflet.extras
- svglite (opcional, para exportar SVG)

Nota: `main.R` instala automáticamente los paquetes faltantes (excepto `svglite`, que se valida en `visualizaciones.R`).

## Cómo ejecutar

### Opción 1: RStudio

1. Abre el proyecto en RStudio.
2. Asegúrate de estar en la carpeta raíz.
3. Ejecuta:

```r
source("main.R")
```

### Opción 2: Terminal

Desde la raíz del proyecto:

```bash
Rscript main.R
```

## Qué hace `main.R`

`main.R` orquesta el flujo completo en este orden:

1. `run_etl()` desde `etl_limpieza.R`
2. `run_eda()` desde `analisis_exploratorio.R`
3. `generate_all_charts()` desde `visualizaciones.R`
4. `run_dashboard()` desde `dashboard.r`

## Salidas esperadas

Después de ejecutar el pipeline:

1. Archivos limpios en `Files/Cleaned/`.
2. Gráficos en `Files/Charts/PNG/`.
3. Gráficos en `Files/Charts/SVG/` (si `svglite` está instalado).
4. Dashboard Shiny disponible en:

- http://127.0.0.1:3838

## Limpieza aplicada (resumen)

El módulo ETL aplica reglas de estandarización como:

- Relleno de nulos con moda en columnas clave.
- Sustitución de faltantes en columnas `Con_*` por `"NO"`.
- Manejo de identificadores faltantes (`"No Identificado"`, `"Sin Código"`).
- Eliminación de columnas no necesarias por dataset.
- Conservación de coordenadas geográficas en siniestros.

## EDA y visualización

El análisis exploratorio incluye:

- Métricas descriptivas por variable.
- Detección de outliers (IQR y Z-score).
- Normalizaciones Min-Max y Z-score.
- Variables derivadas (grupo etario, franja horaria, fin de semana).
- Matrices de correlación y tendencias temporales.

La capa de visualizaciones genera 14 gráficos estáticos (barras, torta, línea, histograma, heatmap, violín, boxplot, etc.) y el dashboard amplía el análisis con interacción, filtros y mapa geográfico.

## Solución de problemas rápida

1. Error por archivos no encontrados:
	- Verifica que `Actor_vial.xlsx`, `Siniestros.xlsx` y `Vehiculos.xlsx` estén en `Files/`.
2. El dashboard no abre:
	- Revisa que los paquetes `shiny`, `shinydashboard`, `plotly`, `leaflet` y `leaflet.extras` estén instalados.
3. No se generan SVG:
	- Instala `svglite`.
4. Error de ruta:
	- Ejecuta el script desde la raíz del proyecto.

## Nota de mantenimiento

En Windows normalmente no hay problema con mayúsculas/minúsculas en nombres de archivo. Si se ejecuta en un sistema sensible a mayúsculas (por ejemplo Linux), conviene unificar el nombre del archivo del dashboard (`dashboard.r` vs `dashboard.R`) para evitar errores al hacer `source`.

