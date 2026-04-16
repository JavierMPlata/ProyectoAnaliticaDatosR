# =============================================================================
# main.R — Orquestador principal del pipeline R
# Proyecto: Siniestralidad Vial Bogotá 2024
# =============================================================================
# Ejecuta secuencialmente:
#   1. ETL (Extracción → Transformación → Carga)
#   2. Análisis Exploratorio (EDA)
#   3. Visualizaciones con ggplot2
#
# Uso desde RStudio:
#   setwd("<ruta_al_proyecto>")
#   source("R/main.R")
#
# Uso desde terminal R:
#   Rscript R/main.R
# =============================================================================

# ── Verificar paquetes e instalar si faltan ────────────────────────────────────
paquetes_necesarios <- c(
  "readxl",    # Lectura de Excel
  "writexl",   # Escritura de Excel
  "dplyr",     # Manipulación de datos
  "tidyr",     # Pivoteo y reshape
  "ggplot2",   # Visualizaciones
  "scales",    # Formato de ejes
  "corrplot"   # Gráfico de correlaciones
)

paquetes_faltantes <- paquetes_necesarios[
  !sapply(paquetes_necesarios, requireNamespace, quietly = TRUE)
]

if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org")
}

# ── Cargar módulos del proyecto ───────────────────────────────────────────────
source("R/config.R")
source("R/etl_limpieza.R")
source("R/analisis_exploratorio.R")
source("R/visualizaciones.R")

# =============================================================================
# PIPELINE COMPLETO
# =============================================================================

t_total <- proc.time()["elapsed"]

message("\n", strrep("#", 65))
message("  PIPELINE R — SINIESTRALIDAD VIAL BOGOTÁ 2024")
message(strrep("#", 65), "\n")

# ── FASE 1: ETL ───────────────────────────────────────────────────────────────
message(">> PASO 1: ETL (Extracción → Transformación → Carga)")
resultado_etl <- run_etl()

# ── FASE 2: ANÁLISIS EXPLORATORIO ─────────────────────────────────────────────
message("\n>> PASO 2: ANÁLISIS EXPLORATORIO")
resultado_eda <- run_eda()

# ── FASE 3: VISUALIZACIONES ───────────────────────────────────────────────────
message("\n>> PASO 3: GENERACIÓN DE GRÁFICOS")
graficos <- generate_all_charts()

# ── RESUMEN FINAL ─────────────────────────────────────────────────────────────
elapsed_total <- proc.time()["elapsed"] - t_total

message("\n", strrep("#", 65))
message("  PIPELINE COMPLETADO EN ", round(elapsed_total, 2), " SEGUNDOS")
message(strrep("#", 65))
message("  Archivos limpios → ", CLEANED_DIR)
message("  Gráficos         → ", CHARTS_DIR)
message(strrep("#", 65), "\n")
