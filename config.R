# =============================================================================
# config.R — Variables centralizadas del pipeline R
# Proyecto: Siniestralidad Vial Bogotá 2024
# Fuente:   https://datosabiertos.bogota.gov.co/dataset/anuario-siniestralidad
# =============================================================================

# ── Rutas base ────────────────────────────────────────────────────────────────
# La ejecución se hace desde la raíz del proyecto con `main.R`.
# Se evita depender de rstudioapi para que también funcione desde terminal.
BASE_DIR    <- normalizePath(getwd(), mustWork = FALSE)
FILES_DIR   <- file.path(BASE_DIR, "Files")
CLEANED_DIR <- file.path(FILES_DIR, "Cleaned")
CHARTS_DIR  <- file.path(FILES_DIR, "Charts")

# ── Archivos de entrada ────────────────────────────────────────────────────────
ACTOR_VIAL_FILE <- "Actor_vial.xlsx"
SINIESTROS_FILE <- "Siniestros.xlsx"
VEHICULOS_FILE  <- "Vehiculos.xlsx"
VEHICULOS_SHEET <- 1          # readxl usa índice 1-based

# ── Archivos de salida (Cleaned/) ─────────────────────────────────────────────
ACTOR_OUTPUT      <- "Actor_vial_limpio_R.xlsx"
SINIESTROS_OUTPUT <- "Siniestros_limpio_R.xlsx"
VEHICULOS_OUTPUT  <- "Vehiculos_limpio_R.xlsx"

# ── Columnas a eliminar por dataset ──────────────────────────────────────────
COLS_DROP_ACTOR     <- c("Muerte_Posterior", "Fecha_CambioGravedad", "Tipo_SITP")
COLS_DROP_SINIESTROS <- character(0)  # Conservar coordenadas en Siniestros
COLS_DROP_VEHICULOS <- c("Tipo_SITP", "Modalidad")

# ── Columnas Con_* explícitas — Siniestros (21 cols) ─────────────────────────
CON_COLS_SINIESTROS <- c(
  "Con_Bicicleta", "Con_Carga", "Con_Embriaguez", "Con_Huecos",
  "Con_Menores", "Con_Moto", "Con_Peaton", "Con_Persona_Mayor",
  "Con_Rutas", "Con_Tpi", "Con_Tpp", "Con_Velocidad", "Con_Sitp",
  "Con_Troncal", "Con_Alimentador", "Con_Zonal", "Con_Provisional",
  "Con_Articulado", "Con_Biarticulado", "Con_Padron_Dual",
  "Con_Servicio_Especial", "Con_Taxi"
)

# ── Columnas Con_* explícitas — Vehículos (22 cols) ──────────────────────────
CON_COLS_VEHICULOS <- c(
  "Con_Bicicleta", "Con_Carga", "Con_Embriaguez", "Con_Huecos",
  "Con_Menores", "Con_Moto", "Con_Peaton", "Con_Persona_Mayor",
  "Con_Rutas", "Con_Tpi", "Con_Tpp", "Con_Velocidad", "Con_Sitp",
  "Con_Troncal", "Con_Alimentador", "Con_Zonal", "Con_Provisional",
  "Con_Articulado", "Con_Biarticulado", "Con_Padron_Dual",
  "Con_Servicio_Especial", "Con_Taxi"
)

# ── Etiquetas de relleno ──────────────────────────────────────────────────────
NO_ID       <- "No Identificado"
NO_CODE     <- "Sin Código"
DEFAULT_CON <- "NO"

# ── Constantes de visualización ───────────────────────────────────────────────
MESES_ES <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

MESES_ABREV <- c(
  "Ene", "Feb", "Mar", "Abr", "May", "Jun",
  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
)

DIAS_SEMANA_ORDER <- c(
  "lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"
)

# Bins y etiquetas de grupos etarios
EDAD_BREAKS <- c(0, 14, 17, 25, 40, 60, 120)
EDAD_LABELS <- c(
  "0-14 Niños", "15-17 Adolescentes", "18-25 Jóvenes",
  "26-40 Adultos jóvenes", "41-60 Adultos", "60+ Adultos mayores"
)

# Paleta de gravedad
PALETTE_GRAV <- c(
  "Solo Daños"   = "#3498db",
  "Con Heridos"  = "#f39c12",
  "Con Muertos"  = "#e74c3c"
)

# Mapeo de columnas Con_* a etiquetas legibles
CON_LABELS <- c(
  Con_Bicicleta        = "Bicicleta",
  Con_Carga            = "Carga",
  Con_Embriaguez       = "Embriaguez",
  Con_Huecos           = "Huecos",
  Con_Menores          = "Menores",
  Con_Moto             = "Motocicleta",
  Con_Peaton           = "Peatón",
  Con_Persona_Mayor    = "Persona mayor",
  Con_Rutas            = "Rutas",
  Con_Tpi              = "TPI",
  Con_Tpp              = "TPP",
  Con_Velocidad        = "Velocidad",
  Con_Sitp             = "SITP",
  Con_Troncal          = "Troncal",
  Con_Alimentador      = "Alimentador",
  Con_Zonal            = "Zonal",
  Con_Provisional      = "Provisional",
  Con_Articulado       = "Articulado",
  Con_Biarticulado     = "Biarticulado",
  Con_Padron_Dual      = "Padrón dual",
  Con_Servicio_Especial = "Servicio especial",
  Con_Taxi             = "Taxi"
)

# URL de la fuente de datos
DATA_SOURCE_URL <- "https://datosabiertos.bogota.gov.co/dataset/anuario-siniestralidad"

message("[config.R] Configuración cargada correctamente.")
