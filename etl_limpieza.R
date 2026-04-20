# =============================================================================
# etl_limpieza.R — Pipeline ETL completo en R
# Fases: Extracción → Transformación → Carga
# Proyecto: Siniestralidad Vial Bogotá 2024
# =============================================================================
# Paquetes necesarios: readxl, dplyr, writexl
# Instalar con: install.packages(c("readxl", "dplyr", "writexl"))
# =============================================================================

library(readxl)    # Lectura de archivos Excel
library(dplyr)     # Manipulación de datos (pipeline con |>)
library(writexl)   # Escritura de archivos Excel

source("config.R")  # Cargar variables de configuración

# =============================================================================
# UTILIDADES AUXILIARES
# =============================================================================

#' Rellena columnas Con_* con DEFAULT_CON donde sean NA, "" o " "
#'
#' @param df      data.frame a transformar
#' @param cols    vector de nombres de columnas Con_*
#' @return        data.frame con nulos corregidos
fill_con_columns <- function(df, cols) {
  found <- intersect(cols, names(df))
  nulos_antes <- sum(sapply(df[found], function(x) sum(is.na(x) | x == "" | x == " ")))

  df[found] <- lapply(df[found], function(x) {
    x[is.na(x) | x == "" | x == " "] <- DEFAULT_CON
    x
  })

  message(sprintf("  Columnas Con_* (%d): %d nulos/vacíos corregidos", length(found), nulos_antes))
  df
}

#' Rellena los NA de una columna con su moda
#'
#' @param df      data.frame
#' @param column  nombre de la columna
#' @return        data.frame con nulos rellenados
fill_mode <- function(df, column) {
  if (!column %in% names(df)) {
    warning(sprintf("  Columna '%s' no encontrada, se omite fill_mode", column))
    return(df)
  }

  nulos <- sum(is.na(df[[column]]))
  if (nulos == 0) return(df)

  # Calcular moda (valor más frecuente, excluyendo NA)
  freq_table <- sort(table(df[[column]]), decreasing = TRUE)
  if (length(freq_table) == 0) {
    warning(sprintf("  No se pudo calcular moda para '%s'", column))
    return(df)
  }
  moda <- names(freq_table)[1]
  df[[column]][is.na(df[[column]])] <- moda
  message(sprintf("  '%s': %d nulos rellenados con moda '%s'", column, nulos, moda))
  df
}

# =============================================================================
# FASE 1 — EXTRACCIÓN
# =============================================================================

#' Carga el dataset de Actor Vial desde el archivo Excel fuente
#'
#' @return data.frame con los datos crudos de Actor Vial
extract_actor_vial <- function() {
  path <- file.path(FILES_DIR, ACTOR_VIAL_FILE)
  message(sprintf("Extrayendo Actor Vial desde: %s", path))

  if (!file.exists(path)) {
    stop(sprintf(
      "Archivo no encontrado: %s\nDescárgalo desde %s y colócalo en %s",
      path, DATA_SOURCE_URL, FILES_DIR
    ))
  }

  df <- read_excel(path)
  message(sprintf("  Actor Vial cargado: %d filas × %d columnas", nrow(df), ncol(df)))
  df
}

#' Carga el dataset de Siniestros desde el archivo Excel fuente
#'
#' @return data.frame con los datos crudos de Siniestros
extract_siniestros <- function() {
  path <- file.path(FILES_DIR, SINIESTROS_FILE)
  message(sprintf("Extrayendo Siniestros desde: %s", path))

  if (!file.exists(path)) {
    stop(sprintf(
      "Archivo no encontrado: %s\nDescárgalo desde %s y colócalo en %s",
      path, DATA_SOURCE_URL, FILES_DIR
    ))
  }

  df <- read_excel(path)
  message(sprintf("  Siniestros cargado: %d filas × %d columnas", nrow(df), ncol(df)))
  df
}

#' Carga el dataset de Vehículos desde la primera hoja del Excel fuente
#'
#' @return data.frame con los datos crudos de Vehículos
extract_vehiculos <- function() {
  path <- file.path(FILES_DIR, VEHICULOS_FILE)
  message(sprintf("Extrayendo Vehículos desde: %s (sheet=%d)", path, VEHICULOS_SHEET))

  if (!file.exists(path)) {
    stop(sprintf(
      "Archivo no encontrado: %s\nDescárgalo desde %s y colócalo en %s",
      path, DATA_SOURCE_URL, FILES_DIR
    ))
  }

  df <- read_excel(path, sheet = VEHICULOS_SHEET)
  message(sprintf("  Vehículos cargado: %d filas × %d columnas", nrow(df), ncol(df)))
  df
}

# =============================================================================
# FASE 2 — TRANSFORMACIÓN
# =============================================================================

#' Limpieza del dataset Actor Vial
#'
#' Pasos:
#' 1. Eliminar columnas innecesarias.
#' 2. Edad/Sexo: NA → "No Identificado".
#' 3. Gravedad: rellenar con moda; coherencia MUERTO.
#' 4. Columnas Con_* (detección dinámica): NA/vacío → "NO".
#' 5. Sexo residual → "No Identificado".
#' 6. Codigo_Vehiculo → "Sin Código".
#'
#' @param df data.frame crudo de Actor Vial
#' @return   data.frame limpio
transform_actor_vial <- function(df) {
  message("--- Transformando Actor Vial ---")
  filas_ini <- nrow(df)

  # 1. Eliminar columnas innecesarias
  cols_drop <- intersect(COLS_DROP_ACTOR, names(df))
  df <- df |> select(-any_of(cols_drop))
  message(sprintf("  Columnas eliminadas: %s", paste(cols_drop, collapse = ", ")))

  # 2. Edad y Sexo: convertir Edad a character para unificar tipos
  df$Edad <- as.character(df$Edad)
  nulos_edad <- sum(is.na(df$Edad))
  df$Sexo[is.na(df$Edad)]  <- NO_ID   # Primero marcar Sexo
  df$Edad[is.na(df$Edad)]  <- NO_ID   # Luego rellenar Edad
  message(sprintf("  Edad/Sexo: %d registros marcados como '%s'", nulos_edad, NO_ID))

  # 3. Gravedad — rellenar con moda
  df <- fill_mode(df, "Gravedad_Indicador_Tradicional")
  df <- fill_mode(df, "Gravedad_Indicador_30d")

  # Coherencia: si Gravedad_Indicador_Tradicional == "MUERTO" → 30d también
  mask_muerto <- !is.na(df$Gravedad_Indicador_Tradicional) &
                 toupper(df$Gravedad_Indicador_Tradicional) == "MUERTO"
  df$Gravedad_Indicador_30d[mask_muerto] <- "MUERTO"
  message(sprintf("  Coherencia MUERTO aplicada a %d filas", sum(mask_muerto)))

  # 4. Columnas Con_* — detección dinámica
  con_cols_dinamicas <- grep("^Con_", names(df), value = TRUE)
  df <- fill_con_columns(df, con_cols_dinamicas)

  # 5. Sexo residual
  nulos_sexo <- sum(is.na(df$Sexo))
  df$Sexo[is.na(df$Sexo)] <- NO_ID
  if (nulos_sexo > 0)
    message(sprintf("  Sexo residual: %d nulos → '%s'", nulos_sexo, NO_ID))

  # 6. Codigo_Vehiculo
  if ("Codigo_Vehiculo" %in% names(df)) {
    nulos_cod <- sum(is.na(df$Codigo_Vehiculo))
    df$Codigo_Vehiculo[is.na(df$Codigo_Vehiculo)] <- NO_CODE
    if (nulos_cod > 0)
      message(sprintf("  Codigo_Vehiculo: %d nulos → '%s'", nulos_cod, NO_CODE))
  }

  message(sprintf(
    "  Actor Vial transformado: %d filas, %d columnas, %d nulos restantes",
    nrow(df), ncol(df), sum(is.na(df))
  ))
  df
}

#' Limpieza del dataset Siniestros
#'
#' Pasos:
#' 1. Elemento_Choque → moda.
#' 2. Tipo_Objeto_Fijo → eliminar filas con NA.
#' 3. Gravedad_indicador_30d → copiar de Tradicional donde sea NA.
#' 4. Columnas Con_* (lista explícita) → "NO".
#' 5. Conservar Longitud y Latitud.
#'
#' @param df data.frame crudo de Siniestros
#' @return   data.frame limpio
transform_siniestros <- function(df) {
  message("--- Transformando Siniestros ---")
  filas_ini <- nrow(df)

  # 1. Elemento_Choque → moda
  df <- fill_mode(df, "Elemento_Choque")

  # 2. Tipo_Objeto_Fijo → eliminar filas con NA
  if ("Tipo_Objeto_Fijo" %in% names(df)) {
    nulos_obj <- sum(is.na(df$Tipo_Objeto_Fijo))
    df <- df |> filter(!is.na(Tipo_Objeto_Fijo))
    message(sprintf("  Tipo_Objeto_Fijo: %d filas eliminadas (NA)", nulos_obj))
  }

  # 3. Gravedad_indicador_30d → rellenar desde Tradicional
  col_30d  <- "Gravedad_indicador_30d"
  col_trad <- "Gravedad_Indicador_Tradicional"
  if (col_30d %in% names(df) && col_trad %in% names(df)) {
    nulos_grav <- sum(is.na(df[[col_30d]]))
    df[[col_30d]][is.na(df[[col_30d]])] <- df[[col_trad]][is.na(df[[col_30d]])]
    message(sprintf("  '%s': %d nulos rellenados desde '%s'", col_30d, nulos_grav, col_trad))
  }

  # 4. Columnas Con_*
  df <- fill_con_columns(df, CON_COLS_SINIESTROS)

  # 5. Conservar Longitud y Latitud (sin eliminación de coordenadas)
  message("  Coordenadas Longitud/Latitud conservadas")

  message(sprintf(
    "  Siniestros transformado: %d → %d filas, %d columnas, %d nulos restantes",
    filas_ini, nrow(df), ncol(df), sum(is.na(df))
  ))
  df
}

#' Limpieza del dataset Vehículos
#'
#' Pasos:
#' 1. Eliminar columnas Tipo_SITP, Modalidad.
#' 2. Clase: NA → moda.
#' 3. Columnas Con_* (lista explícita, 22 cols) → "NO".
#'
#' @param df data.frame crudo de Vehículos
#' @return   data.frame limpio
transform_vehiculos <- function(df) {
  message("--- Transformando Vehículos ---")

  # 1. Eliminar columnas
  cols_drop <- intersect(COLS_DROP_VEHICULOS, names(df))
  df <- df |> select(-any_of(cols_drop))
  message(sprintf("  Columnas eliminadas: %s", paste(cols_drop, collapse = ", ")))

  # 2. Clase → moda
  df <- fill_mode(df, "Clase")

  # 3. Columnas Con_*
  df <- fill_con_columns(df, CON_COLS_VEHICULOS)

  message(sprintf(
    "  Vehículos transformado: %d filas, %d columnas, %d nulos restantes",
    nrow(df), ncol(df), sum(is.na(df))
  ))
  df
}

# =============================================================================
# FASE 3 — CARGA
# =============================================================================

#' Guarda un data.frame como .xlsx en CLEANED_DIR
#'
#' @param df       data.frame a guardar
#' @param filename nombre del archivo de salida
load_dataframe <- function(df, filename) {
  if (!dir.exists(CLEANED_DIR)) dir.create(CLEANED_DIR, recursive = TRUE)
  path <- file.path(CLEANED_DIR, filename)
  write_xlsx(df, path)
  size_kb <- file.info(path)$size / 1024
  message(sprintf("  Guardado: %s  (%d filas, %.1f KB)", path, nrow(df), size_kb))
}

load_actor_vial  <- function(df) load_dataframe(df, ACTOR_OUTPUT)
load_siniestros  <- function(df) load_dataframe(df, SINIESTROS_OUTPUT)
load_vehiculos   <- function(df) load_dataframe(df, VEHICULOS_OUTPUT)

# =============================================================================
# FUNCIÓN PRINCIPAL — ejecutar el pipeline completo
# =============================================================================

#' Ejecuta el pipeline ETL completo: Extract → Transform → Load
#'
#' @return list con los tres data.frames limpios (invisible)
run_etl <- function() {
  message(strrep("=", 60))
  message("INICIO DEL PIPELINE ETL — Siniestralidad Bogotá 2024")
  message(strrep("=", 60))

  t_ini <- proc.time()["elapsed"]

  # ── EXTRACT ──────────────────────────────────────────────────
  message("\n>> FASE 1: EXTRACCIÓN")
  df_actor      <- extract_actor_vial()
  df_siniestros <- extract_siniestros()
  df_vehiculos  <- extract_vehiculos()

  # ── TRANSFORM ────────────────────────────────────────────────
  message("\n>> FASE 2: TRANSFORMACIÓN")
  df_actor      <- transform_actor_vial(df_actor)
  df_siniestros <- transform_siniestros(df_siniestros)
  df_vehiculos  <- transform_vehiculos(df_vehiculos)

  # ── LOAD ─────────────────────────────────────────────────────
  message("\n>> FASE 3: CARGA")
  load_actor_vial(df_actor)
  load_siniestros(df_siniestros)
  load_vehiculos(df_vehiculos)

  elapsed <- proc.time()["elapsed"] - t_ini
  message(strrep("=", 60))
  message(sprintf("ETL COMPLETADO en %.2f segundos", elapsed))
  message(strrep("=", 60))

  invisible(list(
    actor_vial  = df_actor,
    siniestros  = df_siniestros,
    vehiculos   = df_vehiculos
  ))
}
