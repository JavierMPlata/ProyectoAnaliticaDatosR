# =============================================================================
# analisis_exploratorio.R — Análisis Exploratorio de Datos (EDA)
# Proyecto: Siniestralidad Vial Bogotá 2024
# =============================================================================
# Incluye:
#   1. Estadísticas descriptivas (media, mediana, moda, desv. estándar, IQR)
#   2. Detección y manejo de valores atípicos (IQR / Z-score)
#   3. Normalización de variables numéricas (Min-Max y Z-score)
#   4. Creación de nuevas variables (grupo etario, franja horaria, etc.)
#   5. Identificación de correlaciones y tendencias
# =============================================================================
# Paquetes: dplyr, tidyr, corrplot, ggplot2
# Instalar: install.packages(c("dplyr", "tidyr", "corrplot", "ggplot2", "readxl"))
# =============================================================================

library(readxl)    # Lectura de datos limpios
library(dplyr)     # Manipulación de datos
library(tidyr)     # Pivoteo y reshape
library(corrplot)  # Visualización de correlaciones

source("R/config.R")  # Variables centralizadas

# =============================================================================
# LECTURA DE DATOS LIMPIOS
# =============================================================================

#' Lee los tres datasets limpios producidos por etl_limpieza.R
#'
#' @return list con df_siniestros, df_actor, df_vehiculos
load_cleaned <- function() {
  message("Cargando datasets limpios desde CLEANED_DIR...")

  df_siniestros <- read_excel(file.path(CLEANED_DIR, SINIESTROS_OUTPUT))
  message(sprintf("  Siniestros: %d filas × %d cols", nrow(df_siniestros), ncol(df_siniestros)))

  df_actor <- read_excel(file.path(CLEANED_DIR, ACTOR_OUTPUT))
  message(sprintf("  Actor Vial: %d filas × %d cols", nrow(df_actor), ncol(df_actor)))

  df_vehiculos <- read_excel(file.path(CLEANED_DIR, VEHICULOS_OUTPUT))
  message(sprintf("  Vehículos:  %d filas × %d cols", nrow(df_vehiculos), ncol(df_vehiculos)))

  list(siniestros = df_siniestros, actor = df_actor, vehiculos = df_vehiculos)
}

# =============================================================================
# 1. ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

#' Calcula y muestra estadísticas descriptivas completas de un data.frame
#'
#' Incluye: conteo, NA, media, mediana, moda, desviación estándar, IQR,
#'          mínimo, máximo y percentiles 25/75 para variables numéricas.
#'
#' @param df    data.frame a analizar
#' @param label etiqueta para identificar el dataset en consola
#' @return      list con resúmenes por tipo de variable
estadisticas_descriptivas <- function(df, label = "Dataset") {
  message(sprintf("\n%s\n[EDA] Estadísticas descriptivas — %s", strrep("=", 60), label))
  message(sprintf("  Dimensiones: %d filas × %d columnas", nrow(df), ncol(df)))
  message(sprintf("  Nulos totales: %d (%.2f%%)", sum(is.na(df)), mean(is.na(df)) * 100))

  # ── Variables numéricas ────────────────────────────────────────────────────
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) > 0) {
    message(sprintf("\n  Variables numéricas (%d):", length(num_cols)))

    resumen_num <- lapply(num_cols, function(col) {
      x <- df[[col]]
      x_clean <- x[!is.na(x)]
      # Moda: valor más frecuente
      freq <- sort(table(x_clean), decreasing = TRUE)
      moda_val <- if (length(freq) > 0) as.numeric(names(freq)[1]) else NA

      tibble(
        Variable  = col,
        N         = length(x_clean),
        NA_count  = sum(is.na(x)),
        Media     = round(mean(x_clean), 2),
        Mediana   = round(median(x_clean), 2),
        Moda      = moda_val,
        Desv_Std  = round(sd(x_clean), 2),
        IQR       = round(IQR(x_clean), 2),
        Min       = round(min(x_clean), 2),
        P25       = round(quantile(x_clean, 0.25), 2),
        P75       = round(quantile(x_clean, 0.75), 2),
        Max       = round(max(x_clean), 2)
      )
    })

    resumen_df <- bind_rows(resumen_num)
    print(as.data.frame(resumen_df), row.names = FALSE)

  } else {
    message("  No hay variables numéricas en este dataset.")
    resumen_df <- NULL
  }

  # ── Variables categóricas ─────────────────────────────────────────────────
  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  if (length(cat_cols) > 0) {
    message(sprintf("\n  Variables categóricas (%d):", length(cat_cols)))
    for (col in cat_cols) {
      freq <- sort(table(df[[col]], useNA = "ifany"), decreasing = TRUE)
      top_val <- names(freq)[1]
      message(sprintf(
        "    %-35s | Únicos: %3d | Moda: '%s' (%d veces)",
        col, length(freq), top_val, freq[1]
      ))
    }
  }

  invisible(list(numericas = resumen_df, categoricas = cat_cols))
}

# =============================================================================
# 2. DETECCIÓN Y MANEJO DE VALORES ATÍPICOS
# =============================================================================

#' Detecta outliers usando el método IQR (rango intercuartílico)
#'
#' Un valor se considera atípico si cae fuera de [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
#'
#' @param df     data.frame
#' @param column nombre de la columna numérica
#' @return       lista con: indices de outliers, límites inferior y superior
detectar_outliers_iqr <- function(df, column) {
  x  <- df[[column]]
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val

  outliers_idx <- which(x < lim_inf | x > lim_sup)
  pct <- round(length(outliers_idx) / sum(!is.na(x)) * 100, 2)

  message(sprintf(
    "  [IQR] %-25s | Límites: [%.2f, %.2f] | Outliers: %d (%.2f%%)",
    column, lim_inf, lim_sup, length(outliers_idx), pct
  ))

  list(indices = outliers_idx, lim_inf = lim_inf, lim_sup = lim_sup)
}

#' Detecta outliers usando Z-score (umbral por defecto: |z| > 3)
#'
#' @param df        data.frame
#' @param column    nombre de la columna numérica
#' @param threshold umbral de Z-score (default 3)
#' @return          lista con: indices de outliers y z-scores
detectar_outliers_zscore <- function(df, column, threshold = 3) {
  x      <- df[[column]]
  z      <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  outliers_idx <- which(abs(z) > threshold)
  pct <- round(length(outliers_idx) / sum(!is.na(x)) * 100, 2)

  message(sprintf(
    "  [Z-score|%.0f] %-20s | Outliers: %d (%.2f%%)",
    threshold, column, length(outliers_idx), pct
  ))

  list(indices = outliers_idx, z_scores = z)
}

#' Aplica el análisis de outliers a todas las columnas numéricas de un dataset
#'
#' @param df    data.frame
#' @param label etiqueta del dataset
analisis_outliers <- function(df, label = "Dataset") {
  message(sprintf("\n[EDA] Valores Atípicos — %s", label))
  num_cols <- names(df)[sapply(df, is.numeric)]

  if (length(num_cols) == 0) {
    message("  No hay columnas numéricas.")
    return(invisible(NULL))
  }

  resultados <- list()
  for (col in num_cols) {
    iqr_res  <- detectar_outliers_iqr(df, col)
    zsc_res  <- detectar_outliers_zscore(df, col)
    resultados[[col]] <- list(iqr = iqr_res, zscore = zsc_res)
  }
  invisible(resultados)
}

# =============================================================================
# 3. NORMALIZACIÓN DE VARIABLES NUMÉRICAS
# =============================================================================

#' Normalización Min-Max: escala los valores al rango [0, 1]
#'
#' @param x vector numérico
#' @return  vector normalizado
normalizar_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))  # constante → 0
  (x - rng[1]) / diff(rng)
}

#' Estandarización Z-score: media 0, desviación estándar 1
#'
#' @param x vector numérico
#' @return  vector estandarizado
estandarizar_zscore <- function(x) {
  mu  <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)
  if (sig == 0) return(rep(0, length(x)))
  (x - mu) / sig
}

#' Agrega columnas normalizadas a un data.frame para todas las variables numéricas
#'
#' Genera columnas con sufijos _minmax y _zscore.
#'
#' @param df data.frame
#' @return   data.frame ampliado con columnas de normalización
agregar_normalizaciones <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  for (col in num_cols) {
    df[[paste0(col, "_minmax")]] <- normalizar_minmax(df[[col]])
    df[[paste0(col, "_zscore")]] <- estandarizar_zscore(df[[col]])
  }
  message(sprintf(
    "  Normalización completada: %d columnas nuevas agregadas",
    length(num_cols) * 2
  ))
  df
}

# =============================================================================
# 4. CREACIÓN DE NUEVAS VARIABLES
# =============================================================================

#' Enriquece el dataset de Siniestros con variables derivadas:
#'   - Numero_Mes: conversión del nombre del mes a número (1-12)
#'   - Franja_Horaria: madrugada / mañana / tarde / noche
#'   - Es_Fin_Semana: TRUE si el día es sábado o domingo
#'
#' @param df data.frame de Siniestros
#' @return   data.frame enriquecido
crear_variables_siniestros <- function(df) {
  message("\n[EDA] Creando nuevas variables — Siniestros")

  # Número de mes
  df$Numero_Mes <- match(df$MM_Acc, MESES_ES)
  message("  + Numero_Mes (1-12) creado")

  # Franja horaria
  if ("Hora_Acc" %in% names(df)) {
    df$Franja_Horaria <- cut(
      df$Hora_Acc,
      breaks = c(-1, 5, 11, 17, 23),
      labels = c("Madrugada (0-5)", "Mañana (6-11)", "Tarde (12-17)", "Noche (18-23)"),
      include.lowest = TRUE
    )
    message("  + Franja_Horaria creada")
  }

  # Fin de semana
  if ("Dia_Semana_Acc" %in% names(df)) {
    df$Es_Fin_Semana <- tolower(df$Dia_Semana_Acc) %in% c("sábado", "domingo")
    message("  + Es_Fin_Semana (lógico) creada")
  }

  df
}

#' Enriquece el dataset de Actor Vial con variables derivadas:
#'   - Edad_num: Edad convertida a numérica (NA donde sea "No Identificado")
#'   - Grupo_Edad: categoría etaria según EDAD_BREAKS / EDAD_LABELS
#'
#' @param df data.frame de Actor Vial
#' @return   data.frame enriquecido
crear_variables_actor <- function(df) {
  message("\n[EDA] Creando nuevas variables — Actor Vial")

  # Edad numérica
  df$Edad_num <- suppressWarnings(as.numeric(df$Edad))
  message("  + Edad_num (numérica) creada")

  # Grupo etario
  df$Grupo_Edad <- cut(
    df$Edad_num,
    breaks = EDAD_BREAKS,
    labels = EDAD_LABELS,
    right  = TRUE,
    include.lowest = TRUE
  )
  message("  + Grupo_Edad (categoría) creada")

  df
}

# =============================================================================
# 5. CORRELACIONES Y TENDENCIAS
# =============================================================================

#' Calcula y muestra la matriz de correlaciones de las variables numéricas
#'
#' Utiliza el método de Pearson. Imprime en consola y devuelve la matriz.
#'
#' @param df    data.frame
#' @param label nombre del dataset para el log
#' @return      matriz de correlación (invisible)
matriz_correlacion <- function(df, label = "Dataset") {
  message(sprintf("\n[EDA] Matriz de correlación — %s", label))

  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) < 2) {
    message("  Menos de 2 variables numéricas, no se puede calcular correlación.")
    return(invisible(NULL))
  }

  mat_cor <- cor(df[num_cols], use = "pairwise.complete.obs", method = "pearson")

  # Mostrar en consola redondeada a 3 decimales
  print(round(mat_cor, 3))

  # Guardar visualización de la matriz de correlación
  charts_dir <- file.path(CHARTS_DIR, "PNG")
  if (!dir.exists(charts_dir)) dir.create(charts_dir, recursive = TRUE)

  png_path <- file.path(charts_dir, paste0("correlacion_", tolower(gsub(" ", "_", label)), ".png"))
  png(png_path, width = 1400, height = 1200, res = 150)
  corrplot(
    mat_cor,
    method  = "color",
    type    = "upper",
    tl.cex  = 0.85,
    addCoef.col = "black",
    number.cex  = 0.7,
    col     = colorRampPalette(c("#e74c3c", "white", "#3498db"))(200),
    title   = paste("Correlación —", label),
    mar     = c(0, 0, 2, 0)
  )
  dev.off()
  message(sprintf("  Gráfico de correlación guardado: %s", png_path))

  invisible(mat_cor)
}

#' Analiza tendencias temporales en Siniestros: total por año y por mes
#'
#' @param df data.frame de Siniestros (debe tener columnas AA_Acc, Numero_Mes)
#' @return   list con tendencia anual y mensual
tendencias_temporales <- function(df) {
  message("\n[EDA] Tendencias temporales — Siniestros")

  # Por año
  tendencia_anual <- df |>
    count(AA_Acc, name = "Siniestros") |>
    arrange(AA_Acc)

  message("  Siniestros por año:")
  print(as.data.frame(tendencia_anual), row.names = FALSE)

  # Por mes (promedio entre todos los años)
  if ("Numero_Mes" %in% names(df)) {
    tendencia_mensual <- df |>
      count(AA_Acc, Numero_Mes, name = "count") |>
      group_by(Numero_Mes) |>
      summarise(Promedio = round(mean(count), 1), .groups = "drop") |>
      arrange(Numero_Mes) |>
      mutate(Mes = MESES_ABREV[Numero_Mes])

    message("\n  Promedio mensual:")
    print(as.data.frame(tendencia_mensual), row.names = FALSE)

    return(invisible(list(anual = tendencia_anual, mensual = tendencia_mensual)))
  }

  invisible(list(anual = tendencia_anual))
}

#' Analiza la distribución de gravedad por franja horaria y día de semana
#'
#' @param df data.frame de Siniestros enriquecido
gravedad_por_tiempo <- function(df) {
  message("\n[EDA] Gravedad por franja horaria")

  if (!"Franja_Horaria" %in% names(df)) {
    message("  Columna Franja_Horaria no encontrada.")
    return(invisible(NULL))
  }

  grav_franja <- df |>
    count(Franja_Horaria, Gravedad_Indicador_Tradicional, name = "N") |>
    arrange(Franja_Horaria, desc(N))

  print(as.data.frame(grav_franja), row.names = FALSE)
  invisible(grav_franja)
}

# =============================================================================
# FUNCIÓN PRINCIPAL — ejecutar EDA completo
# =============================================================================

#' Ejecuta el análisis exploratorio completo sobre los tres datasets
#'
#' @return list con todos los resultados (invisible)
run_eda <- function() {
  message(strrep("=", 60))
  message("INICIO DEL ANÁLISIS EXPLORATORIO — Siniestralidad Bogotá 2024")
  message(strrep("=", 60))

  datos <- load_cleaned()

  df_sin <- datos$siniestros
  df_act <- datos$actor
  df_veh <- datos$vehiculos

  # ── 1. Estadísticas descriptivas ────────────────────────────
  estadisticas_descriptivas(df_sin, "Siniestros")
  estadisticas_descriptivas(df_act, "Actor Vial")
  estadisticas_descriptivas(df_veh, "Vehículos")

  # ── 2. Outliers ─────────────────────────────────────────────
  analisis_outliers(df_sin, "Siniestros")
  analisis_outliers(df_act, "Actor Vial")
  analisis_outliers(df_veh, "Vehículos")

  # ── 3. Normalización (aplicada a Siniestros como ejemplo) ───
  message("\n[EDA] Normalización — Siniestros (primeras 5 filas)")
  df_sin_norm <- agregar_normalizaciones(df_sin)

  # Mostrar columnas normalizadas
  norm_cols <- grep("_minmax|_zscore", names(df_sin_norm), value = TRUE)
  if (length(norm_cols) > 0) {
    print(head(df_sin_norm[norm_cols], 5))
  }

  # ── 4. Nuevas variables ──────────────────────────────────────
  df_sin <- crear_variables_siniestros(df_sin)
  df_act <- crear_variables_actor(df_act)

  # ── 5. Correlaciones ─────────────────────────────────────────
  matriz_correlacion(df_sin, "Siniestros")
  matriz_correlacion(df_act, "Actor Vial")
  matriz_correlacion(df_veh, "Vehículos")

  # ── Tendencias temporales ─────────────────────────────────────
  tend <- tendencias_temporales(df_sin)
  gravedad_por_tiempo(df_sin)

  message(strrep("=", 60))
  message("EDA COMPLETADO")
  message(strrep("=", 60))

  invisible(list(
    siniestros = df_sin,
    actor      = df_act,
    vehiculos  = df_veh,
    tendencias = tend
  ))
}
