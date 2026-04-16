# =============================================================================
# visualizaciones.R — Gráficos con ggplot2 y paquetes complementarios
# Proyecto: Siniestralidad Vial Bogotá 2024
# =============================================================================
# Genera 14 gráficos con:
#   - Fuentes GRANDES y legibles en títulos, ejes y etiquetas
#   - Paletas de color contrastantes (sin amarillo en texto)
#   - Subtítulos explicativos para facilitar la lectura
#   - Fondo blanco limpio con grilla suave
#
# Tipos producidos:
#   Barras, Barras horizontales, Líneas + área, Torta,
#   Histograma, Boxplot, Violín, Mapa de calor
#
# Paquetes: ggplot2, dplyr, tidyr, scales, readxl
# Instalar: install.packages(c("ggplot2","dplyr","tidyr","scales","readxl"))
# =============================================================================

library(ggplot2)   # Sistema principal de gráficos
library(dplyr)     # Manipulación de datos
library(tidyr)     # Pivoteo y reshape
library(scales)    # Formato de ejes (comma, percent)
library(readxl)    # Lectura de archivos Excel limpios

source("R/config.R")  # Variables centralizadas

# =============================================================================
# PALETAS DE COLOR — sin amarillo en texto, alto contraste
# =============================================================================

# Gravedad: azul fuerte / naranja / rojo
PALETTE_GRAV <- c(
  "Solo Daños"  = "#1a6faf",
  "Con Heridos" = "#e07b00",
  "Con Muertos" = "#c0392b"
)

# Sexo
PALETTE_SEXO <- c(
  "MASCULINO"       = "#1a6faf",
  "FEMENINO"        = "#c0392b",
  "No Identificado" = "#7f8c8d"
)

# Paleta secuencial azul oscuro → claro (sin amarillo)
PAL_AZUL  <- "Blues"
PAL_ROJO  <- "Reds"
PAL_VERDE <- "Greens"
PAL_PURP  <- "Purples"
# Para gradientes continuos sin amarillo:
PAL_CALOR <- "YlOrBr"   # Solo se usa en fondo de tiles; texto siempre oscuro

# =============================================================================
# TEMA BASE — fuentes grandes, fondo limpio
# =============================================================================

#' Tema visual uniforme para todos los gráficos del proyecto.
#' Fuentes grandes, ejes claros y sin grilla menor.
tema_base <- function(base = 16) {
  theme_minimal(base_size = base) +
  theme(
    # Título y subtítulo
    plot.title        = element_text(face = "bold", size = base + 8,
                                     hjust = 0.5, color = "#1a1a2e",
                                     margin = margin(b = 6)),
    plot.subtitle     = element_text(size = base + 2, hjust = 0.5,
                                     color = "#4a4a6a",
                                     margin = margin(b = 10)),
    plot.caption      = element_text(size = base - 2, color = "#888888",
                                     hjust = 1),
    # Ejes
    axis.title        = element_text(size = base + 2, face = "bold",
                                     color = "#2c2c4a"),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    axis.text         = element_text(size = base, color = "#2c2c4a"),
    # Grilla
    panel.grid.major  = element_line(color = "#e8e8f0", linewidth = 0.5),
    panel.grid.minor  = element_blank(),
    # Fondo
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    # Leyenda
    legend.title      = element_text(size = base, face = "bold"),
    legend.text       = element_text(size = base - 1),
    legend.position   = "bottom",
    legend.key.size   = unit(0.9, "cm"),
    # Márgenes externos
    plot.margin       = margin(16, 20, 16, 20)
  )
}

# =============================================================================
# UTILIDADES
# =============================================================================

#' Crea directorios de salida PNG y SVG si no existen
ensure_charts_dir <- function() {
  for (sub in c("PNG", "SVG")) {
    d <- file.path(CHARTS_DIR, sub)
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
}

#' Guarda una figura ggplot en PNG (300 dpi) y SVG
#'
#' @param p    objeto ggplot
#' @param name nombre base del archivo (sin extensión)
#' @param w    ancho en pulgadas (default 13)
#' @param h    alto  en pulgadas (default 8)
save_plot <- function(p, name, w = 13, h = 8) {
  ensure_charts_dir()
  ggsave(file.path(CHARTS_DIR, "PNG", paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(CHARTS_DIR, "SVG", paste0(name, ".svg")),
         plot = p, width = w, height = h, bg = "white")
  message(sprintf("  Gráfico guardado: %s (.png + .svg)", name))
}

# =============================================================================
# LECTURA Y PREPARACIÓN DE DATOS
# =============================================================================

#' Lee los datasets limpios y agrega variables auxiliares para los gráficos
load_cleaned_datasets <- function() {
  message("Cargando datasets limpios...")

  df_sin <- read_excel(file.path(CLEANED_DIR, SINIESTROS_OUTPUT))
  df_act <- read_excel(file.path(CLEANED_DIR, ACTOR_OUTPUT))
  df_veh <- read_excel(file.path(CLEANED_DIR, VEHICULOS_OUTPUT))

  # Variables auxiliares en Siniestros
  df_sin$Numero_Mes <- match(df_sin$MM_Acc, MESES_ES)

  if ("Hora_Acc" %in% names(df_sin)) {
    df_sin$Franja_Horaria <- cut(
      df_sin$Hora_Acc,
      breaks = c(-1, 5, 11, 17, 23),
      labels = c("Madrugada (0-5h)", "Mañana (6-11h)",
                 "Tarde (12-17h)", "Noche (18-23h)"),
      include.lowest = TRUE
    )
  }

  if ("Dia_Semana_Acc" %in% names(df_sin)) {
    df_sin$Dia_Semana_Acc <- factor(
      df_sin$Dia_Semana_Acc,
      levels = DIAS_SEMANA_ORDER, ordered = TRUE
    )
  }

  # Variables auxiliares en Actor Vial
  df_act$Edad_num <- suppressWarnings(as.numeric(df_act$Edad))
  df_act$Grupo_Edad <- cut(
    df_act$Edad_num, breaks = EDAD_BREAKS,
    labels = EDAD_LABELS, right = TRUE, include.lowest = TRUE
  )

  message(sprintf("  Siniestros: %d filas | Actor: %d filas | Vehículos: %d filas",
                  nrow(df_sin), nrow(df_act), nrow(df_veh)))

  list(siniestros = df_sin, actor = df_act, vehiculos = df_veh)
}

# =============================================================================
# GRÁFICO 1 — Barras: Gravedad de Siniestros (absoluto)
# =============================================================================
chart_gravedad_barras <- function(df) {
  orden <- c("Solo Daños", "Con Heridos", "Con Muertos")

  conteos <- df |>
    count(Gravedad_Indicador_Tradicional) |>
    mutate(Gravedad_Indicador_Tradicional =
             factor(Gravedad_Indicador_Tradicional, levels = orden)) |>
    filter(!is.na(Gravedad_Indicador_Tradicional))

  p <- ggplot(conteos,
              aes(x = Gravedad_Indicador_Tradicional, y = n,
                  fill = Gravedad_Indicador_Tradicional)) +
    geom_col(width = 0.62, color = "white", linewidth = 0.7) +
    # Etiquetas encima de las barras — tamaño grande, color oscuro
    geom_text(aes(label = comma(n)),
              vjust = -0.5, size = 6.5, fontface = "bold", color = "#1a1a2e") +
    scale_fill_manual(values = PALETTE_GRAV, guide = "none") +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.14))) +
    labs(
      title    = "Gravedad de Siniestros Viales",
      subtitle = "Número total de siniestros por categoría de gravedad",
      x = "Categoría de gravedad",
      y = "Número de siniestros",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "01_gravedad_barras")
  p
}

# =============================================================================
# GRÁFICO 2 — Torta: Gravedad de Siniestros (porcentual)
# =============================================================================
chart_gravedad_torta <- function(df) {
  orden <- c("Solo Daños", "Con Heridos", "Con Muertos")

  conteos <- df |>
    count(Gravedad_Indicador_Tradicional) |>
    filter(!is.na(Gravedad_Indicador_Tradicional)) |>
    mutate(
      pct   = n / sum(n),
      Grav  = factor(Gravedad_Indicador_Tradicional, levels = orden),
      label = paste0(Gravedad_Indicador_Tradicional,
                     "\n", comma(n), " (", percent(pct, accuracy = 0.1), ")")
    )

  p <- ggplot(conteos, aes(x = "", y = pct, fill = Grav)) +
    geom_col(width = 1, color = "white", linewidth = 1.2) +
    coord_polar(theta = "y", start = 0) +
    # Texto blanco en rojo/naranja/azul oscuro → siempre legible
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 5.5, fontface = "bold", color = "white",
              lineheight = 1.3) +
    scale_fill_manual(values = PALETTE_GRAV, name = "Gravedad") +
    labs(
      title    = "Distribución Porcentual de la Gravedad",
      subtitle = "Proporción de cada tipo de siniestro sobre el total",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base() +
    theme(
      axis.text  = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )

  save_plot(p, "02_gravedad_torta", w = 10, h = 10)
  p
}

# =============================================================================
# GRÁFICO 3 — Barras + Línea: Evolución anual de siniestros
# =============================================================================
chart_evolucion_anual <- function(df) {
  anual <- df |>
    count(AA_Acc, name = "Siniestros") |>
    arrange(AA_Acc) |>
    mutate(AA_Acc = as.character(AA_Acc))

  p <- ggplot(anual, aes(x = AA_Acc, y = Siniestros)) +
    geom_col(fill = "#1a6faf", alpha = 0.88, color = "white",
             width = 0.68, linewidth = 0.6) +
    # Línea de tendencia encima
    geom_line(aes(group = 1), color = "#c0392b", linewidth = 2.2) +
    geom_point(color = "#c0392b", size = 5, shape = 21,
               fill = "white", stroke = 2.5) +
    # Etiquetas: color oscuro, tamaño grande
    geom_text(aes(label = comma(Siniestros)),
              vjust = -0.8, size = 6, fontface = "bold", color = "#1a1a2e") +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = "Evolución Anual de Siniestros Viales",
      subtitle = "Barras = total por año | Línea roja = tendencia",
      x = "Año", y = "Número de siniestros",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "03_evolucion_anual")
  p
}

# =============================================================================
# GRÁFICO 4 — Línea + Área: Promedio mensual de siniestros
# =============================================================================
chart_promedio_mensual <- function(df) {
  mensual <- df |>
    count(AA_Acc, Numero_Mes, name = "count") |>
    group_by(Numero_Mes) |>
    summarise(Promedio = mean(count, na.rm = TRUE), .groups = "drop") |>
    filter(!is.na(Numero_Mes)) |>
    mutate(Mes = factor(MESES_ABREV[Numero_Mes], levels = MESES_ABREV))

  p <- ggplot(mensual, aes(x = Mes, y = Promedio, group = 1)) +
    geom_area(fill = "#1a6faf", alpha = 0.18) +
    geom_line(color = "#1a6faf", linewidth = 2.5) +
    geom_point(color = "#1a6faf", size = 5.5, shape = 21,
               fill = "white", stroke = 2.5) +
    # Etiquetas: color azul oscuro, tamaño grande
    geom_text(aes(label = round(Promedio, 0)),
              vjust = -1.1, size = 5.5, fontface = "bold",
              color = "#1a3a5c") +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Promedio Mensual de Siniestros Viales",
      subtitle = "Promedio calculado entre todos los años del registro",
      x = "Mes", y = "Promedio de siniestros",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "04_promedio_mensual")
  p
}

# =============================================================================
# GRÁFICO 5 — Barras horizontales: Tipo de Objeto Fijo
# =============================================================================
chart_tipo_objeto_fijo <- function(df) {
  conteos <- df |>
    count(Tipo_Objeto_Fijo, name = "N") |>
    filter(!is.na(Tipo_Objeto_Fijo)) |>
    mutate(Tipo_Objeto_Fijo = reorder(Tipo_Objeto_Fijo, N))

  p <- ggplot(conteos, aes(x = N, y = Tipo_Objeto_Fijo)) +
    geom_col(fill = "#1a6faf", color = "white", linewidth = 0.5) +
    # Etiquetas a la derecha, color oscuro, tamaño grande
    geom_text(aes(label = comma(N)),
              hjust = -0.12, size = 5.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_x_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Siniestros por Tipo de Objeto Fijo Involucrado",
      subtitle = "Frecuencia de cada tipo de obstáculo en las colisiones",
      x = "Número de siniestros", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "05_tipo_objeto_fijo", h = 9)
  p
}

# =============================================================================
# GRÁFICO 6 — Histograma: Distribución horaria (sin amarillo)
# =============================================================================
chart_distribucion_horaria <- function(df) {
  horario <- df |>
    filter(!is.na(Hora_Acc)) |>
    count(Hora_Acc, name = "N") |>
    right_join(tibble(Hora_Acc = 0:23), by = "Hora_Acc") |>
    mutate(N = coalesce(N, 0L))

  # Gradiente: azul claro (madrugada) → rojo oscuro (tarde/noche)
  # Evita amarillo usando colorRampPalette con azul-naranja-rojo
  colores_hora <- colorRampPalette(
    c("#2980b9", "#1a6faf", "#e07b00", "#c0392b", "#922b21", "#c0392b",
      "#e07b00", "#1a6faf")
  )(24)

  p <- ggplot(horario, aes(x = Hora_Acc, y = N, fill = factor(Hora_Acc))) +
    geom_col(color = "white", linewidth = 0.4) +
    # Etiquetas: color oscuro fijo, tamaño grande
    geom_text(aes(label = ifelse(N > 0, N, "")),
              vjust = -0.5, size = 4.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_fill_manual(values = colores_hora, guide = "none") +
    scale_x_continuous(
      breaks = 0:23,
      labels = sprintf("%02d:00", 0:23)
    ) +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.13))) +
    labs(
      title    = "Distribución Horaria de Siniestros Viales",
      subtitle = "Número de siniestros según la hora del día (0 a 23 horas)",
      x = "Hora del día", y = "Número de siniestros",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))

  save_plot(p, "06_distribucion_horaria", w = 16, h = 8)
  p
}

# =============================================================================
# GRÁFICO 7 — Barras horizontales: Top 10 localidades
# =============================================================================
chart_top10_localidades <- function(df) {
  top10 <- df |>
    count(Localidad, name = "N") |>
    filter(!is.na(Localidad)) |>
    slice_max(N, n = 10) |>
    mutate(
      Localidad = reorder(Localidad, N),
      # Color más intenso para las localidades con más siniestros
      pct_max = N / max(N)
    )

  # Paleta: azul oscuro para el mayor, azul claro para el menor
  colores <- colorRampPalette(c("#aed6f1", "#1a6faf"))(10)

  p <- ggplot(top10, aes(x = N, y = Localidad, fill = N)) +
    geom_col(color = "white", linewidth = 0.5) +
    geom_text(aes(label = comma(N)),
              hjust = -0.12, size = 5.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_fill_gradient(low = "#aed6f1", high = "#1a4f8a", guide = "none") +
    scale_x_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Top 10 Localidades con Más Siniestros Viales",
      subtitle = "Localidades ordenadas de mayor a menor frecuencia",
      x = "Número de siniestros", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "07_top10_localidades")
  p
}

# =============================================================================
# GRÁFICO 8 — Mapa de calor: Intensidad por día y hora (sin amarillo en texto)
# =============================================================================
chart_heatmap_dia_hora <- function(df) {
  if (!all(c("Dia_Semana_Acc", "Hora_Acc") %in% names(df))) {
    message("  Heatmap: columnas requeridas no encontradas.")
    return(invisible(NULL))
  }

  heatmap_data <- df |>
    filter(!is.na(Dia_Semana_Acc), !is.na(Hora_Acc)) |>
    count(Dia_Semana_Acc, Hora_Acc, name = "N") |>
    mutate(
      Dia_Semana_Acc = factor(Dia_Semana_Acc,
                               levels = rev(DIAS_SEMANA_ORDER)),
      # Color del texto: blanco si la celda es oscura, negro si es clara
      text_color = ifelse(N > quantile(N, 0.55, na.rm = TRUE),
                          "white", "#1a1a2e")
    )

  p <- ggplot(heatmap_data,
              aes(x = factor(Hora_Acc), y = Dia_Semana_Acc, fill = N)) +
    geom_tile(color = "white", linewidth = 0.6) +
    # Texto adaptativo: blanco en celdas oscuras, negro en celdas claras
    geom_text(aes(label = N, color = text_color),
              size = 3.8, fontface = "bold") +
    scale_color_identity() +
    # Paleta naranja-rojo sin amarillo puro
    scale_fill_gradient(low = "#fde8d8", high = "#8b0000",
                        name = "Número de\nsiniestros",
                        labels = comma) +
    scale_x_discrete(labels = sprintf("%02d", 0:23)) +
    scale_y_discrete(labels = function(x) tools::toTitleCase(x)) +
    labs(
      title    = "Mapa de Calor — Siniestros por Día y Hora",
      subtitle = "Intensidad de siniestros: rojo oscuro = más accidentes",
      x = "Hora del día (00 a 23)", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base() +
    theme(
      axis.text.x    = element_text(size = 12),
      axis.text.y    = element_text(size = 14, face = "bold"),
      legend.position = "right"
    )

  save_plot(p, "08_heatmap_dia_hora", w = 18, h = 8)
  p
}

# =============================================================================
# GRÁFICO 9 — Barras horizontales: Top 10 factores contribuyentes
# =============================================================================
chart_top10_factores <- function(df) {
  con_cols <- grep("^Con_", names(df), value = TRUE)
  if (length(con_cols) == 0) {
    message("  No se encontraron columnas Con_*")
    return(invisible(NULL))
  }

  factores <- sapply(con_cols, function(col) {
    sum(df[[col]] == "SI", na.rm = TRUE)
  })

  top10 <- tibble(
    Factor = CON_LABELS[names(factores)],
    N      = factores
  ) |>
    filter(!is.na(Factor), N > 0) |>
    slice_max(N, n = 10) |>
    mutate(Factor = reorder(Factor, N))

  p <- ggplot(top10, aes(x = N, y = Factor, fill = N)) +
    geom_col(color = "white", linewidth = 0.5) +
    geom_text(aes(label = comma(N)),
              hjust = -0.12, size = 5.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_fill_gradient(low = "#f4a6a0", high = "#8b0000", guide = "none") +
    scale_x_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.20))) +
    labs(
      title    = "Top 10 Factores Contribuyentes en Siniestros",
      subtitle = "Columnas 'Con_*': número de siniestros donde el factor estuvo presente",
      x = "Número de siniestros", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "09_top10_factores")
  p
}

# =============================================================================
# GRÁFICO 10 — Barras horizontales: Top 10 elementos de choque
# =============================================================================
chart_top10_elementos_choque <- function(df) {
  top10 <- df |>
    count(Tipo_Objeto_Fijo, name = "N") |>
    filter(!is.na(Tipo_Objeto_Fijo)) |>
    slice_max(N, n = 10) |>
    mutate(Tipo_Objeto_Fijo = reorder(Tipo_Objeto_Fijo, N))

  p <- ggplot(top10, aes(x = N, y = Tipo_Objeto_Fijo, fill = N)) +
    geom_col(color = "white", linewidth = 0.5) +
    geom_text(aes(label = comma(N)),
              hjust = -0.12, size = 5.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_fill_gradient(low = "#a8d5a2", high = "#1a5c1a", guide = "none") +
    scale_x_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.20))) +
    labs(
      title    = "Top 10 Tipos de Objeto Fijo en Colisiones",
      subtitle = "Objetos más frecuentes con los que impactaron los vehículos",
      x = "Número de siniestros", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "10_top10_elementos_choque")
  p
}

# =============================================================================
# GRÁFICO 11 — Barras: Distribución por Sexo
# =============================================================================
chart_distribucion_sexo <- function(df) {
  conteos <- df |>
    count(Sexo, name = "N") |>
    filter(!is.na(Sexo)) |>
    arrange(desc(N)) |>
    mutate(
      pct       = N / sum(N),
      label_top = comma(N),
      label_pct = percent(pct, accuracy = 0.1),
      Sexo      = factor(Sexo, levels = Sexo)
    )

  p <- ggplot(conteos, aes(x = Sexo, y = N, fill = Sexo)) +
    geom_col(width = 0.60, color = "white", linewidth = 0.8) +
    # Número absoluto encima
    geom_text(aes(label = label_top),
              vjust = -1.5, size = 6.5, fontface = "bold",
              color = "#1a1a2e") +
    # Porcentaje justo sobre la barra
    geom_text(aes(label = paste0("(", label_pct, ")")),
              vjust = -0.1, size = 5, color = "#4a4a6a") +
    scale_fill_manual(values = PALETTE_SEXO, guide = "none") +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Distribución de Actores Viales por Sexo",
      subtitle = "Número y porcentaje de personas involucradas según sexo",
      x = NULL, y = "Número de actores viales",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "11_distribucion_sexo")
  p
}

# =============================================================================
# GRÁFICO 12a — Barras: Distribución por Grupo de Edad
# =============================================================================
chart_grupos_edad_barras <- function(df) {
  df_edad <- df |> filter(!is.na(Grupo_Edad))

  # Paleta fría → cálida sin amarillo
  colores_edad <- c("#1a6faf", "#2196a6", "#2e8b57",
                    "#e07b00", "#c0392b", "#8b0000")

  p <- ggplot(df_edad, aes(x = Grupo_Edad, fill = Grupo_Edad)) +
    geom_bar(color = "white", linewidth = 0.6) +
    geom_text(
      stat = "count",
      aes(label = after_stat(comma(count))),
      vjust = -0.6, size = 6, fontface = "bold", color = "#1a1a2e"
    ) +
    scale_fill_manual(values = colores_edad, guide = "none") +
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = "Distribución de Actores Viales por Grupo de Edad",
      subtitle = "Agrupación en seis tramos etarios según rango de edad",
      x = "Grupo de edad", y = "Número de actores",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base() +
    theme(axis.text.x = element_text(angle = 22, hjust = 1, size = 13))

  save_plot(p, "12a_grupos_edad_barras", w = 13, h = 8)
  p
}

# =============================================================================
# GRÁFICO 12b — Boxplot: Edad numérica por Grupo
# =============================================================================
chart_grupos_edad_boxplot <- function(df) {
  df_box <- df |>
    filter(!is.na(Edad_num), !is.na(Grupo_Edad), Edad_num <= 110)

  colores_edad <- c("#1a6faf", "#2196a6", "#2e8b57",
                    "#e07b00", "#c0392b", "#8b0000")

  p <- ggplot(df_box, aes(x = Grupo_Edad, y = Edad_num, fill = Grupo_Edad)) +
    geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.35,
                 color = "#2c2c4a", linewidth = 0.7) +
    # Punto de media: blanco visible sobre cualquier color
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 4.5, fill = "white", color = "#1a1a2e",
                 stroke = 1.5, show.legend = FALSE) +
    scale_fill_manual(values = colores_edad, guide = "none") +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(
      title    = "Distribución de Edad por Grupo Etario",
      subtitle = "Boxplot con mediana (línea) y media (diamante blanco)",
      x = "Grupo de edad", y = "Edad (años)",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base() +
    theme(axis.text.x = element_text(angle = 22, hjust = 1, size = 13))

  save_plot(p, "12b_grupos_edad_boxplot", w = 13, h = 8)
  p
}

# =============================================================================
# GRÁFICO 13 — Barras horizontales: Tipos de Vehículos
# =============================================================================
chart_tipos_vehiculo <- function(df) {
  top15 <- df |>
    count(Clase, name = "N") |>
    filter(!is.na(Clase)) |>
    slice_max(N, n = 15) |>
    mutate(Clase = reorder(Clase, N))

  p <- ggplot(top15, aes(x = N, y = Clase, fill = N)) +
    geom_col(color = "white", linewidth = 0.5) +
    geom_text(aes(label = comma(N)),
              hjust = -0.12, size = 5.5, fontface = "bold",
              color = "#1a1a2e") +
    scale_fill_gradient(low = "#c9a9e0", high = "#4a0080", guide = "none") +
    scale_x_continuous(labels = comma,
                       expand = expansion(mult = c(0, 0.20))) +
    labs(
      title    = "Tipos de Vehículos Involucrados en Siniestros",
      subtitle = "Top 15 clases de vehículos según frecuencia en siniestros",
      x = "Número de vehículos", y = NULL,
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "13_tipos_vehiculo", h = 9)
  p
}

# =============================================================================
# GRÁFICO 14 — Violín + Boxplot: Hora del día por Gravedad
# =============================================================================
chart_violin_hora_gravedad <- function(df) {
  orden_grav <- c("Solo Daños", "Con Heridos", "Con Muertos")
  df_plot <- df |>
    filter(!is.na(Hora_Acc),
           Gravedad_Indicador_Tradicional %in% orden_grav) |>
    mutate(Gravedad = factor(Gravedad_Indicador_Tradicional,
                             levels = orden_grav))

  p <- ggplot(df_plot, aes(x = Gravedad, y = Hora_Acc, fill = Gravedad)) +
    geom_violin(trim = FALSE, alpha = 0.75, linewidth = 0.4,
                color = "#2c2c4a") +
    geom_boxplot(width = 0.14, fill = "white", color = "#2c2c4a",
                 outlier.size = 1.2, outlier.alpha = 0.35,
                 linewidth = 0.8) +
    # Media como punto de diamante oscuro visible sobre blanco
    stat_summary(fun = mean, geom = "point", shape = 23, size = 5,
                 fill = "#1a1a2e", color = "white",
                 stroke = 1.5, show.legend = FALSE) +
    scale_fill_manual(values = PALETTE_GRAV, guide = "none") +
    scale_y_continuous(
      breaks = seq(0, 23, 3),
      labels = sprintf("%02d:00 h", seq(0, 23, 3))
    ) +
    labs(
      title    = "Distribución Horaria según Gravedad del Siniestro",
      subtitle = "Violín = densidad | Caja = IQR | Diamante = media | Línea = mediana",
      x = "Categoría de gravedad", y = "Hora del día",
      caption  = "Fuente: Anuario de Siniestralidad Vial Bogotá 2024"
    ) +
    tema_base()

  save_plot(p, "14_violin_hora_gravedad")
  p
}

# =============================================================================
# FUNCIÓN PRINCIPAL — generar todos los gráficos
# =============================================================================

#' Genera los 14 gráficos del proyecto y los guarda en R/Charts/
#'
#' @return list con los objetos ggplot (invisible)
generate_all_charts <- function() {
  message(strrep("=", 62))
  message("GENERANDO GRÁFICOS — Siniestralidad Vial Bogotá")
  message(strrep("=", 62))

  t_ini <- proc.time()["elapsed"]
  datos <- load_cleaned_datasets()

  df_sin <- datos$siniestros
  df_act <- datos$actor
  df_veh <- datos$vehiculos

  total <- 14
  plots <- list()

  message(sprintf("[1/%d]  Gravedad — Barras",              total)); plots$g1  <- chart_gravedad_barras(df_sin)
  message(sprintf("[2/%d]  Gravedad — Torta %%",            total)); plots$g2  <- chart_gravedad_torta(df_sin)
  message(sprintf("[3/%d]  Evolución anual",                total)); plots$g3  <- chart_evolucion_anual(df_sin)
  message(sprintf("[4/%d]  Promedio mensual",               total)); plots$g4  <- chart_promedio_mensual(df_sin)
  message(sprintf("[5/%d]  Tipo de Objeto Fijo",            total)); plots$g5  <- chart_tipo_objeto_fijo(df_sin)
  message(sprintf("[6/%d]  Distribución horaria",           total)); plots$g6  <- chart_distribucion_horaria(df_sin)
  message(sprintf("[7/%d]  Top 10 localidades",             total)); plots$g7  <- chart_top10_localidades(df_sin)
  message(sprintf("[8/%d]  Heatmap día × hora",             total)); plots$g8  <- chart_heatmap_dia_hora(df_sin)
  message(sprintf("[9/%d]  Top 10 factores",                total)); plots$g9  <- chart_top10_factores(df_sin)
  message(sprintf("[10/%d] Top 10 elementos de choque",     total)); plots$g10 <- chart_top10_elementos_choque(df_sin)
  message(sprintf("[11/%d] Distribución por sexo",          total)); plots$g11 <- chart_distribucion_sexo(df_act)
  message(sprintf("[12/%d] Grupos edad — Barras",           total)); plots$g12a <- chart_grupos_edad_barras(df_act)
  message(sprintf("[12/%d] Grupos edad — Boxplot",          total)); plots$g12b <- chart_grupos_edad_boxplot(df_act)
  message(sprintf("[13/%d] Tipos de vehículos",             total)); plots$g13 <- chart_tipos_vehiculo(df_veh)
  message(sprintf("[14/%d] Violín hora × gravedad",         total)); plots$g14 <- chart_violin_hora_gravedad(df_sin)

  elapsed <- proc.time()["elapsed"] - t_ini
  message(strrep("=", 62))
  message(sprintf("COMPLETADO: %d gráficos generados en %.1f s", total, elapsed))
  message(sprintf("Ubicación: %s", CHARTS_DIR))
  message(strrep("=", 62))

  invisible(plots)
}
