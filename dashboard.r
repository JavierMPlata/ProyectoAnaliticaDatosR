# =============================================================================
# dashboard.R — Dashboard · Siniestralidad Vial Bogotá 2024
# 18 gráficas interactivas + sliders top-N + anotaciones explicativas
# shinydashboard + ggplotly + Leaflet
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(scales)

source("config.R")

# =============================================================================
# CARGA DE DATOS
# =============================================================================

load_dashboard_data <- function() {
  message("[dashboard] Cargando datos limpios...")

  df_sin <- read_excel(file.path(CLEANED_DIR, SINIESTROS_OUTPUT))
  df_act <- read_excel(file.path(CLEANED_DIR, ACTOR_OUTPUT))
  df_veh <- read_excel(file.path(CLEANED_DIR, VEHICULOS_OUTPUT))

  df_sin$Severo      <- ifelse(df_sin$Gravedad_Indicador_Tradicional %in%
                                 c("Con Heridos", "Con Muertos"), "Severo", "No Severo")
  df_sin$Numero_Mes  <- match(df_sin$MM_Acc, MESES_ES)
  df_sin$MM_Acc      <- factor(df_sin$MM_Acc, levels = MESES_ES, ordered = TRUE)
  df_sin$Dia_Semana_Acc <- factor(df_sin$Dia_Semana_Acc,
                                   levels = DIAS_SEMANA_ORDER, ordered = TRUE)

  df_act$Edad_num   <- suppressWarnings(as.numeric(df_act$Edad))
  df_act$Grupo_Edad <- cut(df_act$Edad_num, breaks = EDAD_BREAKS,
                            labels = EDAD_LABELS, right = TRUE,
                            include.lowest = TRUE)

  message("[dashboard] Procesando coordenadas...")
  lat_col <- intersect(c("Latitud","latitud","LATITUD"), names(df_sin))[1]
  lon_col <- intersect(c("Longitud","longitud","LONGITUD"), names(df_sin))[1]

  coords_df <- data.frame(lat = numeric(0), lon = numeric(0),
                           Gravedad = character(0), Localidad = character(0),
                           Anio = character(0))
  if (!is.na(lat_col) && !is.na(lon_col)) {
    coords_df <- df_sin |>
      select(lat = all_of(lat_col), lon = all_of(lon_col),
             Gravedad = Gravedad_Indicador_Tradicional,
             Localidad, Anio = AA_Acc) |>
      mutate(lat  = suppressWarnings(as.numeric(gsub(",",".", as.character(lat)))),
             lon  = suppressWarnings(as.numeric(gsub(",",".", as.character(lon)))),
             Anio = as.character(Anio)) |>
      filter(!is.na(lat), !is.na(lon),
             lat >= 3.5, lat <= 5.5, lon >= -75.0, lon <= -73.0)
    message(sprintf("[dashboard] Coordenadas válidas: %d puntos", nrow(coords_df)))
  }
  message(sprintf("[dashboard] Sin: %d | Act: %d | Veh: %d",
                  nrow(df_sin), nrow(df_act), nrow(df_veh)))
  list(siniestros = df_sin, actor = df_act, vehiculos = df_veh, coords = coords_df)
}

# =============================================================================
# FUNCIÓN PRINCIPAL
# =============================================================================

run_dashboard <- function(launch_browser = TRUE) {
  message("\n", strrep("=", 62))
  message("  INICIANDO DASHBOARD — 18 GRÁFICAS")
  message(strrep("=", 62))

  datos      <- load_dashboard_data()
  df_sin     <- datos$siniestros
  df_act     <- datos$actor
  df_veh     <- datos$vehiculos
  coords_all <- datos$coords

  anios       <- sort(unique(as.character(df_sin$AA_Acc)))
  localidades <- sort(unique(df_sin$Localidad[!is.na(df_sin$Localidad)]))

  PAL_GRAV <- c("Solo Da\u00f1os" = "#1a6faf",
                "Con Heridos"     = "#e07b00",
                "Con Muertos"     = "#c0392b")
  PAL_SEXO <- c("MASCULINO"       = "#1a6faf",
                "FEMENINO"        = "#c0392b",
                "No Identificado" = "#7f8c8d")

  # Tema base para ggplot
  tema <- function()
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor  = element_blank(),
          plot.background   = element_rect(fill = "white", color = NA),
          panel.background  = element_rect(fill = "white", color = NA),
          plot.subtitle     = element_text(size = 10, color = "#555555",
                                           margin = margin(b = 8)),
          plot.caption      = element_text(size = 9,  color = "#888888", hjust = 1))

  # Box con slider top-N + descripción + gráfica
  box_top <- function(id, titulo, slider_id, slider_label,
                      descripcion, min_v = 3, max_v = 20, def = 10,
                      status = "primary", h = "420px", w = 12) {
    box(
      width = w, title = titulo, status = status, solidHeader = TRUE,
      fluidRow(
        column(3,
          sliderInput(slider_id, slider_label,
                      min = min_v, max = max_v, value = def,
                      step = 1, ticks = FALSE)
        ),
        column(9,
          p(descripcion,
            style = "font-style:italic; color:#555; font-size:14px;
                     margin-top:12px; line-height:1.5;")
        )
      ),
      plotlyOutput(id, height = h)
    )
  }

  # Box estándar (sin slider)
  cbox <- function(id, titulo, descripcion, status = "primary",
                   h = "380px", w = 6) {
    box(
      width = w, title = titulo, status = status, solidHeader = TRUE,
      p(descripcion,
        style = "font-style:italic; color:#555; font-size:14px;
                 margin-bottom:6px; line-height:1.5;"),
      plotlyOutput(id, height = h)
    )
  }

  # ============================================================================
  # UI
  # ============================================================================

  ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Siniestralidad Bogot\u00e1 2024"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Resumen",        tabName = "resumen",     icon = icon("tachometer-alt")),
        menuItem("Temporal",       tabName = "temporal",    icon = icon("clock")),
        menuItem("Geograf\u00eda", tabName = "geografia",   icon = icon("map")),
        menuItem("Actores",        tabName = "actores",     icon = icon("users")),
        menuItem("Veh\u00edculos", tabName = "vehiculos",   icon = icon("car")),
        menuItem("Factores",       tabName = "factores",    icon = icon("exclamation-circle")),
        menuItem("Correlaciones",  tabName = "correlacion", icon = icon("project-diagram")),
        menuItem("Conclusiones",   tabName = "conclusion",  icon = icon("lightbulb"))
      ),
      hr(),
      tags$p("Filtros globales", style = "color:#aaa; padding:6px 14px;
             font-size:0.85em; font-weight:600; margin:0;"),
      radioButtons("modo", NULL,
                   choices  = c("Interactivo", "General (todos los a\u00f1os)"),
                   selected = "General (todos los a\u00f1os)"),
      conditionalPanel(
        condition = "input.modo == 'Interactivo'",
        selectInput("anio", "A\u00f1o:", choices = anios, selected = tail(anios, 1))
      ),
      checkboxInput("soloSevero", "Solo severos (heridos + muertos)", FALSE),
      selectInput("localidad", "Localidad:",
                  choices = c("Todas", localidades), selected = "Todas",
                  selectize = TRUE),
      hr(),
      tags$p("Los filtros se aplican a todas las gr\u00e1ficas.",
             style = "color:#aaa; padding:0 14px; font-size:0.8em;")
    ),

    dashboardBody(
      tags$head(tags$style(HTML("
        .content-wrapper { background:#f4f6f9; }
        .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,.09); }
        .small-box { border-radius:8px; }
        .main-header .logo { font-weight:700; }
        .irs-single, .irs-bar, .irs-bar-edge { background:#1a6faf !important;
          border-color:#1a6faf !important; }
      "))),

      tabItems(

        # ═══════════════════════════════════════════════════════════
        # TAB 1 · RESUMEN  — gráficas 01 02 03 04
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "resumen",
          fluidRow(
            valueBoxOutput("kpi_total",      width = 4),
            valueBoxOutput("kpi_severos",    width = 4),
            valueBoxOutput("kpi_porcentaje", width = 4)
          ),
          fluidRow(
            cbox("p01_grav_barras",
                 "01 · Gravedad de Siniestros — Barras",
                 "Compara la cantidad total de accidentes seg\u00fan su nivel de gravedad.
                  'Solo Da\u00f1os' son choques sin personas heridas; 'Con Heridos' incluye
                  al menos una persona lesionada; 'Con Muertos' registra v\u00edctimas fatales.",
                 status = "primary", h = "380px", w = 7),
            cbox("p02_grav_torta",
                 "02 · Proporci\u00f3n de Gravedad",
                 "Muestra qu\u00e9 porcentaje del total representa cada categor\u00eda de gravedad.
                  Permite identificar de un vistazo si los accidentes tienden a ser leves
                  o si hay una proporci\u00f3n preocupante de casos graves.",
                 status = "primary", h = "380px", w = 5)
          ),
          fluidRow(
            cbox("p03_evolucion",
                 "03 · Evoluci\u00f3n Anual de Siniestros",
                 "Barras azules = total de accidentes en cada a\u00f1o. L\u00ednea roja = tendencia
                  general. Si la l\u00ednea sube, los accidentes aumentan; si baja, disminuyen.
                  La l\u00ednea naranja horizontal representa el promedio hist\u00f3rico.",
                 status = "info", h = "380px", w = 6),
            cbox("p04_mensual",
                 "04 · Promedio Mensual de Siniestros",
                 "\u00c1rea azul y l\u00ednea = promedio de accidentes en cada mes, calculado
                  sobre todos los a\u00f1os disponibles. Permite identificar \u00e9pocas del a\u00f1o
                  con mayor riesgo vial.",
                 status = "info", h = "380px", w = 6)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 2 · TEMPORAL  — gráficas 06 08 14
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "temporal",
          fluidRow(
            cbox("p06_horaria",
                 "06 · Distribuci\u00f3n Horaria de Siniestros",
                 "Cada barra muestra cu\u00e1ntos accidentes ocurrieron en esa hora del d\u00eda
                  (de 0:00 a 23:00). Las franjas de color de fondo delimitan los per\u00edodos
                  del d\u00eda: madrugada, ma\u00f1ana, tarde y noche.",
                 status = "warning", h = "380px", w = 12)
          ),
          fluidRow(
            cbox("p08_heatmap",
                 "08 · Mapa de Calor — D\u00eda de la Semana \u00d7 Hora del D\u00eda",
                 "Cada celda es la combinaci\u00f3n de un d\u00eda y una hora. El color indica
                  cu\u00e1ntos accidentes ocurrieron en ese momento: blanco/naranja claro = pocos,
                  rojo oscuro = muchos. Revela los horarios m\u00e1s peligrosos de la semana.",
                 status = "danger", h = "400px", w = 12)
          ),
          fluidRow(
            cbox("p14_violin",
                 "14 · Distribuci\u00f3n Horaria por Gravedad del Siniestro (Viol\u00edn)",
                 "La forma del viol\u00edn muestra a qu\u00e9 horas se concentran m\u00e1s accidentes
                  de cada tipo. La caja interior marca el rango del 50 \u0025 central de los datos
                  (Q1\u2013Q3). Si el viol\u00edn es ancho en cierta hora, muchos accidentes ocurren ah\u00ed.",
                 status = "warning", h = "440px", w = 12)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 3 · GEOGRAFÍA  — mapa leaflet + gráfica 07
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "geografia",
          fluidRow(
            valueBoxOutput("kpi_mapa_total",   width = 4),
            valueBoxOutput("kpi_mapa_heridos", width = 4),
            valueBoxOutput("kpi_mapa_muertos", width = 4)
          ),
          fluidRow(
            box(width = 12,
                title       = "Mapa de Siniestros Viales — Bogot\u00e1",
                status      = "primary",
                solidHeader = TRUE,
                p("Cambia entre 'Cluster' y 'Mapa de Calor' con el control superior derecho
                   del mapa. Los c\u00edrculos agrupan accidentes cercanos (el n\u00famero indica
                   cu\u00e1ntos hay). Haz clic en un grupo para acercar y ver los puntos
                   individuales. El mapa de calor muestra densidad: rojo = zona cr\u00edtica.",
                  style = "font-style:italic; color:#555; font-size:11px;
                           margin-bottom:8px; line-height:1.5;"),
                leafletOutput("mapa_pro", height = "560px"))
          ),
          fluidRow(
            box_top("p07_localidades",
                    "07 · Localidades con M\u00e1s Siniestros",
                    "top_loc", "N\u00famero de localidades a mostrar:",
                    "Ranking de localidades de Bogot\u00e1 ordenadas de mayor a menor n\u00famero
                     de accidentes. Usa el control para ver m\u00e1s o menos localidades y
                     comparar cu\u00e1les concentran el mayor riesgo vial.",
                    min_v = 3, max_v = length(localidades), def = 10,
                    status = "primary", h = "420px", w = 12)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 4 · ACTORES  — gráficas 11 12a 12b
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "actores",
          fluidRow(
            cbox("p11_sexo",
                 "11 · Actores Viales por Sexo",
                 "Personas involucradas en accidentes clasificadas por sexo.
                  Incluye conductores, pasajeros y peatones. El porcentaje
                  muestra la proporci\u00f3n de cada grupo sobre el total.",
                 status = "primary", h = "360px", w = 5),
            cbox("p12a_edad_barras",
                 "12a · Actores Viales por Grupo de Edad",
                 "Las personas involucradas se agrupan en seis rangos etarios.
                  Permite identificar cu\u00e1les grupos de edad son los m\u00e1s
                  vulnerables o frecuentes en los siniestros viales.",
                 status = "primary", h = "360px", w = 7)
          ),
          fluidRow(
            cbox("p12b_edad_box",
                 "12b \u00b7 Edad seg\u00fan Gravedad de la Lesi\u00f3n (Boxplot)",
                 "\u00bfLas personas que fallecen o resultan heridas tienen edades distintas
                  a las ilesas? Cada caja muestra la distribuci\u00f3n de edades para un
                  nivel de gravedad: mediana (l\u00ednea), rango 50\u0025 central (caja),
                  promedio (rombo) y edades at\u00edpicas (puntos).",
                 status = "info", h = "420px", w = 12)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 5 · VEHÍCULOS  — gráficas 05 10 13
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "vehiculos",
          fluidRow(
            box_top("p13_tipos_veh",
                    "13 · Tipos de Veh\u00edculos Involucrados en Siniestros",
                    "top_veh", "N\u00famero de tipos a mostrar:",
                    "Clases de veh\u00edculo que aparecen con m\u00e1s frecuencia en los registros
                     de accidentes. Un veh\u00edculo 'involucrado' no significa que sea el
                     culpable: puede ser v\u00edctima o partee del siniestro.",
                    min_v = 3, max_v = 20, def = 15,
                    status = "primary", h = "440px", w = 6),
            box_top("p05_obj_fijo",
                    "05 · Tipo de Objeto Fijo en el Siniestro",
                    "top_obj", "N\u00famero de tipos a mostrar:",
                    "Elementos fijos de la v\u00eda o entorno urbano contra los que impactaron
                     los veh\u00edculos. Incluye postes, separadores, \u00e1rboles, etc.
                     Ayuda a identificar qu\u00e9 elementos de la infraestructura generan riesgo.",
                    min_v = 3, max_v = 20, def = 10,
                    status = "warning", h = "440px", w = 6)
          ),
          fluidRow(
            box_top("p10_choque",
                    "10 · Elementos de Choque M\u00e1s Frecuentes",
                    "top_choque", "N\u00famero de elementos a mostrar:",
                    "Los elementos con los que chocaron los veh\u00edculos con mayor frecuencia.
                     La diferencia con 'Objeto Fijo' es que este an\u00e1lisis incluye tambi\u00e9n
                     colisiones con otros veh\u00edculos y elementos m\u00f3viles.",
                    min_v = 3, max_v = 20, def = 10,
                    status = "danger", h = "400px", w = 12)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 6 · FACTORES  — gráfica 09
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "factores",
          fluidRow(
            box_top("p09_factores",
                    "09 · Factores Contribuyentes en Siniestros Viales",
                    "top_fac", "N\u00famero de factores a mostrar:",
                    "Cada barra representa un factor que estaba presente en los accidentes
                     registrados. Por ejemplo, 'Motocicleta' indica que hab\u00eda una moto
                     involucrada; 'Embriaguez' que se detect\u00f3 alcohol. Un mismo accidente
                     puede tener varios factores simult\u00e1neos.",
                    min_v = 3, max_v = 22, def = 10,
                    status = "danger", h = "500px", w = 12)
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 7 · CORRELACIONES  — 3 matrices
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "correlacion",
          fluidRow(
            box(width = 12, status = "info", solidHeader = TRUE,
                title = "C\u00f3mo leer una matriz de correlaci\u00f3n",
                p("Cada celda muestra el coeficiente de correlaci\u00f3n de Pearson (r) entre
                   dos variables num\u00e9ricas. ",
                  tags$b("r = 1"), " = relaci\u00f3n positiva perfecta (suben juntas). ",
                  tags$b("r = -1"), " = relaci\u00f3n negativa perfecta (una sube, la otra baja). ",
                  tags$b("r = 0"), " = sin relaci\u00f3n lineal. Colores azul intenso = correlaci\u00f3n
                   positiva; rojo intenso = negativa; blanco = sin relaci\u00f3n.",
                  style = "font-size:0.9em; line-height:1.6;"))
          ),
          fluidRow(
            box(width = 4, title = "Siniestros — Variables num\u00e9ricas",
                status = "primary", solidHeader = TRUE,
                p("Relaci\u00f3n entre hora del accidente, mes, a\u00f1o y otras variables
                   num\u00e9ricas del registro de siniestros.",
                  style = "font-style:italic; color:#555; font-size:11px;"),
                plotlyOutput("p_corr_sin", height = "380px")),
            box(width = 4, title = "Actor Vial — Variables num\u00e9ricas",
                status = "primary", solidHeader = TRUE,
                p("Relaci\u00f3n entre la edad del actor vial, el a\u00f1o del siniestro
                   y otras variables num\u00e9ricas del registro de personas involucradas.",
                  style = "font-style:italic; color:#555; font-size:11px;"),
                plotlyOutput("p_corr_act", height = "380px")),
            box(width = 4, title = "Veh\u00edculos — Variables num\u00e9ricas",
                status = "primary", solidHeader = TRUE,
                p("Relaci\u00f3n entre las variables num\u00e9ricas del registro de veh\u00edculos
                   involucrados en siniestros viales.",
                  style = "font-style:italic; color:#555; font-size:11px;"),
                plotlyOutput("p_corr_veh", height = "380px"))
          )
        ),

        # ═══════════════════════════════════════════════════════════
        # TAB 8 · CONCLUSIONES
        # ═══════════════════════════════════════════════════════════
        tabItem(tabName = "conclusion",
          fluidRow(
            box(width = 12, title = "Conclusi\u00f3n General del An\u00e1lisis",
                status = "success", solidHeader = TRUE,
                verbatimTextOutput("conclusion"))
          )
        )
      )
    )
  )

  # ============================================================================
  # SERVER
  # ============================================================================

  server <- function(input, output, session) {

    # ── Reactivos base ────────────────────────────────────────────────────────

    d_sin <- reactive({
      d <- df_sin
      if (input$modo == "Interactivo" && !is.null(input$anio))
        d <- filter(d, as.character(AA_Acc) == input$anio)
      if (isTRUE(input$soloSevero))
        d <- filter(d, Severo == "Severo")
      if (!is.null(input$localidad) && input$localidad != "Todas")
        d <- filter(d, Localidad == input$localidad)
      d
    })

    d_act <- reactive({
      d <- df_act
      if (input$modo == "Interactivo" && !is.null(input$anio))
        d <- filter(d, as.character(AA_Acc) == input$anio)
      d
    })

    d_veh <- reactive({
      d <- df_veh
      if (input$modo == "Interactivo" && !is.null(input$anio))
        d <- filter(d, as.character(AA_Acc) == input$anio)
      d
    })

    d_coords <- reactive({
      d <- coords_all
      if (input$modo == "Interactivo" && !is.null(input$anio))
        d <- d[d$Anio == input$anio, ]
      if (isTRUE(input$soloSevero))
        d <- d[d$Gravedad %in% c("Con Heridos","Con Muertos"), ]
      if (!is.null(input$localidad) && input$localidad != "Todas")
        d <- d[d$Localidad == input$localidad, ]
      d
    })

    # ── KPIs resumen ──────────────────────────────────────────────────────────

    output$kpi_total <- renderValueBox({
      valueBox(comma(nrow(d_sin())), "Total Siniestros",
               icon = icon("car-crash"), color = "blue")
    })
    output$kpi_severos <- renderValueBox({
      valueBox(comma(sum(d_sin()$Severo == "Severo", na.rm = TRUE)),
               "Siniestros Severos (heridos + muertos)",
               icon = icon("exclamation-triangle"), color = "red")
    })
    output$kpi_porcentaje <- renderValueBox({
      pct <- mean(d_sin()$Severo == "Severo", na.rm = TRUE) * 100
      valueBox(paste0(round(pct, 1), "%"), "% Severidad",
               icon = icon("chart-line"), color = "yellow")
    })

    # ── 01 · Gravedad barras ──────────────────────────────────────────────────

    output$p01_grav_barras <- renderPlotly({
      orden <- c("Solo Da\u00f1os","Con Heridos","Con Muertos")
      ct <- d_sin() |>
        count(Gravedad_Indicador_Tradicional) |>
        filter(!is.na(Gravedad_Indicador_Tradicional)) |>
        mutate(Grav  = factor(Gravedad_Indicador_Tradicional, levels = orden),
               color = PAL_GRAV[as.character(Gravedad_Indicador_Tradicional)],
               pct   = n / sum(n))

      p <- ggplot(ct, aes(Grav, n, fill = Grav,
          text = paste0("<b>", Grav, "</b><br>",
                        comma(n), " siniestros<br>",
                        percent(pct, .1), " del total"))) +
        geom_col(color = "white", width = .65, show.legend = FALSE) +
        scale_fill_manual(values = PAL_GRAV) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.18))) +
        labs(x = "Nivel de gravedad del accidente",
             y = "Cantidad de accidentes registrados",
             caption = "Fuente: Anuario de Siniestralidad Vial Bogot\u00e1 2024") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 02 · Gravedad torta ───────────────────────────────────────────────────

    output$p02_grav_torta <- renderPlotly({
      orden <- c("Solo Da\u00f1os","Con Heridos","Con Muertos")
      ct <- d_sin() |>
        count(Gravedad_Indicador_Tradicional) |>
        filter(!is.na(Gravedad_Indicador_Tradicional)) |>
        mutate(Grav  = factor(Gravedad_Indicador_Tradicional, levels = orden),
               color = PAL_GRAV[as.character(Gravedad_Indicador_Tradicional)]) |>
        arrange(Grav)

      plot_ly(ct, labels = ~Grav, values = ~n, type = "pie",
              marker = list(colors = ct$color,
                            line   = list(color = "white", width = 2)),
              textinfo = "label+percent",
              hovertemplate = paste0(
                "<b>%{label}</b><br>",
                "%{value:,} siniestros<br>",
                "Proporci\u00f3n: %{percent}<extra></extra>")) |>
        layout(showlegend = TRUE,
               legend = list(orientation = "h", y = -0.18),
               plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 03 · Evolución anual ──────────────────────────────────────────────────

    output$p03_evolucion <- renderPlotly({
      anual <- d_sin() |>
        count(AA_Acc, name = "N") |>
        arrange(AA_Acc) |>
        mutate(Anio = as.character(AA_Acc))
      prom <- mean(anual$N)

      p <- ggplot(anual, aes(Anio, N, group = 1,
          text = paste0("A\u00f1o: ", Anio, "<br>",
                        comma(N), " siniestros<br>",
                        ifelse(N > prom,
                               paste0("+", comma(round(N - prom)), " sobre el promedio"),
                               paste0(comma(round(prom - N)), " bajo el promedio"))))) +
        geom_col(fill = "#1a6faf", alpha = .85, color = "white", width = .68) +
        geom_line(color = "#c0392b", linewidth = 2) +
        geom_point(color = "#c0392b", size = 4, fill = "white",
                   shape = 21, stroke = 2) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.18))) +
        labs(x = "A\u00f1o del registro",
             y = "Total de accidentes ese a\u00f1o",
             caption = "L\u00ednea roja = tendencia a lo largo del tiempo") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 04 · Promedio mensual ─────────────────────────────────────────────────

    output$p04_mensual <- renderPlotly({
      mensual <- d_sin() |>
        count(AA_Acc, Numero_Mes, name = "cnt") |>
        group_by(Numero_Mes) |>
        summarise(Prom = mean(cnt, na.rm = TRUE), .groups = "drop") |>
        filter(!is.na(Numero_Mes)) |>
        mutate(Mes = factor(MESES_ABREV[Numero_Mes], levels = MESES_ABREV))

      prom_global <- mean(mensual$Prom)
      pico        <- mensual$Mes[which.max(mensual$Prom)]

      p <- ggplot(mensual, aes(Mes, Prom, group = 1,
          text = paste0("Mes: ", Mes, "<br>Promedio: ", round(Prom), " siniestros"))) +
        geom_area(fill = "#1a6faf", alpha = .18) +
        geom_line(color = "#1a6faf", linewidth = 2) +
        geom_point(color = "#1a6faf", size = 4, fill = "white",
                   shape = 21, stroke = 2) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.2))) +
        labs(x = "Mes del a\u00f1o",
             y = "Accidentes t\u00edpicos en ese mes",
             caption = "Promedio calculado sobre todos los a\u00f1os registrados") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 05 · Tipo objeto fijo (top-N) ─────────────────────────────────────────

    output$p05_obj_fijo <- renderPlotly({
      top_n <- input$top_obj
      ct <- d_sin() |>
        count(Tipo_Objeto_Fijo, name = "N") |>
        filter(!is.na(Tipo_Objeto_Fijo)) |>
        slice_max(N, n = top_n) |>
        mutate(Tipo_Objeto_Fijo = reorder(Tipo_Objeto_Fijo, N),
               pct = N / sum(d_sin()$Tipo_Objeto_Fijo %in% Tipo_Objeto_Fijo,
                             na.rm = TRUE))

      p <- ggplot(ct, aes(N, Tipo_Objeto_Fijo,
          text = paste0("<b>", Tipo_Objeto_Fijo, "</b><br>",
                        comma(N), " siniestros"))) +
        geom_col(fill = "#1a6faf", color = "white") +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.20))) +
        labs(x = "Cantidad de accidentes donde aparece este objeto",
             y = "Tipo de objeto en la v\u00eda") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 06 · Distribución horaria (con franjas de día) ────────────────────────

    output$p06_horaria <- renderPlotly({
      horario <- d_sin() |>
        filter(!is.na(Hora_Acc)) |>
        count(Hora_Acc, name = "N") |>
        right_join(tibble(Hora_Acc = 0:23), by = "Hora_Acc") |>
        mutate(N = coalesce(N, 0L),
               Franja = case_when(
                 Hora_Acc <= 5  ~ "Madrugada\n(0-5h)",
                 Hora_Acc <= 11 ~ "Ma\u00f1ana\n(6-11h)",
                 Hora_Acc <= 17 ~ "Tarde\n(12-17h)",
                 TRUE           ~ "Noche\n(18-23h)"
               ))

      cols24 <- colorRampPalette(
        c("#2980b9","#1a6faf","#e07b00","#c0392b","#922b21",
          "#c0392b","#e07b00","#1a6faf"))(24)

      p <- ggplot(horario, aes(Hora_Acc, N, fill = factor(Hora_Acc),
          text = paste0(sprintf("%02d", Hora_Acc), ":00h — ",
                        comma(N), " siniestros\nFranja: ", Franja))) +
        # Franjas de fondo por período del día
        annotate("rect", xmin=-0.5, xmax=5.5,  ymin=0, ymax=Inf,
                 fill="#3498db", alpha=.06) +
        annotate("rect", xmin=5.5,  xmax=11.5, ymin=0, ymax=Inf,
                 fill="#2ecc71", alpha=.06) +
        annotate("rect", xmin=11.5, xmax=17.5, ymin=0, ymax=Inf,
                 fill="#e67e22", alpha=.06) +
        annotate("rect", xmin=17.5, xmax=23.5, ymin=0, ymax=Inf,
                 fill="#8e44ad", alpha=.06) +
        annotate("text", x=2.5,  y=max(horario$N)*.97,
                 label="Madrugada", color="#2980b9", size=3, fontface="bold") +
        annotate("text", x=8.5,  y=max(horario$N)*.97,
                 label="Ma\u00f1ana",   color="#27ae60", size=3, fontface="bold") +
        annotate("text", x=14.5, y=max(horario$N)*.97,
                 label="Tarde",     color="#d35400", size=3, fontface="bold") +
        annotate("text", x=20.5, y=max(horario$N)*.97,
                 label="Noche",     color="#7d3c98", size=3, fontface="bold") +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_manual(values = cols24) +
        scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.14))) +
        labs(x = "Hora del d\u00eda en que ocurri\u00f3 el accidente",
             y = "Accidentes registrados a esa hora") +
        tema() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 07 · Top localidades (top-N) ──────────────────────────────────────────

    output$p07_localidades <- renderPlotly({
      top_n <- input$top_loc
      top <- d_sin() |>
        count(Localidad, name = "N") |>
        filter(!is.na(Localidad)) |>
        slice_max(N, n = top_n) |>
        arrange(N) |>
        mutate(Localidad = factor(Localidad, levels = Localidad),
               pct       = N / sum(N))

      p <- ggplot(top, aes(N, Localidad, fill = N,
          text = paste0("<b>", Localidad, "</b><br>",
                        comma(N), " siniestros<br>",
                        percent(pct, .1), " del grupo mostrado"))) +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "#aed6f1", high = "#1a4f8a") +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.20))) +
        labs(x = "Total de accidentes en esa localidad",
             y = "Localidad de Bogot\u00e1",
             caption = "Color m\u00e1s oscuro = mayor concentraci\u00f3n de accidentes") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 08 · Heatmap día × hora ───────────────────────────────────────────────

    output$p08_heatmap <- renderPlotly({
      hm <- d_sin() |>
        filter(!is.na(Dia_Semana_Acc), !is.na(Hora_Acc)) |>
        count(Dia_Semana_Acc, Hora_Acc, name = "N")

      dias_ord <- rev(DIAS_SEMANA_ORDER)
      mat <- matrix(0, nrow = length(dias_ord), ncol = 24,
                    dimnames = list(dias_ord, as.character(0:23)))
      for (i in seq_len(nrow(hm))) {
        dia <- as.character(hm$Dia_Semana_Acc[i])
        hr  <- as.character(hm$Hora_Acc[i])
        if (dia %in% dias_ord && hr %in% colnames(mat))
          mat[dia, hr] <- hm$N[i]
      }

      plot_ly(z = mat,
              x = sprintf("%02d:00", 0:23),
              y = tools::toTitleCase(dias_ord),
              type = "heatmap",
              colorscale = list(list(0,"#fff5f0"), list(.4,"#fc8d59"),
                                list(.7,"#d73027"), list(1,"#8b0000")),
              hovertemplate = paste0(
                "<b>%{y}</b> a las <b>%{x}</b><br>",
                "Siniestros registrados: <b>%{z:,}</b><br>",
                "Color m\u00e1s rojo = mayor concentraci\u00f3n<extra></extra>")) |>
        layout(
          xaxis  = list(title = "Hora del d\u00eda en que ocurri\u00f3 el accidente",
                        tickangle = -45),
          yaxis  = list(title = "D\u00eda de la semana"),
          coloraxis = list(colorbar = list(title = "N\u00famero de\naccidentes")),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    })

    # ── 09 · Top factores (top-N) ─────────────────────────────────────────────

    output$p09_factores <- renderPlotly({
      top_n   <- input$top_fac
      con_cols <- grep("^Con_", names(d_sin()), value = TRUE)
      if (length(con_cols) == 0) return(NULL)

      factores <- sapply(con_cols, function(col)
        sum(d_sin()[[col]] == "SI", na.rm = TRUE))

      top <- tibble(Factor = CON_LABELS[names(factores)], N = factores) |>
        filter(!is.na(Factor), N > 0) |>
        slice_max(N, n = top_n) |>
        arrange(N) |>
        mutate(Factor = factor(Factor, levels = Factor),
               pct    = N / nrow(d_sin()))

      p <- ggplot(top, aes(N, Factor, fill = N,
          text = paste0("<b>", Factor, "</b><br>",
                        comma(N), " siniestros con este factor<br>",
                        "Presente en el ", percent(pct, .1), " de los accidentes"))) +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "#f4a6a0", high = "#8b0000") +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.22))) +
        labs(x = "Accidentes donde este factor estuvo presente",
             y = "Factor de riesgo identificado",
             caption = "Un mismo accidente puede tener varios factores simult\u00e1neos") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 10 · Top elementos choque (top-N) ─────────────────────────────────────

    output$p10_choque <- renderPlotly({
      top_n <- input$top_choque
      top <- d_sin() |>
        count(Tipo_Objeto_Fijo, name = "N") |>
        filter(!is.na(Tipo_Objeto_Fijo)) |>
        slice_max(N, n = top_n) |>
        arrange(N) |>
        mutate(Tipo_Objeto_Fijo = factor(Tipo_Objeto_Fijo, levels = Tipo_Objeto_Fijo))

      p <- ggplot(top, aes(N, Tipo_Objeto_Fijo, fill = N,
          text = paste0("<b>", Tipo_Objeto_Fijo, "</b><br>",
                        comma(N), " colisiones registradas"))) +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "#a8d5a2", high = "#1a5c1a") +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.22))) +
        labs(x = "Cantidad de colisiones registradas con este elemento",
             y = "Elemento con el que choc\u00f3 el veh\u00edculo") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 11 · Distribución por sexo ────────────────────────────────────────────

    output$p11_sexo <- renderPlotly({
      ct <- d_act() |>
        count(Sexo, name = "N") |>
        filter(!is.na(Sexo)) |>
        arrange(desc(N)) |>
        mutate(pct   = N / sum(N),
               color = PAL_SEXO[Sexo],
               color = ifelse(is.na(color), "#7f8c8d", color))

      p <- ggplot(ct, aes(Sexo, N, fill = Sexo,
          text = paste0("<b>", Sexo, "</b><br>",
                        comma(N), " personas<br>",
                        percent(pct, .1), " del total"))) +
        geom_col(color = "white", width = .6, show.legend = FALSE) +
        geom_text(aes(label = percent(pct, .1)),
                  vjust = -0.5, size = 4, fontface = "bold", color = "#1a1a2e") +
        scale_fill_manual(values = PAL_SEXO) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.2))) +
        labs(x = "Sexo de la persona involucrada",
             y = "Personas involucradas en accidentes",
             caption = "Incluye conductores, pasajeros y peatones") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 12a · Grupos de edad barras ───────────────────────────────────────────

    output$p12a_edad_barras <- renderPlotly({
      ct <- d_act() |>
        filter(!is.na(Grupo_Edad)) |>
        count(Grupo_Edad, name = "N") |>
        mutate(pct = N / sum(N))
      cols_e <- c("#1a6faf","#2196a6","#2e8b57","#e07b00","#c0392b","#8b0000")

      p <- ggplot(ct, aes(Grupo_Edad, N, fill = Grupo_Edad,
          text = paste0("<b>", Grupo_Edad, "</b><br>",
                        comma(N), " personas<br>",
                        percent(pct, .1), " del total"))) +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_manual(values = cols_e[seq_len(nrow(ct))]) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.18))) +
        labs(x = "Rango de edad de la persona",
             y = "Personas involucradas en accidentes") +
        tema() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 12b · Boxplot edad ────────────────────────────────────────────────────

    output$p12b_edad_box <- renderPlotly({
      orden_grav_act <- c("ILESO", "HERIDO", "MUERTO")
      etiq_grav_act  <- c(ILESO  = "Sin lesiones (Ileso)",
                           HERIDO = "Con lesiones (Herido)",
                           MUERTO = "Fallecido (Muerto)")
      cols_grav_act  <- c(ILESO  = "#2196a6",
                           HERIDO = "#e07b00",
                           MUERTO = "#c0392b")

      df_box <- d_act() |>
        filter(!is.na(Edad_num), Edad_num >= 0, Edad_num <= 110,
               Gravedad_Indicador_Tradicional %in% orden_grav_act)

      resumen <- df_box |>
        group_by(Gravedad_Indicador_Tradicional) |>
        summarise(
          Med   = median(Edad_num),
          Media = round(mean(Edad_num), 1),
          Q1    = quantile(Edad_num, .25),
          Q3    = quantile(Edad_num, .75),
          N     = n(),
          .groups = "drop"
        )

      p <- plot_ly()
      for (g in orden_grav_act) {
        col  <- cols_grav_act[g]
        etiq <- etiq_grav_act[g]
        vals <- df_box$Edad_num[df_box$Gravedad_Indicador_Tradicional == g]
        inf  <- resumen[resumen$Gravedad_Indicador_Tradicional == g, ]

        ht <- if (nrow(inf) > 0)
          paste0("<b>", etiq, "</b><br>",
                 "Edad t\u00edpica (mediana): <b>", inf$Med, " a\u00f1os</b><br>",
                 "Promedio: ", inf$Media, " a\u00f1os<br>",
                 "50\u0025 central: ", inf$Q1, "\u2013", inf$Q3, " a\u00f1os<br>",
                 "Personas: ", comma(inf$N),
                 "<extra></extra>")
        else "<extra></extra>"

        p <- add_trace(p,
          type = "box", y = vals, name = etiq,
          boxpoints = "suspectedoutliers", jitter = 0.35,
          marker    = list(color = col, size = 4, opacity = 0.45),
          line      = list(color = col, width = 2),
          fillcolor = paste0(col, "33"),
          boxmean   = TRUE,
          hoverinfo = "none",
          hovertemplate = ht
        )
      }
      p |> layout(
        xaxis = list(title = "Gravedad de la lesi\u00f3n de la persona",
                     tickfont = list(size = 13)),
        yaxis = list(title = "Edad de la persona (a\u00f1os)",
                     gridcolor = "#eeeeee", zeroline = FALSE),
        showlegend = FALSE, boxmode = "group",
        annotations = list(list(
          text = "Rombo = promedio \u00b7 L\u00ednea = mediana \u00b7 Caja = 50\u0025 central de edades \u00b7 Puntos = edades extremas",
          showarrow = FALSE, x = 0.5, y = -0.13, xref = "paper", yref = "paper",
          font = list(size = 10, color = "#888888")
        )),
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
    })

    # ── 13 · Tipos de vehículos (top-N) ──────────────────────────────────────

    output$p13_tipos_veh <- renderPlotly({
      top_n <- input$top_veh
      top <- d_veh() |>
        count(Clase, name = "N") |>
        filter(!is.na(Clase)) |>
        slice_max(N, n = top_n) |>
        arrange(N) |>
        mutate(Clase = factor(Clase, levels = Clase),
               pct   = N / sum(d_veh()$Clase %in% Clase, na.rm = TRUE))

      p <- ggplot(top, aes(N, Clase, fill = N,
          text = paste0("<b>", Clase, "</b><br>",
                        comma(N), " veh\u00edculos involucrados"))) +
        geom_col(color = "white", show.legend = FALSE) +
        scale_fill_gradient(low = "#c9a9e0", high = "#4a0080") +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.22))) +
        labs(x = "Veces que este tipo de veh\u00edculo aparece en accidentes",
             y = "Tipo de veh\u00edculo") +
        tema()
      ggplotly(p, tooltip = "text") |>
        layout(plot_bgcolor = "white", paper_bgcolor = "white")
    })

    # ── 14 · Violín hora × gravedad ──────────────────────────────────────────

    output$p14_violin <- renderPlotly({
      orden_grav <- c("Solo Da\u00f1os","Con Heridos","Con Muertos")
      df_plot <- d_sin() |>
        filter(!is.na(Hora_Acc),
               Gravedad_Indicador_Tradicional %in% orden_grav)

      resumen <- df_plot |>
        group_by(Gravedad_Indicador_Tradicional) |>
        summarise(Med = median(Hora_Acc), Q1 = quantile(Hora_Acc,.25),
                  Q3  = quantile(Hora_Acc,.75), N  = n(), .groups = "drop")

      p <- plot_ly()
      for (g in orden_grav) {
        col  <- PAL_GRAV[g]
        vals <- df_plot$Hora_Acc[df_plot$Gravedad_Indicador_Tradicional == g]
        inf  <- resumen[resumen$Gravedad_Indicador_Tradicional == g,]
        ht <- if (nrow(inf) > 0)
          paste0("<b>", g, "</b><br>",
                 "Hora m\u00e1s com\u00fan (mediana): ", sprintf("%02d:00h", as.integer(inf$Med)), "<br>",
                 "Rango central (Q1\u2013Q3): ", sprintf("%02d:00\u2013%02d:00h",
                                                          as.integer(inf$Q1), as.integer(inf$Q3)), "<br>",
                 "Total de siniestros: ", comma(inf$N),
                 "<extra></extra>") else "<extra></extra>"

        p <- add_trace(p, type = "violin", y = vals, name = g,
                       spanmode  = "hard",
                       line      = list(color = col, width = 1.5),
                       fillcolor = paste0(col, "55"),
                       box       = list(visible = TRUE, width = 0.12,
                                        fillcolor = "white",
                                        line = list(color = col, width = 2)),
                       meanline  = list(visible = FALSE),
                       points    = FALSE, hoverinfo = "none",
                       hovertemplate = ht)
      }
      p |> layout(
        xaxis = list(title = "Gravedad del accidente", tickfont = list(size = 13)),
        yaxis = list(title = "Hora del d\u00eda (¿a qu\u00e9 hora pas\u00f3?)",
                     range    = c(-0.5, 23.5),
                     tickvals = seq(0, 23, 3),
                     ticktext = sprintf("%02d:00h", seq(0, 23, 3)),
                     gridcolor = "#eeeeee", zeroline = FALSE),
        showlegend = FALSE, violinmode = "group",
        annotations = list(list(
          text = "Ancho del viol\u00edn = cu\u00e1ntos accidentes hubo a esa hora | Caja = rango central | Forma sim\u00e9trica = accidentes distribuidos a lo largo del d\u00eda",
          showarrow = FALSE, x = 0.5, y = -0.12, xref = "paper", yref = "paper",
          font = list(size = 10, color = "#888888")
        )),
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
    })

    # ── Correlaciones ─────────────────────────────────────────────────────────

    corr_plotly <- function(df_data, label) {
      num_cols <- names(df_data)[sapply(df_data, is.numeric)]
      if (length(num_cols) < 2)
        return(plot_ly() |> layout(
          title = list(text = paste("Sin suficientes variables —", label))))

      mat <- round(cor(df_data[num_cols], use = "pairwise.complete.obs"), 2)

      plot_ly(z = mat, x = colnames(mat), y = rownames(mat),
              type = "heatmap",
              colorscale = list(list(0,"#c0392b"), list(.5,"white"), list(1,"#1a6faf")),
              zmin = -1, zmax = 1,
              hovertemplate = paste0(
                "<b>%{x}</b> con <b>%{y}</b><br>",
                "r = %{z:.2f}<br>",
                "Interpretaci\u00f3n: cercano a 1 o -1 = relaci\u00f3n fuerte",
                "<extra></extra>")) |>
        add_annotations(
          x = rep(colnames(mat), each = nrow(mat)),
          y = rep(rownames(mat), ncol(mat)),
          text = as.vector(mat),
          showarrow = FALSE,
          font = list(size = 10, color = "black")
        ) |>
        layout(xaxis = list(tickangle = -35),
               plot_bgcolor = "white", paper_bgcolor = "white")
    }

    output$p_corr_sin <- renderPlotly({ corr_plotly(d_sin(), "Siniestros") })
    output$p_corr_act <- renderPlotly({ corr_plotly(d_act(), "Actor Vial") })
    output$p_corr_veh <- renderPlotly({ corr_plotly(d_veh(), "Veh\u00edculos") })

    # ── KPIs mapa ─────────────────────────────────────────────────────────────

    output$kpi_mapa_total <- renderValueBox({
      valueBox(comma(nrow(d_coords())), "Puntos georeferenciados",
               icon = icon("map-marker-alt"), color = "blue")
    })
    output$kpi_mapa_heridos <- renderValueBox({
      valueBox(comma(sum(d_coords()$Gravedad == "Con Heridos", na.rm = TRUE)),
               "Con Heridos (en mapa)", icon = icon("user-injured"), color = "orange")
    })
    output$kpi_mapa_muertos <- renderValueBox({
      valueBox(comma(sum(d_coords()$Gravedad == "Con Muertos", na.rm = TRUE)),
               "Con Muertos (en mapa)", icon = icon("skull-crossbones"), color = "red")
    })

    # ── Mapa ──────────────────────────────────────────────────────────────

    output$mapa_pro <- renderLeaflet({
      d <- d_coords()

      pal_l <- colorFactor(
        palette = c("#3498db","#e07b00","#c0392b"),
        levels  = c("Solo Da\u00f1os","Con Heridos","Con Muertos"),
        na.color = "#95a5a6"
      )

      mapa <- leaflet(d) |>
        addProviderTiles(providers$CartoDB.Positron,   group = "Claro") |>
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") |>
        setView(lng = -74.0817, lat = 4.7110, zoom = 11)

      if (nrow(d) > 0) {
        mapa <- mapa |>
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            color       = ~pal_l(Gravedad),
            fillColor   = ~pal_l(Gravedad),
            radius = 5, fillOpacity = 0.72, stroke = TRUE, weight = 1,
            clusterOptions = markerClusterOptions(
              iconCreateFunction = JS("
                function(cluster){
                  var n=cluster.getChildCount();
                  var c=n>500?'#c0392b':n>100?'#e07b00':'#3498db';
                  return new L.DivIcon({
                    html:'<div style=\"background:'+c+';color:white;border-radius:50%;'+
                    'width:38px;height:38px;display:flex;align-items:center;'+
                    'justify-content:center;font-weight:bold;font-size:13px;\">'+n+'</div>',
                    className:'',iconSize:[38,38]});}")
            ),
            popup = ~paste0(
              "<b>Gravedad:</b> ", Gravedad, "<br>",
              "<b>Localidad:</b> ", Localidad, "<br>",
              "<i>Haz clic en el mapa para acercar</i>"
            ),
            group = "Cluster"
          ) |>
          addHeatmap(lng = ~lon, lat = ~lat,
                     layerId  = "heatmap_main",
                     blur = 20, max = 0.05, radius = 15,
                     gradient = c("0.0"="#3498db","0.4"="#f39c12",
                                  "0.7"="#e74c3c","1.0"="#8b0000"),
                     group = "Mapa de Calor") |>
          addLegend("bottomright", pal = pal_l, values = ~Gravedad,
                    title = "Gravedad del siniestro", opacity = 0.85)
      }

      mapa |> addLayersControl(
        baseGroups    = c("Claro","Oscuro"),
        overlayGroups = c("Cluster","Mapa de Calor"),
        options       = layersControlOptions(collapsed = FALSE)
      )
    })

    # ── Conclusión ────────────────────────────────────────────────────────────

    output$conclusion <- renderText({
      paste(
        "AN\u00c1LISIS DE SINIESTRALIDAD VIAL \u2014 BOGOT\u00c1 2024",
        strrep("-", 54), "",
        "1) CONCENTRACI\u00d3N TERRITORIAL",
        "   Kennedy, Suba y Engativ\u00e1 concentran el mayor n\u00famero",
        "   de siniestros. Requieren intervenciones prioritarias",
        "   en infraestructura vial y control de tr\u00e1nsito.",
        "",
        "2) PATRONES TEMPORALES CR\u00cdTICOS",
        "   Picos en horas de desplazamiento (07-09h y 17-19h).",
        "   Viernes y fines de semana con mayor proporci\u00f3n de casos",
        "   severos, posiblemente asociados a consumo de alcohol.",
        "",
        "3) COMPOSICI\u00d3N POR GRAVEDAD",
        "   La mayor\u00eda de siniestros registran 'Solo Da\u00f1os', pero",
        "   los casos con heridos y muertos exigen atenci\u00f3n urgente",
        "   especialmente en v\u00edas de alta velocidad.",
        "",
        "4) ACTORES M\u00c1S VULNERABLES",
        "   Motociclistas y peatones son los m\u00e1s afectados.",
        "   J\u00f3venes (18-25 a\u00f1os) representan el grupo etario",
        "   m\u00e1s frecuente en los registros de siniestralidad.",
        "",
        "5) FACTORES CONTRIBUYENTES PRINCIPALES",
        "   Embriaguez y exceso de velocidad son los factores",
        "   m\u00e1s presentes en los siniestros con mayor gravedad.",
        "",
        "6) RECOMENDACIONES",
        "   \u2022 Controles de alcoholemia en fin de semana.",
        "   \u2022 Campa\u00f1as de seguridad dirigidas a motociclistas.",
        "   \u2022 Mejor se\u00f1alizaci\u00f3n en zonas de alta concentraci\u00f3n.",
        "   \u2022 C\u00e1maras de velocidad en v\u00edas principales.",
        "   \u2022 Programas de educaci\u00f3n vial para j\u00f3venes.",
        sep = "\n"
      )
    })
  }

  # --------------------------------------------------------------------------
  app <- shinyApp(ui = ui, server = server)
  message("[dashboard] Abriendo en http://127.0.0.1:3838")
  shiny::runApp(app, host = "127.0.0.1", port = 3838,
                launch.browser = launch_browser)
}
