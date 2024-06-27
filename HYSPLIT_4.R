# Instalar y cargar las bibliotecas necesarias
if (!requireNamespace("splitr", quietly = TRUE)) install.packages("splitr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")
if (!requireNamespace("webshot", quietly = TRUE)) install.packages("webshot")

# Cargar las bibliotecas necesarias
library(splitr)
library(lubridate)
library(leaflet)
library(dplyr)
library(mapview)
library(webshot)

# Instalar PhantomJS, que es necesario para capturar capturas de pantalla
webshot::install_phantomjs()

# Evitar que se agote el tiempo de espera
getOption('timeout')
options(timeout = 10000)

# Construir el modelo de trayectoria
trajectory_model <- hysplit_trajectory(
  lat = -34.58389,  # Latitud del punto de inicio
  lon = -71.74972,  # Longitud del punto de inicio
  height = 500,     # Altura inicial en metros
  duration = 24,    # Duración de la trayectoria en horas
  met_type = "gdas1",  # Tipo de datos meteorológicos a usar
  direction = "forward",  # Dirección de la trayectoria (adelante en el tiempo)
  days = seq(        # Secuencia de días para las simulaciones
    lubridate::ymd("2017-01-18"),  # Fecha de inicio
    lubridate::ymd("2017-01-20"),  # Fecha de finalización
    by = "1 day"    # Incremento de un día
  ),
  daily_hours = c(8, 16, 24)  # Horas del día para iniciar las simulaciones
)

# Eliminar los valores faltantes (NAs)
trajectory_model_complete <- trajectory_model[complete.cases(trajectory_model), ]

# Crear una nueva función para trazar las trayectorias
trajectory_plot_new <- function(x, show_hourly = TRUE, color_scheme = "cycle_hues") {
  
  if (inherits(x, "trajectory_model")) {
    if (!is.null(x$traj_df)) {
      traj_df <- x$traj_df  # Obtener el dataframe de trayectorias
    } else {
      stop("No hay datos disponibles para trazar.")
    }
  }
  
  if (inherits(x, "data.frame")) {
    if (all(c("run", "receptor", "hour_along", "traj_dt", "lat", "lon", "height", "traj_dt_i") %in% colnames(x))) {
      traj_df <- x  # Obtener el dataframe de trayectorias
    } else {
      stop("Esta tibble no contiene datos trazables de trayectoria.")
    }
  }
  
  # Corregir valores de longitud cerca del meridiano de Greenwich
  traj_df$lon[which(traj_df$lon > 0)] <- traj_df$lon[which(traj_df$lon > 0)] - (180*2)
  
  receptors <- traj_df %>% dplyr::pull(receptor) %>% unique()  # Obtener los receptores únicos
  dates <- traj_df %>% dplyr::pull(traj_dt_i) %>% unique()  # Obtener las fechas únicas
  
  # Crear el objeto de mapa
  traj_plot <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(provider = "CartoDB.DarkMatter", group = "CartoDB Dark Matter") %>%
    leaflet::addProviderTiles(provider = "CartoDB.Positron", group = "CartoDB Positron") %>%
    leaflet::addProviderTiles(provider = "Esri.WorldTerrain", group = "ESRI World Terrain") %>%
    leaflet::fitBounds(  # Ajustar los límites del mapa a las coordenadas de la trayectoria
      lng1 = min(traj_df[["lon"]]),
      lat1 = min(traj_df[["lat"]]),
      lng2 = max(traj_df[["lon"]]),
      lat2 = max(traj_df[["lat"]])
    ) %>%
    leaflet::addLayersControl(  # Añadir controles de capas al mapa
      baseGroups = c("CartoDB Positron", "CartoDB Dark Matter", "ESRI World Terrain"),
      overlayGroups = c("trajectory_points", "trajectory_paths"),
      position = "topright"
    )
  
  # Obtener diferentes trayectorias por receptor y por fecha
  for (i in seq_along(receptors)) {
    receptor_i <- receptors[i]
    
    for (j in seq_along(dates)) {
      date_i <- dates[j]
      
      wind_traj_ij <- traj_df %>%
        dplyr::filter(receptor == receptor_i, traj_dt_i == date_i)  # Filtrar datos por receptor y fecha
      
      # Crear un popup para mostrar información de la trayectoria
      popup <- paste0(
        "<strong>trayectoria</strong> ", wind_traj_ij[["traj_dt_i"]],
        "<br><strong>en tiempo</strong> ", wind_traj_ij[["traj_dt"]],
        " (", wind_traj_ij[["hour_along"]],
        " h)<br><strong>altura</strong> ", wind_traj_ij[["height"]],
        " <font size=\"1\">m AGL</font> / ",
        "<strong>P</strong> ", wind_traj_ij[["pressure"]],
        " <font size=\"1\">hPa</font>"
      )
      
      # Añadir líneas de trayectoria al mapa
      traj_plot <- traj_plot %>%
        leaflet::addPolylines(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_paths",
          weight = 2,
          smoothFactor = 1,
          color = "black"  # Color negro para las líneas
        ) %>%
        # Añadir círculos de trayectoria al mapa
        leaflet::addCircles(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_points",
          radius = 250,
          fill = TRUE,
          color = "black",  # Color negro para los círculos
          fillColor = "black",
          popup = popup
        )
    }
  }
  
  traj_plot  # Devolver el objeto de mapa
}

# Crear el gráfico de trayectorias
map <- trajectory_plot_new(trajectory_model_complete)

# Guardar el gráfico en la carpeta especificada
output_path <- "C:/Users/Usuario/Desktop/mapa_incendios/trajectory_map.png"
mapshot(map, file = output_path, vwidth = 1920, vheight = 1080)  # Guardar el mapa como imagen PNG
