# Tarea 1
rm(list=ls())
getwd()

# Cargar librería para lectura rápida
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(sf)
library(geosphere)
library(ggplot2)
library(maps)

####################### PARTE 1 ############################

# Cargamos los datos
# Relativo a la carpeta que eligamos
# setwd("~/Documents/ITAM/DatosEspaciales/repositorio/tareas_espaciales/tarea1")
#data <- read_csv("data/taquerias_cdmx.csv")
data <- read_csv("taquerias_cdmx.csv")


# Creamos una función para calcular las distancia de Haversine de todos los puntos
# Utilizamos vectorización aquí para eficientar el cómputo - Esta función nos sirve mucho en un ejercicio NxN
obtener_distancias_haversine <- function(datos=data) {
  # Total de filas
  n <- nrow(datos)
  
  # Extraemos las latitudes y longitudes del dataframe
  lats <- datos$latitude
  lons <- datos$longitude
  
  # Vamos a formar dos matrices, la primera, será p1 y la segunda p2 (de acuerdo a las ayudas distHaversine)
  # p1 contendra los "origenes" y p2 contendrá los "destinos"
  
  lat_origen <- rep(lats, each = n) # Es decir, los primeros n elementos contienen la latitud  de la primera observación y así sucesivamente
  lon_origen <- rep(lons, each = n) # Es decir, los primeros n elementos contienen la longitud de la primera observación y así sucesivamente
  
  lat_destino <- rep(lats, times = n) # Es decir, se repiten n veces las latitudes
  lon_destino <- rep(lons, times = n) # Es decir, se repiten n veces las longitudes
  
  p1 <- cbind(lon_origen, lat_origen) # Formamos la matriz p1
  p2 <- cbind(lon_destino, lat_destino) # Formamos la matriz p2
  
  distancias_vector <- distHaversine(p1, p2, r=6378137) # Calculamos la distancia de Haversine de 
  
  # Convertimos a una matriz de n x n donde cada (i,j) corresponde a la distancia entre el comercio i y el comercio j
  matriz_dist <- matrix(distancias_vector, nrow = n, ncol = n, byrow = TRUE)
  
  # Retornamos (odio R)
  return(matriz_dist)
}

# Obtenemos las distancias de Haversine para cada taquería 
haversine_distancias <- obtener_distancias_haversine()

# Para cada comercio, obtenemos el número de taquerías que se encuentran a una distancia igual o menor a 1500 metros
# Creamos una columna con los conteos
conteos <- rowSums(haversine_distancias > 0 & haversine_distancias <= 1500) # Los ceros son autodistancias
data$total_competencia <- conteos

patron <- "(?i)cuauh"
taquerias_cuauh <- data[grepl(patron, data$locality, perl = TRUE) | 
                          grepl(patron, data$region, perl = TRUE), ]

# Creamos un DataFrame con el top 10 de taquerías
top10 <- taquerias_cuauh |>
  slice_max(n = 10, order_by = total_competencia, with_ties = TRUE)

# Guardamos el DataFrame
write_csv(top10, "top_10_taquerias.csv")




####################### PARTE 2 ############################


# Se carga la base de centroides y la base de places de méxico

centroides <- read_csv("centroides_mexico.csv") |>
              rename(latitude = lat, longitude = lon) |>    # Cambiamos el nombre de las columnas para asegurar su correcta aplicacion el la distancia
              select(Entidad_Federativa, longitude, latitude)

# Dado que el dataframe por si solo es pesado y sólo nos interesan ciertas columnas, con esas nos quedamos para optimizar el proceso
data_negocios <- read_csv("negocios_mexico.csv")
data_negocios <- data_negocios |>
                 select(name,latitude,longitude,region)

# Creamos una matriz con la longitud y latidud
M_negocios <- as.matrix(data_negocios[, c("longitude", "latitude")])

# Definiimos el radio en el cuál se aplicara el filtro
radio_m <- 25000

# Iteramos sobre los centroides y contamos negocios a <= 25 km, en este caso es mejor aplicarlo directamente
# ya que no tenemos una matriz NxN
conteos <- centroides |>
  rowwise() |>
  mutate(
    total_places = {
      centroide <- c(longitude, latitude)
      distancias <- distHaversine(centroide, M_negocios, r=6378137)
      sum(distancias <= radio_m, na.rm = TRUE)
    }
  ) |>
  ungroup()

# 6. Armamos el layout y ordenamos por el que tiene más lugares/negocios
places_centroide_estado <- conteos |>
  select(
    `Entidad Federativa` = Entidad_Federativa,
    latitud_centroide = latitude,
    longitud_centroide = longitude,
    total_places
  )
places_centroide_estado <- places_centroide_estado |>
  arrange(desc(total_places))

write_csv(places_centroide_estado, "places_centroide_estado.csv")

##### EXTRA : Un mapa para visualizar donde se tienen más negocios cerca del centroide por entidad ####

mexico_map <- map_data("world", region = "Mexico")
ggplot() +
  geom_polygon(
    data = mexico_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95", color = "gray70"
  ) +
  geom_point(
    data = places_centroide_estado,
    aes(
      x = longitud_centroide,
      y = latitud_centroide,
      size = total_places,
      color = total_places
    ),
    alpha = 0.7
  ) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Número de negocios en un radio de 25 km del centroide estatal",
    subtitle = "Archivos: centroides_mexico.csv + negocios_mexico.csv",
    x = "Longitud",
    y = "Latitud",
    size = "Total places",
    color = "Total places"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )
