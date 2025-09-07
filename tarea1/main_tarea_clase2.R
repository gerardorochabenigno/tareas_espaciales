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

# Cargamos los datos
data <- read_csv("data/taquerias_cdmx.csv")


# Creamos una función para calcular las distancia de Haversine de todos los puntos
# Utilizamos vectorización aquí para eficientar el cómputo
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

# Nos quedamos con las taquerías que se encuentran en CUAUHTEMOC (AGREGAR CÓDIGO)
taquerias_cuauh <- data |>
  arrange(desc(total_competencia))

# Creamos un DataFrame con el top 10 de taquerías
top10 <- taquerias_cuauh |>
  slice_head(n=10)

# Guardamos el DataFrame
write_csv(top10, "data/top_10_taquerias.csv")

