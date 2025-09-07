# Tarea 1
rm(list=ls())
getwd()

# Cargar librería para lectura rápida
library(readr)
library(dplyr)
library(stringr)
library(stringi)

#setwd("./tarea/tarea1")
setwd("~/Documents/ITAM/DatosEspaciales/repositorio/tareas_espaciales/tarea1")


# Listar todos los archivos CSV en la carpeta "data"
archivos <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
archivos

# Leer y combinar todos los CSV en un solo data frame
datos <- do.call(rbind, lapply(archivos, read_csv))

# Ver primeras filas
head(datos)


# --- (1) función de normalización (quita tildes, pone minúsculas, limpia espacios/puntuación) ---
normalize_txt <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%        # espacios repetidos
    str_trim() %>%
    str_to_lower(locale = "es") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%  # quita tildes
    str_replace_all("[^a-z0-9]+", " ") %>%  # deja solo letras/números como palabras
    str_squish()
}


# tablas de frecuencia (ordenadas) para inspección
region_freq <- datos %>%
  mutate(region_norm = normalize_txt(region)) %>%
  count(region_norm, sort = TRUE)

locality_freq <- datos %>%
  mutate(locality_norm = normalize_txt(locality)) %>%
  count(locality_norm, sort = TRUE)

print(head(region_freq, 1000)  ,n=1000)
print(head(locality_freq, 1000), n=1000)


# --- (2) diccionario de homologación -> "Ciudad de México" ---

variantes_cdmx <- c(
  "distrito federal", "cdmx", "df", "ciudad de mexico", "d f", 
  "mexico city", "mexico d f", "mexico df", "iztapalapa", "tlalpan", 
  "coyoacan", "gustavo a madero", "benito juarez", "iztacalco", 
  "alvaro obregon", "azcapotzalco", "federal district", "miguel hidalgo", 
  "tlahuac", "venustiano carranza", "cd mexico", "cd mx", "cuajimalpa", 
  "distrito federa", "cd de mexico", "cuidad de mexico", "distrito federak", 
  "cdmxp", "cdnx", "citta del messico", "ciuda de mexico", "ciudad mexico", 
  "ciudadad mexico", "ciudas de mexico", "cm", "cm mexico", "col del valle", 
  "colonia del valle centro", "colonia doctores", "colonia obrera", "colonia roma", 
  "cuajimalpa de morelos", "cuauhtemoc df", "del alvaro obregon", "del valle", 
  "del valle centro cdmx", "delegacion benito juarez", "delegacion iztacalco", 
  "df azcapotzalco", "df cuajimalpa", "df gam", "df iztapalapa", "df tlalpan", 
  "disfrito federal", "district federal", "distrio federal", "distrito federal cdmx", 
  "distrito federal mexico", "distrito federsl", "g a madero", "gustavo a madrro", 
  "iztacalco d f"
)

# Creamos diccionario
diccionario <- setNames(rep("Ciudad de México", length(variantes_cdmx)), variantes_cdmx)

# --- (3) genera banderas sin alterar el objeto original ---
# (crea columnas temporales para decidir el filtro)
datos_flag <- datos %>%
  mutate(
    .region_norm   = normalize_txt(region),
    .locality_norm = normalize_txt(locality),
    .region_std    = if_else(.region_norm   %in% names(diccionario), "Ciudad de México", NA_character_),
    .locality_std  = if_else(.locality_norm %in% names(diccionario), "Ciudad de México", NA_character_),
    .is_cdmx       = !is.na(.region_std) | !is.na(.locality_std)
  )

# --- (4) filtra y conserva EXACTAMENTE las columnas originales ---
cols_originales <- names(datos)
taquerias_cdmx <- datos_flag %>%
  filter(.is_cdmx) #%>%
  #select(all_of(cols_originales))


# --- (5) exporta en UTF-8 ---
write_csv(taquerias_cdmx, "taquerias_cdmx.csv")          # UTF-8 estánd
glue::glue("Registros exportados: {nrow(taquerias_cdmx)}  |  Variables: {ncol(taquerias_cdmx)}")


#Distancia de Haversine
library(geosphere) #Nuevo paquete para la distancia de Haversine

datos_xy <- taquerias_cdmx %>%
  filter(!is.na(latitude), !is.na(longitude))

# Indicador de si un registro pertenece a la alcaldía Cuauhtémoc
# (busca "cuauhtemoc" en 'region' o 'locality' normalizados)
is_cuauhtemoc <- function(region, locality) {
  r <- normalize_txt(region)
  l <- normalize_txt(locality)
  str_detect(r, "\\bcuauhtemoc\\b") | str_detect(l, "\\bcuauhtemoc\\b")
}

# Conjunto "sujeto" = taquerías de la alcaldía Cuauhtémoc
cuauh <- datos_xy %>%
  mutate(en_cuauhtemoc = is_cuauhtemoc(.region_norm, .locality_norm)) %>%
  filter(en_cuauhtemoc)


# Conjunto "vecinos" = TODAS las taquerías (en cualquier alcaldía)
vecinos <- datos_xy

#------------------------------
# 3) Matriz de distancias Haversine
#   distm usa c(lon, lat)
#------------------------------
M_cuauh   <- as.matrix(cuauh[, c("longitude", "latitude")])
M_vecinos <- as.matrix(vecinos[, c("longitude", "latitude")])
D <- geosphere::distm(M_cuauh, M_vecinos, fun = geosphere::distHaversine)
radio_m <- 1500
contar_competencia <- function(dist_row) sum(dist_row <= radio_m & dist_row > 0, na.rm = TRUE)
conteo <- apply(D, 1, contar_competencia)

resultado_top10 <- cuauh %>%
  mutate(competencia_1500m = conteo) %>%
  arrange(desc(competencia_1500m)) %>%
  slice_head(n = 10) %>%
  select(everything(), competencia_1500m)


resultado_final <- resultado_top10 %>%
  rename(total_competencia = competencia_1500m)

# exportamos a CSV con codificación correcta
write_csv(resultado_final, "top_10_taquerias.csv")
glue::glue("Archivo exportado: top_10_taquerias.csv | Registros: {nrow(resultado_final)} | Variables: {ncol(resultado_final)}")



# PARTE 2

#De la base “centroides_mexico.csv” toma el centroide de cada entidad federativa y determina el número de places
#que se encuentran a menos de 25 kilómetros de cada uno, tómalos de la base (Liga descarga places_foursquare.csv.zip).
#Para cada entidad, considera todos los places aunque no pertenezcan al mismo estado.

centroides <- read_csv("centroides_mexico.csv") %>%
  rename(latitude = lat, longitude = lon)     # asegúrate de que tenga columnas: entidad, lat, lon
centroides
places <- datos

# 2. Prepara matrices de coordenadas (lon, lat)
M_places <- as.matrix(places[, c("longitude", "latitude")])


# 3. Itera por cada entidad y calcula distancias
radio_m <- 25000  # 25 km en metros
conteos <- centroides %>%
  rowwise() %>%
  mutate(
    total_places_25km = {
      centroide <- c(longitude, latitude)
      distancias <- distHaversine(centroide, M_places)
      sum(distancias <= radio_m, na.rm = TRUE)
    }
  ) %>%
  ungroup()

print(conteos)



# 5. Renombrar columnas para la salida final
places_centroide_estado <- conteos %>%
  select(
    `Entidad Federativa` = Entidad_Federativa,
    latitud_centroide = latitude,
    longitud_centroide = longitude,
    total_places = total_places_25km,
  )

places_centroide_estado_ordenado <- places_centroide_estado %>%
  arrange(desc(total_places))

places_centroide_estado_ordenado
# 6. Exportar tabla base
write_csv(places_centroide_estado, "places_centroide_estado.csv")

