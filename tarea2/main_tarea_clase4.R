# Tarea 2
rm(list=ls())
getwd()

################################################################################
########################### 0 LIBRERIAS ########################################
################################################################################

# Cargar librería para lectura rápida
library(foreign)
library(tidyverse)
library(lubridate)
library(readr)
library(sp)
# library(rgdal)
library(pracma)
library(R.utils)
library(geosphere)
library(sf)
library(rvest)
library(rjson)
library(RCurl)
#Nuevo paquete a utilizar:
library(bayesbio)
library(stringdist)
library(stringi)


library(dplyr)
library(purrr)

################################################################################
########################### 0 FUNCIONES ########################################
################################################################################

# ---- Conversión ángulos DMS a grados decimales ------------------------------
# Recibe grados, minutos y segundos y regresa grados decimales.
dms_to_decimal <- function(degrees, minutes, seconds) {
  return(degrees + minutes/60 + seconds/3600)
}


# ---- Normalización de nombres para comparación ------------------------------
# Estandariza columnas (id, nombre, lat/lon) y genera 'name_norm' limpio.
# - id_col: columna id en el df original
# - name_col: columna del nombre en el df original
# - lat_col, lon_col: nombres de latitud/longitud en el df original
# Devuelve: id | name_original | name_norm | latitude | longitude
normalize_names_df <- function(df, id_col, name_col, lat_col, lon_col) {
  # stopwords específicas del contexto
  stopwords_mx <- c(
    "s.a. de c.v.", "sa de cv", "s. de r.l.", "sc", "sociedad anonima", 
    "compania", "compañia", "cia",
    "sucursal", "tienda", "local", "negocio", "establecimiento", "servicios",
    "restaurant", "restaurante", "cafeteria",
    "mexico", "cdmx", "ciudad de mexico"
  )
  
  # construir regex para eliminar stopwords (con bordes de palabra)
  stopwords_regex <- paste0("\\b(", paste(stopwords_mx, collapse="|"), ")\\b")
  
  df |>
    transmute(
      id            = .data[[id_col]],
      name_original = .data[[name_col]],
      # normalización de nombres
      name_norm = name_original |>
        str_to_lower() |>                        # minúsculas
        stringi::stri_trans_general("Latin-ASCII") |> # quitar acentos
        str_replace_all("[[:punct:]]", " ") |>   # quitar puntuación
        str_replace_all(stopwords_regex, " ") |>      # quitar stopwords
        str_replace_all("\\s+", " ") |>          # colapsar espacios
        str_trim(),
      latitude  = as.numeric(.data[[lat_col]]),
      longitude = as.numeric(.data[[lon_col]])
    )
}


# ---- Bloqueo espacial por radios crecientes (en metros) ---------------------
# Genera pares DENUE–FSQ candidatos por proximidad espacial:
# 1) Convierte a sf en EPSG:4326 (grados).
# 2) Proyecta a EPSG:3857 (metros) y crea buffers de r metros alrededor de cada DENUE.
# 3) Agrega todos los FSQ que caen dentro de esos buffers (200, 300, 500 m por defecto).
# 4) Quita duplicados y calcula distancia Haversine (metros) entre cada par.
# Devuelve: denue_id, fsq_id, coords de ambos y distance_m.
spatial_blocking <- function(denue_df, fsq_df, expand_radii = c(200, 300, 500)) {
  
  # 1) Convertir a sf 
  denue_sf <- st_as_sf(denue_df, coords = c("longitude", "latitude"), crs = 4326)
  fsq_sf   <- st_as_sf(fsq_df, coords = c("longitude", "latitude"), crs = 4326)
  
  #  2) Buscar candidatos por radios crecientes
  pairs <- tibble()
  for (r in expand_radii) {
    # trabajar en metros -> proyectar a EPSG:3857
    denue_buf <- st_buffer(st_transform(denue_sf, 3857), dist = r)
    fsq_3857  <- st_transform(fsq_sf, 3857)
    
    cand <- st_intersects(denue_buf, fsq_3857)
    
    step_pairs <- map2_dfr(
      denue_sf$id, cand,
      ~ tibble(denue_id = .x, fsq_id = fsq_sf$id[.y])
    )
    
    pairs <- bind_rows(pairs, step_pairs)
  }
  
  # 3) Eliminar duplicados
  pairs <- distinct(pairs)
  
  # 4) Calcular distancia haversine (m)
  pairs <- pairs |>
    left_join(
      denue_df |> select(denue_id = id, denue_lat = latitude, denue_lon = longitude),
      by = "denue_id"
    ) |>
    left_join(
      fsq_df |> select(fsq_id = id, fsq_lat = latitude, fsq_lon = longitude),
      by = "fsq_id"
    ) |>
    mutate(
      distance_m = distHaversine(
        cbind(denue_lon, denue_lat),
        cbind(fsq_lon, fsq_lat),
        r = 6378137
      )
    )
  
  return(pairs)
}


# ---- Score de Similitus a partir de Lev y Jaccard ----------------------------
# Calcula similitud de nombres entre dos columnas de un dataframe.
#   - name1 : nombre de la columna 1 (string, default "denue_name_norm").
#   - name2 : nombre de la columna 2 (string, default "fsq_name_norm").
# Devuelve:
#   El mismo dataframe + 3 nuevas columnas:
#     - lev_sim     : similitud de Levenshtein (0–1, 1 = idéntico).
#     - jaccard_sim : similitud de Jaccard (0–1, 1 = idéntico).
#     - score_sim   : promedio ponderado (0.5*lev + 0.5*jaccard).
# Cálculo:
#   1) Levenshtein: (1 - dist / longitud_máxima), redondeado a 2 decimales.
#   2) Jaccard: intersección(tokens) / unión(tokens), redondeado a 2 decimales.
#   3) Score global: promedio 50/50 de lev_sim y jaccard_sim.
scores_similities <- function(df, name1 = "denue_name_norm", name2 = "fsq_name_norm") {
  df |>
    rowwise() |>
    mutate(
      # 1) Levenshtein
      lev_dist = stringdist(get(name1), get(name2), method = "lv"),
      lev_sim = ifelse(
        max(nchar(get(name1)), nchar(get(name2))) > 0,
        round(1 - lev_dist / max(nchar(get(name1)), nchar(get(name2))), 2),
        0
      ),
      
      # 2) Jaccard
      jaccard_sim = {
        t1 <- str_split(get(name1), "\\s+")[[1]] |> unique()
        t2 <- str_split(get(name2), "\\s+")[[1]] |> unique()
        if (length(union(t1, t2)) == 0) {
          0
        } else {
          round(length(intersect(t1, t2)) / length(union(t1, t2)), 2)
        }
      },
      
      # 3) Score Global
      score_sim = round(0.5 * lev_sim + 0.5 * jaccard_sim, 2)
    ) |>
    ungroup() #|>
  # select(-lev_dist)   # limpiar columna auxiliar
}


################################################################################
########################### 1 CARGA DE DATOS ###################################
################################################################################

setwd("~/Downloads/tarea2/")

# Los mutate son para que no nos aparezcan caracteres especiales. Permiten leer en UTF-8
df_denue <- read_csv("denue_09_csv/conjunto_de_datos/denue_inegi_09_.csv")
df_denue <- df_denue |>
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))
df_places_foursquare <- read_csv("places_foursquare.csv") 
shp_mun <- st_read("09_ciudaddemexico/conjunto_de_datos/09mun.shp")
shp_mun <- shp_mun |>
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))
shp_mun <- st_transform(shp_mun,crs = 4326)


################################################################################
######################## 2 MANIPULACIÓN DE DATOS ###############################
################################################################################


################################################################################ 
#2.1 Manipulación de datos con el DENUE para actividades especificas en la Cuauh
################################################################################


##############################################
### 2.1.1 DELIMITAMOS EL ÁREA - CUAUHTÉMOC ###
#https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/imagen_cartografica/map_top_municipal/794551123584_geo.pdf
# delimitación la delegación cuauhtémoc cdmx con coordenadas
# https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551123584
# Para este caso se hará primero paso a paso
##############################################

# Convertir las coordenadas límite a grados decimales
lat_min <- dms_to_decimal(19, 22, 37)  # 19.37694
lat_max <- dms_to_decimal(19, 29, 20)  # 19.48889

# Longitud Oeste: 99°06'54" a 99°11'31" (valores negativos para oeste)
lon_min <- -dms_to_decimal(99, 11, 31)  # -99.19194 (más al oeste)
lon_max <- -dms_to_decimal(99, 6, 54)   # -99.11500 (menos al oeste)


# Filtramos, quitamos nulos, aseguramos que sea numérico y nos quedamos con el bounding box
df_denue_filtro <- df_denue |>
  filter(!is.na(latitud), !is.na(longitud)) |>
  mutate(
    latitud  = as.numeric(latitud),
    longitud = as.numeric(longitud)
  ) |>
  filter(
    between(latitud,  lat_min, lat_max),
    between(longitud, lon_min, lon_max)
  )


# Notemos lo siguiente:
# Guiarnos por la variable de municipio para hacer filtros puede ser algo erroneo ya que pudiera ser incorrecto los datos
# es mejor hacer filtros a partir de la longitud y latitud. 
# Notemos que filtrando por coordenas nos quedamos con  67,115 registros en la cuauhtemoc 
# y en el dataframe total sin filtros había 67,134 registros, 
# por lo que tenemos sólo una diferencia de 19 registros 

df_denue_filtro |>
  group_by(municipio) |>
  summarise(n = n(), .groups = 'drop') |>
  mutate(
    porcentaje = round((n/sum(n))*100, 2),
  ) |>
  arrange(desc(n))


df_denue_filtro |>
  group_by(municipio) |>
  summarise(n = n(), .groups = 'drop') |>
  mutate(
    porcentaje = round((n/sum(n))*100, 2),
  ) |>
  arrange(desc(n))

67134-67115

##############################################
### 2.1.2 Filtro . Actividad Económica     ###
# Bares, cantinas y similares  ó 
# “Restaurantes con servicio de preparación de alimentos a la carta o de comida corrida #
##############################################

# Para realizar esto, veamos las categorías que tiene la actividad económica

df_denue_filtro |>
  summarise(categorias_unicas = n_distinct(nombre_act))
# Son 848 categorías únicas

print( df_denue_filtro |>
  count(nombre_act, sort = TRUE) |>
  head(50), n=50)

actividades <- c(
  "Bares, cantinas y similares",
  "Restaurantes con servicio de preparación de alimentos a la carta o de comida corrida"
)

df_denue_filtro_act <- df_denue_filtro |> filter(nombre_act %in% actividades)

df_denue_filtro_act |>
         count(, sort = TRUE) |>
         head()

# Nos quedamos con 4,028 registros

df_denue_filtro_act |>
  count(municipio, sort = TRUE) |>
  head()

# De los cuáles tenemos etiquetados a 2,280 de la cuauhtémoc

##############################################
### 2.1.3 Inpolygon()  - Cuauhtémoc        ###
##############################################

# Finalmente usamos un polígono para quedarnos sólo con los que están en la cuauhtémoc
# Polígono de Cuauhtémoc

cuauh <- shp_mun |> filter(CVEGEO == "09015")
cuauh
cuauh$geometry
plot(cuauh$geometry)

co <- st_coordinates(cuauh)          # columnas: X, Y, L1, L2
ext <- co[co[, "L2"] == 1, ]         # anillo exterior
holes_idx <- setdiff(unique(co[, "L2"]), 1) # no tiene hoyos, tal como se ven en la gráfica

# vectores del polígono (xp, yp)
xp <- ext[, "X"]                       # longitudes de los vértices
yp <- ext[, "Y"]                       # latitudes de los vértices
# puntos a probar (x, y)
x <- df_denue_filtro_act$longitud      # longitudes de negocios
y <- df_denue_filtro_act$latitud       # latitudes de negocios

# máscara: incluir también los de la frontera
inside <- inpolygon(x, y, xp, yp, boundary = TRUE)


# filtrar dataframe
denue_cuau <- df_denue_filtro_act[inside, ]

denue_cuau |>
  count(, sort = TRUE) |>
  head()

denue_cuau |>
  count(municipio, sort = TRUE) |>
  head()
# Finalmente nos quedamos con 2,280 registros y todos pertenecen a la cuauhtemoc

##############################################
### 2.1.3 Exportar Data - denue_cdmx.csv   ###
##############################################


# Exportamos los registros en la tabla “denue_cdmx.csv”
write_csv(denue_cuau, "denue_cdmx.csv")

################################################################################ 
#2.2 Manipulación de datos con FOURSQUARE
################################################################################

# Activida a realizar
# Para todos los registros de la tabla “denue_cdmx.csv”, determina el place de 
# Foursquare que mejor coincida con el registro correspondiente del DENUE. 
# Diseña un algoritmo para realizar la asignación.

# Vamos a seguir un procedimiento similar al del Denue para la limpeza de de df_places_foursquare

##  Limpiar NAs y filtrar por el rectángulo 
df_fsq_bbox <- df_places_foursquare |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  filter(
    between(latitude,  lat_min, lat_max),
    between(longitude, lon_min, lon_max)
  )

# Preparar el polígono (anillo exterior) para inpolygon() y filtrado fina
inside <- inpolygon(
  x  = df_fsq_bbox$longitude,            # puntos a evaluar (longitudes)
  y  = df_fsq_bbox$latitude,             # puntos a evaluar (latitudes)
  xp = xp,                              # vértices del polígono (X)
  yp = yp,                              # vértices del polígono (Y)
  boundary = TRUE                       # incluir puntos exactamente en el borde
)

df_fsq_cuau <- df_fsq_bbox[inside, ]



################################################################################
#################### 3 TÉCNICA DE MATCHEO DE DATOS  ############################
################################################################################

################################################################################
######## LA TÉCNICA SE RESUMEN EN LOS SIGUIENTES PUNTOS ########################

# 1. Normalización de nombres: limpiar, pasar a minúsculas, quitar acentos, puntuación y stopwords comerciales.
# 2. Bloqueo espacial: para cada DENUE buscar lugares de Foursquare dentro de un radio creciente (200–500 m).
# 3. Cálculo de distancias: medir en metros con fórmula de Haversine entre cada par candidato.
# 4. Similitud de nombres: combinar Levenshtein y Jaccard en un score_sim (ponderación 0.5 + 0.5).
# 5. Selección de pares: filtrar candidatos con score_sim > 0.8 y elegir el mejor match por DENUE.
################################################################################


# Primero vamos a remover todo lo que ya no utilizamos
rm(lat_min, lat_max, lon_min, lon_max, df_denue_filtro, df_denue_filtro_act, inside,co, ext,holes_idx, xp, yp, x, y, cuauh,df_fsq_bbox,actividades)


colnames(df_fsq_cuau)

# Se seleccionan sólo variables de interés para hacer el empezar a hacer el matcheo (menor costo computacional)
######
df_fsq_cuau_ids <- df_fsq_cuau |>
  select(fsq_place_id,name,latitude,longitude)


#a. b. Por cada registro del DENUE, selecciona únicamente lugares cercanos de Foursquare, los mejores candidatos
# Debes ofrecer un candidato de places para todos de “denue_cdmx.csv”, no puedes dejar vacío.

109553*2280 = 249780840


################################################################################ 
#3.1 Normalización de Texto
################################################################################

# Para DENUE
denue_norm <- normalize_names_df(
  df = denue_cuau,
  id_col = "id",
  name_col = "nom_estab",
  lat_col = "latitud",
  lon_col = "longitud"
)

# Para Foursquare
fsq_norm <- normalize_names_df(
  df = df_fsq_cuau_ids,
  id_col = "fsq_place_id",
  name_col = "name",
  lat_col = "latitude",
  lon_col = "longitude"
)



################################################################################ 
#3.2 Bloqueo Espacial
################################################################################

pairs_radius <- spatial_blocking(
  denue_norm, fsq_norm,
  expand_radii = c(100, 200, 400)
)

head(pairs_radius)

pairs_radius |>
  group_by(denue_id) |>
  summarise(count = n())


length(unique(pairs_radius$denue_id))


################################################################################ 
#3.3 Unión de Datos (Sólo los escenaciales)
################################################################################

pairs_ready <- pairs_radius |>
  # agregar info de DENUE
  left_join(
    denue_norm |>
      select(denue_id = id, denue_name_original = name_original, denue_name_norm = name_norm),
    by = "denue_id"
  ) |>
  # agregar info de FSQ
  left_join(
    fsq_norm |>
      select(fsq_id = id, fsq_name_original = name_original, fsq_name_norm = name_norm),
    by = "fsq_id"
  )


################################################################################ 
#3.4 Scores de Similitud
################################################################################


#pairs_ready_sample <- sample_n(pairs_ready, 3) 
pairs_scored <- scores_similities(pairs_ready)
pairs_scored
#head(pairs_scored |> select(denue_name_norm, fsq_name_norm, lev_sim, jaccard_sim, score_sim))

################################################################################ 
#3.5 Flagear Datos
################################################################################


target_denues <- pairs_scored |> distinct(denue_id)




# --- Parámetro para escalar distancia a [0,1] ---
dist_cap <- 100
# =========================
# 1) Selección principal
# =========================
# Requisito mínimo de similitud de nombre
# y ranking por score combinado: 0.75*score_sim + 0.25*score_dist
top_by_score <- pairs_scored %>%
  filter(score_sim >= 0.6) %>%
  mutate(
    score_dist      = pmax(0, 1 - (distance_m / dist_cap)),
    primary_score   = 0.75 * score_sim + 0.25 * score_dist
  ) %>%
  arrange(denue_id, desc(primary_score), distance_m, fsq_id) %>%
  group_by(denue_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(match = 1)  # match fuerte (pasada principal)



# =========================
# 2) Fallback (para los que faltan)
# =========================
missing_denues <- target_denues %>%
  anti_join(top_by_score %>% distinct(denue_id), by = "denue_id")
dist_cap <- 500
top_by_distance <- pairs_scored %>%
  semi_join(missing_denues, by = "denue_id") %>%
  filter(score_sim >= 0.1) %>%                          # umbral mínimo de similitud
  mutate(
    score_dist      = pmax(0, 1 - (distance_m / dist_cap)),
    fallback_score  = 0.5 * score_sim + 0.5 * score_dist
  ) %>%
  arrange(denue_id, desc(fallback_score), desc(score_sim), distance_m, fsq_id) %>%
  group_by(denue_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(match = 0)  # asignado por fallback


# =========================
# 3) Unión final (1 por DENUE)
# =========================
final_matches <- bind_rows(top_by_score, top_by_distance) %>%
  arrange(denue_id)



# 1) Renombrar columnas de df_denue con sufijo "_denue"
df_denue_ren <- df_denue |>
  rename_with(~ paste0(.x, "_denue"), -id)

# 2) Renombrar columnas de df_places_foursquare con sufijo "_fsq"
df_fsq_ren <- df_places_foursquare |>
  rename_with(~ paste0(.x, "_fsq"), -fsq_place_id)

# 3) Join con final_matches
final_full <- final_matches |>
  left_join(df_denue_ren, by = c("denue_id" = "id")) |>
  left_join(df_fsq_ren, by = c("fsq_id" = "fsq_place_id"))



################################################################################ 
#3.5 Exportar Datos
################################################################################

# Exporta los registros en la tabla “denue_places_cdmx.csv”
write_csv(final_full, "denue_places_cdmx.csv")

write_csv(pairs_scored, "denue_places_cdmx_all.csv")

write_csv(final_matches,"denue_places_cdmx_all_2280.csv")



