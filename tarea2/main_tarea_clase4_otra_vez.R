# Tarea 1
rm(list=ls())
getwd()

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

########################
### 1 CARGA DE DATOS ###
########################

setwd("~/Downloads/tarea2/")
df_denue <- read_csv("denue_09_csv/conjunto_de_datos/denue_inegi_09_.csv")

df_denue <- df_denue |>
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))


df_places_foursquare <- read_csv("places_foursquare.csv") 
shp_mun <- st_read("09_ciudaddemexico/conjunto_de_datos/09mun.shp")
shp_mun <- shp_mun |>
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))
shp_mun <- st_transform(shp_mun,crs = 4326)

# Función para convertir grados, minutos, segundos a grados decimales
dms_to_decimal <- function(degrees, minutes, seconds) {
  return(degrees + minutes/60 + seconds/3600)
}


###########################################
### 1 DELIMITAMOS EL ÁREA - CUAUHTÉMOC ###
#https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/imagen_cartografica/map_top_municipal/794551123584_geo.pdf
# delimitación la delegación cuauhtémoc cdmx con coordenadas
# https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551123584
##########################################

# Convertir las coordenadas límite a grados decimales
lat_min <- dms_to_decimal(19, 22, 37)  # 19.37694
lat_max <- dms_to_decimal(19, 29, 20)  # 19.48889

# Longitud Oeste: 99°06'54" a 99°11'31" (valores negativos para oeste)
lon_min <- -dms_to_decimal(99, 11, 31)  # -99.19194 (más al oeste)
lon_max <- -dms_to_decimal(99, 6, 54)   # -99.11500 (menos al oeste)
rm(df_filtrado)

df_denue_filtro <- df_denue |>
  # 1) quitar nulos
  filter(!is.na(latitud), !is.na(longitud)) |>
  # 2) asegurar que sean numéricas
  mutate(
    latitud  = as.numeric(latitud),
    longitud = as.numeric(longitud)
  ) |>
  # 3) filtrar por el rectángulo
  filter(
    between(latitud,  lat_min, lat_max),
    between(longitud, lon_min, lon_max)
  )


# Notemos lo siguiente:
# Guiarnos por la variable de municipio para hacer filtros puede ser algo erroneo ya que pudiera ser incorrecto los datos
# es mejor hacer filtros a partir de la longitud y latitud. Notemos que filtrando por coordenas nos quedamos con 
# 67,115 registros en la cuauhtemoc y en el dataframe total sin filtros había 67,134 registros, por lo que tenemos sólo 
# una diferencia de 19 registros 

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

###########################################
### 2 Filtro . Actividad Económica  ###
# Bares, cantinas y similares  ó “Restaurantes con servicio de preparación de alimentos a la carta o de comida corrida #
##########################################

# Para realizar esto, veamos las categorías que tiene la actividad económica

df_denue_filtro |>
  summarise(categorias_unicas = n_distinct(nombre_act))

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

df_denue_filtro_act |>
  count(municipio, sort = TRUE) |>
  head()

## Finalmente usemos un polygono para delimitar bien los registros que sobran

# Polígono de Cuauhtémoc
cuauh <- shp_mun |> filter(CVEGEO == "09015")
cuauh
cuauh$geometry
#Mapa simple municipios
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


# Exporta los registros en la tabla “denue_cdmx.csv”
write_csv(denue_cuau, "denue_cdmx.csv")



###########################################
### 1 LIMPIEZA PLACES FOURSQUARE
##########################################
### 
# Para todos los registros de la tabla “denue_cdmx.csv”, determina el place de Foursquare que mejor coincida con el
# registro correspondiente del DENUE. Diseña un algoritmo para realizar la asignación.

# Vamos a seguir un procedimiento similar para la limpeza de de df_places_foursquare

## --- Limpiar NAs y filtrar por el rectángulo ---
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

# --- Preparar el polígono (anillo exterior) para inpolygon() ---

# ---  inpolygon() y filtrado final ---
inside <- inpolygon(
  x  = df_fsq_bbox$longitude,            # puntos a evaluar (longitudes)
  y  = df_fsq_bbox$latitude,             # puntos a evaluar (latitudes)
  xp = xp,                              # vértices del polígono (X)
  yp = yp,                              # vértices del polígono (Y)
  boundary = TRUE                       # incluir puntos exactamente en el borde
)

df_fsq_cuau <- df_fsq_bbox[inside, ]

colnames(df_fsq_cuau)
# Esto es momentaneo, para hacer pruebas, se debe jalar todo, es para el outer join
#df_fsq_cuau_ids <- df_fsq_cuau |>
#  select(fsq_place_id,name,latitude,longitude,address,
#         locality,region,postcode, date_created, date_refreshed, date_closed,fsq_category_labels)

df_fsq_cuau_ids <- df_fsq_cuau |>
  select(fsq_place_id,name,latitude,longitude)


#a. b. Por cada registro del DENUE, selecciona únicamente un place de Foursquare, el que mejor coincida.
#Debes ofrecer un candidato de places para todos de “denue_cdmx.csv”, no puedes dejar vacío.

109553*2280 = 249780840


########### PROCESO PARA MATCHEAR LOS REGITROS #######


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
  
  df %>%
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




spatial_blocking <- function(denue_df, fsq_df, k = 20, radius = 300, 
                             expand_radii = c(300, 500, 1000), 
                             mode = c("knn", "radius")) {
  mode <- match.arg(mode)
  
  # --- 1) Convertir dataframes a sf ---
  denue_sf <- st_as_sf(denue_df, coords = c("longitude", "latitude"), crs = 4326)
  fsq_sf   <- st_as_sf(fsq_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # --- 2) Distinto comportamiento según el modo ---
  if (mode == "knn") {
    # para cada DENUE, obtener k vecinos más cercanos
    nn <- st_nearest_neighbors(denue_sf, fsq_sf, k = k)
    
    # expandir a tabla denue_id - fsq_id
    pairs <- purrr::map2_dfr(
      denue_sf$id, nn,
      ~ tibble(denue_id = .x, fsq_id = fsq_sf$id[.y])
    )
    
  } else if (mode == "radius") {
    # para cada radio en secuencia, buscar intersecciones
    pairs <- tibble()
    for (r in expand_radii) {
      buf <- st_buffer(denue_sf, dist = units::set_units(r, "m"))
      cand <- st_intersects(buf, fsq_sf)
      step_pairs <- purrr::map2_dfr(
        denue_sf$id, cand,
        ~ tibble(denue_id = .x, fsq_id = fsq_sf$id[.y])
      )
      pairs <- bind_rows(pairs, step_pairs)
    }
    # asegurarnos que cada DENUE tiene al menos un candidato
    pairs <- pairs %>% distinct()
  }
  
  return(pairs)
}


library(sf)
library(dplyr)
library(purrr)
library(geosphere)

spatial_blocking <- function(denue_df, fsq_df, expand_radii = c(200, 300, 500)) {
  
  # --- 1) Convertir a sf ---
  denue_sf <- st_as_sf(denue_df, coords = c("longitude", "latitude"), crs = 4326)
  fsq_sf   <- st_as_sf(fsq_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # --- 2) Buscar candidatos por radios crecientes ---
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
  
  # --- 3) Eliminar duplicados ---
  pairs <- distinct(pairs)
  
  # --- 4) Calcular distancia haversine (m) ---
  pairs <- pairs %>%
    left_join(
      denue_df %>% select(denue_id = id, denue_lat = latitude, denue_lon = longitude),
      by = "denue_id"
    ) %>%
    left_join(
      fsq_df %>% select(fsq_id = id, fsq_lat = latitude, fsq_lon = longitude),
      by = "fsq_id"
    ) %>%
    mutate(
      distance_m = distHaversine(
        cbind(denue_lon, denue_lat),
        cbind(fsq_lon, fsq_lat),
        r = 6378137
      )
    )
  
  return(pairs)
}

pairs_radius <- spatial_blocking(
  denue_norm, fsq_norm,
  expand_radii = c(100, 200, 400)
)

head(pairs_radius)


pairs_ready <- pairs_radius %>%
  # agregar info de DENUE
  left_join(
    denue_norm %>%
      select(denue_id = id, denue_name_original = name_original, denue_name_norm = name_norm),
    by = "denue_id"
  ) %>%
  # agregar info de FSQ
  left_join(
    fsq_norm %>%
      select(fsq_id = id, fsq_name_original = name_original, fsq_name_norm = name_norm),
    by = "fsq_id"
  )


# Printing three rows 

scores_similities <- function(df, name1 = "denue_name_norm", name2 = "fsq_name_norm") {
  df %>%
    rowwise() %>%
    mutate(
      # --- 1) Levenshtein similarity ---
      lev_dist = stringdist(get(name1), get(name2), method = "lv"),
      lev_sim = ifelse(
        max(nchar(get(name1)), nchar(get(name2))) > 0,
        round(1 - lev_dist / max(nchar(get(name1)), nchar(get(name2))), 2),
        0
      ),
      
      # --- 2) Jaccard similarity ---
      jaccard_sim = {
        t1 <- str_split(get(name1), "\\s+")[[1]] %>% unique()
        t2 <- str_split(get(name2), "\\s+")[[1]] %>% unique()
        if (length(union(t1, t2)) == 0) {
          0
        } else {
          round(length(intersect(t1, t2)) / length(union(t1, t2)), 2)
        }
      },
      
      # --- 3) Combined score ---
      score_sim = round(0.5 * lev_sim + 0.5 * jaccard_sim, 2)
    ) %>%
    ungroup() #%>%
    # select(-lev_dist)   # limpiar columna auxiliar
}

#pairs_ready_sample <- sample_n(pairs_ready, 3) 
pairs_scored <- scores_similities(pairs_ready)
pairs_scored
#head(pairs_scored %>% select(denue_name_norm, fsq_name_norm, lev_sim, jaccard_sim, score_sim))

high_score_pairs <- pairs_scored %>%
  filter(score_sim > 0.7)



# Exporta los registros en la tabla “denue_places_cdmx.csv”
write_csv(high_score_pairs, "denue_places_cdmx.csv")

write_csv(pairs_scored, "denue_places_cdmx_all.csv")
