# Código para armar df por estado
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(sf)
library(gt)

# Fijamos dirección. La estructura de la carpeta es
# |- taller/
# |-- data/
# |--- censo/ # Archivos del censo
# |--- marco/ # Archivos del marco geoestadístico
# |--- centros.xlsx

setwd("/Users/gerardorochabenigno/Documents/ITAM/datos_espaciales/clases/clase7/taller")

# Archivos del censo

# Listamos los archivos de la carpeta data que terminan en .csv
ls_files_censo <- list.files(recursive = TRUE, pattern = "conjunto_de_datos_ageb_urbana")

# Listamos los archivos shp por manzana
ls_files_marco <- list.files(recursive = TRUE, pattern = "\\d+m\\.shp")

# Cargamos el dataframe
#df_censo <- archivos_censo_list |>
#  map_df(~read_csv(.x) |> select(POBTOT, ENTIDAD))

# Cargamos dataframe de centroides
df_centros <- read_xlsx("./data/centros.xlsx")
df_centros$CVE_ENT <- c("01","02","03","04","07","08","09","05","06","10","11","12","13","14",
                        "16","17","15","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32")

# Dataframe para almacenar resultados
resultados_finales <- tibble()

# Loop para los 32 estados
for (i in 1:length(ls_files_censo)) {
  
  # Extraer CVE_ENT del nombre del archivo
  cve_ent <- str_extract(ls_files_censo[i], "(?<=urbana_)\\d{2}(?=_)")
  
  # Cargamos dataframe del censo
  df_censo <- read_csv(ls_files_censo[i], show_col_types = FALSE) |>
    select(ENTIDAD, MUN, AGEB, MZA, LOC, POBTOT, 
           VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
           VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB) |>
    mutate(
      across(
        c(POBTOT, VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
          VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB),
        as.numeric)
    ) |>
    rename(
      CVE_ENT = ENTIDAD,
      CVE_MUN = MUN,
      CVE_LOC = LOC,
      CVE_AGEB = AGEB,
      CVE_MZA = MZA
    ) |>
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), width = 2, pad = "0", side = "left"),
      CVE_MUN = str_pad(as.character(CVE_MUN), width = 3, pad = "0", side = "left"),
      CVE_LOC = str_pad(as.character(CVE_LOC), width = 4, pad = "0", side = "left"),
      CVE_AGEB = str_pad(as.character(CVE_AGEB), width = 4, pad = "0", side = "left"),
      CVE_MZA = str_pad(as.character(CVE_MZA), width = 3, pad = "0", side = "left")
    ) |>
    mutate(CVEGEO = paste0(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA)) |>
    arrange(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA) |>
    filter(CVE_MZA != "000")
  
  # Cargamos dataframe del marco
  df_marco <- st_read(ls_files_marco[i], quiet = TRUE) |>
    filter(CVE_MZA != "000") |>
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), width = 2, pad = "0", side = "left"),
      CVE_MUN = str_pad(as.character(CVE_MUN), width = 3, pad = "0", side = "left"),
      CVE_LOC = str_pad(as.character(CVE_LOC), width = 4, pad = "0", side = "left"),
      CVE_AGEB = str_pad(as.character(CVE_AGEB), width = 4, pad = "0", side = "left"),
      CVE_MZA = str_pad(as.character(CVE_MZA), width = 3, pad = "0", side = "left")
    ) |>
    arrange(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA)
  
  # Combinamos la información
  df_completo <- df_marco |> 
    left_join(df_centros, by = "CVE_ENT") |>
    left_join(df_censo, by = "CVEGEO")
  
  # Liberamos memoria
  rm(df_censo, df_marco)
  gc()
  
  # Transformamos a CRS proyectado
  df_completo_2 <- st_transform(df_completo, crs = 6372)
  
  # Crear centroide
  centroide <- df_completo_2 |>
    slice(1) |>
    st_drop_geometry() |>
    select(lon, lat) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs = 6372)
  
  # Manzanas alrededor de 3 KM
  buffer <- st_buffer(centroide, dist = 3000)
  df_manzanas <- df_completo_2 |>
    st_filter(buffer)
  
  # Liberamos memoria
  rm(df_completo, df_completo_2, centroide, buffer)
  gc()
  
  # Rellenamos valores faltantes
  df_manzanas <- df_manzanas |>
    mutate(
      across(
        c(POBTOT, VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
          VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB),
        ~replace_na(., 0)
      )
    )
  
  # Calculamos el máximo de viviendas registradas
  df_manzanas <- df_manzanas |>
    mutate(max_viviendas = pmax(VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
                                VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB, na.rm = TRUE))
  
  # Calculamos totales
  totales <- df_manzanas |>
    st_drop_geometry() |>
    summarise(
      CVE_ENT = cve_ent,
      POBTOT_tot = sum(POBTOT, na.rm = TRUE),
      VPH_C_ELEC_tot = sum(VPH_C_ELEC, na.rm = TRUE),
      VPH_EXCSA_tot = sum(VPH_EXCSA, na.rm = TRUE),
      VPH_DRENAJ_tot = sum(VPH_DRENAJ, na.rm = TRUE),
      VPH_REFRI_tot = sum(VPH_REFRI, na.rm = TRUE),
      VPH_AUTOM_tot = sum(VPH_AUTOM, na.rm = TRUE),
      VPH_TV_tot = sum(VPH_TV, na.rm = TRUE),
      VPH_PC_tot = sum(VPH_PC, na.rm = TRUE),
      VPH_CEL_tot = sum(VPH_CEL, na.rm = TRUE),
      VPH_INTER_tot = sum(VPH_INTER, na.rm = TRUE),
      VPH_STVP_tot = sum(VPH_STVP, na.rm = TRUE),
      VPH_CVJ_tot = sum(VPH_CVJ, na.rm = TRUE),
      VIVPAR_HAB_tot = sum(VIVPAR_HAB, na.rm = TRUE),
      max_viviendas_tot = sum(max_viviendas, na.rm = TRUE)
    )
  
  # Agregamos al dataframe de resultados
  resultados_finales <- bind_rows(resultados_finales, totales)
  
  # Liberamos memoria
  rm(df_manzanas)
  gc()
  
}

write.csv(resultados_finales, file="data/resultados_finales.csv")


################################################################ Para la parte 2
# Cargar polígonos antes del loop
ls_files_auto <- list.files("./data/poligonos_auto_3km", full.names = TRUE)
df_poligonos <- map_df(ls_files_auto, ~st_read(.x, quiet = TRUE))

df_poligonos$CVE_ENT <- c("01","03","02","04","07","08","09","05","06","10",
                          "11","12","13","14","15","16","17","18","19","20",
                          "21","22","23","24","25","26","27","28","29","30",
                          "31","32")
df_poligonos <- df_poligonos |> 
  select(CVE_ENT, geometry) |>
  st_transform(crs = 6372)  # Transformar a mismo CRS que usarás

# Dataframe para resultados
resultados_finales <- tibble()

# Loop para los 32 estados
for (i in 1:length(ls_files_censo)) {
  
  # Extraer CVE_ENT del nombre del archivo
  cve_ent <- str_extract(ls_files_censo[i], "(?<=urbana_)\\d{2}(?=_)")
  
  # Cargamos dataframe del censo
  df_censo <- read_csv(ls_files_censo[i], show_col_types = FALSE) |>
    select(ENTIDAD, MUN, AGEB, MZA, LOC, POBTOT, 
           VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
           VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB) |>
    mutate(
      across(
        c(POBTOT, VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
          VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB),
        as.numeric)
    ) |>
    rename(
      CVE_ENT = ENTIDAD,
      CVE_MUN = MUN,
      CVE_LOC = LOC,
      CVE_AGEB = AGEB,
      CVE_MZA = MZA
    ) |>
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), width = 2, pad = "0", side = "left"),
      CVE_MUN = str_pad(as.character(CVE_MUN), width = 3, pad = "0", side = "left"),
      CVE_LOC = str_pad(as.character(CVE_LOC), width = 4, pad = "0", side = "left"),
      CVE_AGEB = str_pad(as.character(CVE_AGEB), width = 4, pad = "0", side = "left"),
      CVE_MZA = str_pad(as.character(CVE_MZA), width = 3, pad = "0", side = "left")
    ) |>
    mutate(CVEGEO = paste0(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA)) |>
    arrange(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA) |>
    filter(CVE_MZA != "000")
  
  # Cargamos dataframe del marco
  df_marco <- st_read(ls_files_marco[i], quiet = TRUE) |>
    filter(CVE_MZA != "000") |>
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), width = 2, pad = "0", side = "left"),
      CVE_MUN = str_pad(as.character(CVE_MUN), width = 3, pad = "0", side = "left"),
      CVE_LOC = str_pad(as.character(CVE_LOC), width = 4, pad = "0", side = "left"),
      CVE_AGEB = str_pad(as.character(CVE_AGEB), width = 4, pad = "0", side = "left"),
      CVE_MZA = str_pad(as.character(CVE_MZA), width = 3, pad = "0", side = "left")
    ) |>
    arrange(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA)
  
  # Combinamos la información
  df_completo <- df_marco |> 
    left_join(df_centros, by = "CVE_ENT") |>
    left_join(df_censo, by = "CVEGEO")
  
  # Liberamos memoria
  rm(df_censo, df_marco)
  gc()
  
  # Transformamos a CRS proyectado
  df_completo_2 <- st_transform(df_completo, crs = 6372)
  
  # Obtener polígono del estado actual en lugar de crear buffer
  poligono_estado <- df_poligonos |>
    filter(CVE_ENT == cve_ent)
  
  # Filtrar manzanas que intersectan con el polígono
  df_manzanas <- df_completo_2 |>
    st_filter(poligono_estado)
  
  # Liberamos memoria
  rm(df_completo, df_completo_2, poligono_estado)
  gc()
  
  # Rellenamos valores faltantes
  df_manzanas <- df_manzanas |>
    mutate(
      across(
        c(POBTOT, VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
          VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB),
        ~replace_na(., 0)
      )
    )
  
  # Calculamos el máximo de viviendas registradas
  df_manzanas <- df_manzanas |>
    mutate(max_viviendas = pmax(VPH_C_ELEC, VPH_EXCSA, VPH_DRENAJ, VPH_REFRI, VPH_AUTOM, 
                                VPH_TV, VPH_PC, VPH_CEL, VPH_INTER, VPH_STVP, VPH_CVJ, VIVPAR_HAB, na.rm = TRUE))
  
  # Calculamos totales
  totales <- df_manzanas |>
    st_drop_geometry() |>
    summarise(
      CVE_ENT = cve_ent,
      POBTOT_tot = sum(POBTOT, na.rm = TRUE),
      VPH_C_ELEC_tot = sum(VPH_C_ELEC, na.rm = TRUE),
      VPH_EXCSA_tot = sum(VPH_EXCSA, na.rm = TRUE),
      VPH_DRENAJ_tot = sum(VPH_DRENAJ, na.rm = TRUE),
      VPH_REFRI_tot = sum(VPH_REFRI, na.rm = TRUE),
      VPH_AUTOM_tot = sum(VPH_AUTOM, na.rm = TRUE),
      VPH_TV_tot = sum(VPH_TV, na.rm = TRUE),
      VPH_PC_tot = sum(VPH_PC, na.rm = TRUE),
      VPH_CEL_tot = sum(VPH_CEL, na.rm = TRUE),
      VPH_INTER_tot = sum(VPH_INTER, na.rm = TRUE),
      VPH_STVP_tot = sum(VPH_STVP, na.rm = TRUE),
      VPH_CVJ_tot = sum(VPH_CVJ, na.rm = TRUE),
      VIVPAR_HAB_tot = sum(VIVPAR_HAB, na.rm = TRUE),
      max_viviendas_tot = sum(max_viviendas, na.rm = TRUE)
    )
  
  # Agregamos al dataframe de resultados
  resultados_finales <- bind_rows(resultados_finales, totales)
  
  # Liberamos memoria
  rm(df_manzanas, totales)
  gc()
  
}

write.csv(resultados_finales, file = "data/resultados_finales_poligonos.csv", row.names = FALSE)
