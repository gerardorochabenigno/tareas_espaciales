# Tarea 2
rm(list=ls())
getwd()
setwd("./tarea/tarea1")

## 1. Descarga los archivos en parquet

### Cargamos librerías necesarias
library("hfhub")
library("arrow")
library("dplyr")
library("reticulate")

### Usaremos python para descargar los archivos
use_condaenv("espaciales", required=TRUE)
py_run_file("descargar_datos.py")
### Ejecutamos script de python que descarga datos
