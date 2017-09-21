# Scrip principal para la la ejecución de los .R

#librerias

library(rgdal)
library(rgeos)
library(raster)
library(sp)

library(tidyverse)
library(magrittr)
library(stringr)

library(viridis)
library(RColorBrewer)
library(gridExtra)

library(visdat)
library(GGally)
library(wesanderson)

library(ggrepel)
#(DiagrammeR)


#  correr los script en el orden correcto para realizar todos los calculos 

source("funciones.R")
source("geodata.R")
source("arboles.R")
source("censopoblacion.R")
source("consolidarDatos.R")
source("analisis_exploratorio.R")
source("analisis_estadistico.R")
source("analisis_geoestadistico.R")
