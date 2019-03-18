# Scrip principal para la la ejecución de los .R

#librerias

library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(spdep)
library(car)

library(tidyverse)
library(magrittr)
library(stringr)
library(purrr)
library(reshape2)
library(broom)

library(viridis)
library(RColorBrewer)
library(gridExtra)
library(wesanderson)

library(knitr)
library(bookdown)

library(GGally)
library(ggrepel)
library(grid)
library(ggfortify)
library(olsrr)
library(kableExtra)
library(lmtest)
library(glue)
library(visdat)
library(latex2exp)

#  correr los script en el orden correcto para realizar todos los calculos 
# Start the clock!
# ptm <- proc.time()
# 
# source("funciones.R")
# source("geodata.R")
# source("arboles.R")
# source("censopoblacion.R")
# source("consolidarDatos.R")
# source("analisis_exploratorio.R")
# source("analisis_estadistico.R")
# source("resultados.R")
# source("analisis_geoestadistico.R")
# source("analisis_estadistico_EV.R")
# source("analisis_geoestadistico_EV.R")
# # Stop the clock
# proc.time() - ptm
# save.image(file = "bck_201811.RData")

load("bck_201811.RData")


