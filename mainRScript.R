# Scrip principal para la la ejecución de los .R

#librerias
#libs <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap") 
library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(spdep)
library(car)
library(ggmap)
library(maptools)

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
ptm <- proc.time()

source("funciones.R", echo = T)
source("geodata.R", echo = T)
source("arboles.R", echo = T)
source("censopoblacion.R", echo = T)
source("consolidarDatos.R", echo = T)
source("analisis_exploratorio.R", echo = T)
source("analisis_estadistico.R", echo = T)
source("resultados.R", echo = T)
source("analisis_geoestadistico.R", echo = T)
source("analisis_estadistico_EV.R", echo = T)
source("analisis_geoestadistico_EV.R", echo = T)
# Stop the clock
proc.time() - ptm
save.image(file = "bck_202001.RData")

# load("bck_202001.RData")


