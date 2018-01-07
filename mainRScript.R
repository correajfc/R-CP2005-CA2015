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


#  correr los script en el orden correcto para realizar todos los calculos 

source("funciones.R")
source("geodata.R")
source("arboles.R")
source("censopoblacion.R")
source("consolidarDatos.R")
source("analisis_exploratorio.R")
source("analisis_estadistico.R")
source("analisis_geoestadistico.R")
source("analisis_estadistico_EV.R")
source("analisis_geoestadistico_EV.R")
save.image(file = "bck1.RData")
