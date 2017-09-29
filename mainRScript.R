# Scrip principal para la la ejecución de los .R

#librerias

library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(spdep)

library(tidyverse)
library(magrittr)
library(stringr)

library(viridis)
library(RColorBrewer)
library(gridExtra)
library(wesanderson)

library(knitr)
library(sjPlot)
library(broom)

#library(visdat)
library(GGally)

library(ggrepel)
library(grid)
#(DiagrammeR)

# library(pander)
#library(stargazer)
#library(stargazer)
#  correr los script en el orden correcto para realizar todos los calculos 

source("funciones.R")
source("geodata.R")
source("arboles.R")
source("censopoblacion.R")
source("consolidarDatos.R")
source("analisis_exploratorio.R")
source("analisis_estadistico.R")
save.image(file = "bck1.RData")
source("analisis_geoestadistico.R")
