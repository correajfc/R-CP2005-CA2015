##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Analisis exploratorio de los datos
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: consolidarDatos.R
##################################################

library(visdat)
library(GGally)
library(wesanderson)
#library(OpenStreetMap)
#library(ggforce)
analisis.cali.df
summary(analisis.cali.base)
vis_dat(analisis.cali.base)

# # descargar mapas base
# latlong <- "+init=epsg:4326"
# b_poly.idesc <- as(extent(su), "SpatialPolygons")
# proj4string(b_poly.idesc)<-crs_mc_idesc
# b_poly.ll<-spTransform(b_poly.idesc,latlong)
# b<-bbox(b_poly.ll)
# b<-expandBbox(b,0.01,00.1)
# 
# #obtener mapa base
# bsm.terreno<-OpenStreetMap::openmap(upperLeft =  c(b[2,2],b[1,1]) , 
#                     lowerRight =  c(b[2,1],b[1,2]), 
#                     type = "stamen-terrain")
# bsm.terreno.idesc<- openproj(bsm.terreno, crs_mc_idesc)


# Arboles

AU_analsis_spatial %>% summary()

AU_analsis %>% 
  #filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
  geom_point(aes(y=altura_arbol,x=diametro_copa,color=vegetacion),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  facet_wrap(~emplazamiento , ncol = 5 ,labeller = label_wrap_gen())+
  guides(colour = guide_legend(override.aes = list(alpha=1)))+
  tema_lgnd_up()

ggsave("./images/facet-emplazamiento-color-vegetacion.png",
       scale = 1, width = 8, height = 11,
       dpi = 300)

#carateristicas por tipo de vegetacion
AU_analsis %>% 
  ggplot()+
  geom_point(aes(y=altura_arbol,x=diametro_copa),alpha=0.1, color ="forestgreen" )+
  coord_equal()+
  theme_bw()+
  facet_wrap( ~ vegetacion, nrow = 2 )

ggsave("./images/facet-vegetacion.png",
       scale = 1, width = 8, height = 8,
       dpi = 300)

# grafica de arboles con el diametro a escala en QGIS

#varibilidad del diametro de copa por emplazamiento

AU_analsis %>% 
ggplot( aes(x=emplazamiento,y=diametro_copa))+
  geom_jitter(position = position_jitter(0.3),alpha=0.1, color = "forestgreen")+ 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "tomato")+
  coord_flip()+
  tema_lgnd_up()

ggsave("./images/diametro-emplazamiento.png",
       scale = 1, width = 11, height = 8,
       dpi = 300)


#varibilidad del diametro de copa por emplazamiento

AU_analsis %>% 
  ggplot( aes(x=emplazamiento,y=altura_arbol))+
  geom_jitter(position = position_jitter(0.3),alpha=0.1, color = "forestgreen")+ 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "tomato")+
  coord_flip()+
  tema_lgnd_up()

ggsave("./images/altura-emplazamiento.png",
       scale = 1, width = 11, height = 8,
       dpi = 300)

# crear mapas base para la graficacion

base_plot.manzanas<- ggplot()+
            geom_polygon(data = manzanas.su.f,
                         aes(x = long, y = lat, group = group),
                         fill="grey80") +coord_equal() + theme_bw()

# puntos por emplazamiento
base_plot.manzanas + geom_point(data = AU_analsis,
                                aes(x = Este, y = Norte),
                                size=0.01,
                                color="forestgreen",
                                alpha=0.1)+
  theme_void()+
  facet_wrap(~emplazamiento , ncol = 5 ,labeller = label_wrap_gen())

  ggsave("./images/mapa-arboles-facet-emplazamiento.png",
       scale = 1, width = 11, height = 8,
       dpi = 300)

# suma de cobertura por hex
p.hex.copa  <-base_plot.manzanas+ stat_summary_hex(data = AU_analsis,
                                 aes(x = Este, y = Norte, z = area_copa),
                               binwidth = c(250, 250),
                               fun = sum 
                               )+
    geom_path(data = su.f,
                 aes(x = long, y = lat, group = group),
              color ="grey50",
              size=0.2)+
    coord_equal()+
    scale_fill_viridis(name="área \n copa",direction = 1)+
    theme_void()
    
  # cantidad de arboles por hex
p.hex.arboles<- base_plot.manzanas+ geom_hex(data = AU_analsis,
                                       aes(x = Este, y = Norte),
                                       binwidth = c(250, 250)
  )+
    geom_path(data = su.f,
              aes(x = long, y = lat, group = group),
              color ="grey50",
              size=0.2)+
    coord_equal()+
    scale_fill_viridis(name="num \narboles",direction = 1)+
    theme_void()
  

  
# mapa de valores agregados del cobertura y otros valores de CA2015

  
p.su.copa<-su.f %>% dplyr::select(-area_su)  %>%
  left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = area_copa))+
  coord_equal()+
  theme_void()+
  scale_fill_viridis(name = "área \n copa", direction = 1 )


p.su.arboles<-su.f %>% dplyr::select(-area_su)  %>%
  left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = num_arboles))+
  coord_equal()+
  theme_void()+
  scale_fill_viridis(name = "num \narboles ", direction = 1 )




grid.arrange(p.hex.copa,p.su.copa, ncol = 2, top = "Agregación de area de copa por hexagonos y SU")
grid.arrange(p.hex.arboles,p.su.arboles, ncol = 2, top = "Agregación de número de árboles por hexagonos y SU")


#  metricas agregadas del censo arboreo ----
metricas.ca<-c("area_copa",
               "num_arboles",
               "diametro_medio_copa",
               "altura_media",
               "cobertura_copa.su",
               "cobertura_copa.ap",
               "arboles_area.su",
               "arboles_area.ap",
               "arboles_habitante")




analisis.cali.df %>% select(one_of(metricas.ca)) %>%
  mutate_all(cut_number,10) -> metricas.ca.qn

analisis.cali.df %>% select(one_of(metricas.ca)) %>%
  mutate_all(ntile,10)->metricas.ca.ntl

metricas.ca.qn<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.ca.qn)
metricas.ca.ntl<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.ca.ntl)

# datos en formato long para graficacion de small multiples

metricas.ca.ntl.long<-gather(metricas.ca.ntl, 
                             key = metricas.arboles,
                            value = valores,
                            area_copa:arboles_habitante)


su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.ca.qn,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = area_copa))+
  coord_equal()+
  theme_void()+
  scale_fill_viridis(name = "num \narboles ", 
                     direction = 1 ,
                     discrete = T, 
                     drop=FALSE,
                     na.value ="grey50")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.ca.ntl,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(area_copa)))+
  coord_equal()+
  theme_void()+
  scale_fill_viridis(name = "num \narboles ", 
                     direction = 1 ,
                     discrete = T, 
                     drop=FALSE,
                     na.value ="grey50")


# small multiple datos arboles -----

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.ca.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.arboles %in% metricas.ca[c(1,5,6)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.arboles, nrow = 1)+
  tema_lgnd_abajo()


su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.ca.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.arboles %in% metricas.ca[c(1,3,4)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.arboles, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.ca.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.arboles %in% metricas.ca[c(2,7,8,9)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.arboles, nrow = 1)+
  tema_lgnd_abajo()


#histogramas arboles -----

analisis.cali.df %>% select(one_of(metricas.ca)) %>%
  gather( key = metricas.arboles,
          value = valores,
          area_copa:arboles_habitante) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30,
                 color = "white", fill="forestgreen")+
  facet_wrap(~metricas.arboles, scales = "free")




# relacion entre variables sobre los arboles ----




ggpairs(
  analisis.cali.df[,metricas.ca[c(1,2,3,4)]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.ca[c(1,5,6)]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.ca[c(2,7,8,9)]], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

# la seleccion de varibles relacionadas con elcenso arboreo que se eleigen como
# dependentes para ser explicadas son area copa, por ser una variable
# ampliamente usada en la literatura Por otro lado, las variantes relativas al
# area de cada sector urbano permiten hacer una comparación entre los diferentes
# sectores, están "normalizadas" y puede ser interesante ver el ajuste
# de modelos con base en estas metricas que describen de forma relativa el grado
# de arborizacion de cada sector.
# 




# variables estructurales ----

metricas.estruct<- c("area_media_manzana",
                     "area_manzanas",
                     "num_manzanas",
                     "area_ep",
                     "area_publica",
                     "area_privada",
                     "area_calle",
                     "area_ep.porcentaje",
                     "area_publica.porcentaje",
                     "area_privada.porcentaje",
                     "area_calle.porcentaje",
                     "area_su")


# analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
#   mutate_all(cut_number,7) -> metricas.estruct.qn

analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
  mutate_all(ntile,10)->metricas.estruct.ntl

# metricas.estruct.qn<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.estruct.qn)
metricas.estruct.ntl<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.estruct.ntl)

# datos en formato long para graficacion de small multiples

metricas.estruct.ntl.long<-gather(metricas.estruct.ntl, 
                             key = metricas.estructura,
                             value = valores,
                             area_media_manzana:area_su)


# small multiple datos estructura ----

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.estruct.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.estructura %in% metricas.estruct[c(1,2,3)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.estructura, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.estruct.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.estructura %in% metricas.estruct[c(4,5,6,7,12)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.estructura, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.estruct.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.estructura %in% metricas.estruct[c(8,9,10,11)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.estructura, nrow = 1)+
  tema_lgnd_abajo()

#histogramas estructura ----

analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
  gather( key = metricas.estructura,
          value = valores,
          area_media_manzana:area_su) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~metricas.estructura, scales = "free", ncol = 3)

# relacion entre varibles de de estructura -----

ggpairs(
  analisis.cali.df[,metricas.estruct[c(1,2,3)]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.estruct[c(4,5,6,7,12)]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.estruct[c(8,9,10,11)]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)



ggduo(analisis.cali.df,
      columnsX =metricas.estruct[c(8,9,10,11)], 
      columnsY =metricas.estruct[ c(4,5,6,7,12)],
      types = list(continuous = wrap(lm_with_cor))
      )

ggduo(analisis.cali.df,
      columnsX =metricas.estruct[c(8,9,10,11)], 
      columnsY =metricas.estruct[c(1,2,3)],
      types = list(continuous = wrap(lm_with_cor))
)

# relacion entre varibles a predicir y metricas de estructura
dep.arboles<-metricas.ca[c(1,5,6)]
ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.estruct[c(1,2,3)],
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.estruct[ c(4,5,6,7,12)],
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.estruct[c(8,9,10,11)],
      types = list(continuous = wrap(lm_with_cor))
)


ggduo(analisis.cali.df,
      columnsX =dep.arboles, 
      columnsY =metricas.estruct[c(8,9,10,11)],
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

# primera preseleccion sin haber eliminado outliers y sectores urbanos con
# carateristicas especiales.

presel.estructura<-c("area_ep","area_ep.porcentaje") # revisar

# variables sobre los predios del censo de poblacion -----


metricas.predios<- c("casa",
                     "casa_indigena", 
                     "apartamento",
                     "tipo_cuarto",
                     "otro_tipo_de_vivienda",
                     #"viviendas_tipo",                  
                     "uso_vivienda",
                     "uso_unidad_economica",
                     "uso_LEA",
                     #"viviendas_uso",
                     "casa.porcentaje",
                     "apartamento.porcentaje",
                     "cuarto.porcentaje",               
                     "otro_tipo_de_vivienda.porcentaje",
                     "viviendas.porcentaje",
                     "unidad_economica.porcentaje",    
                     "LEA.porcentaje"
                     )


# analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
#   mutate_all(cut_number,7) -> metricas.estruct.qn

analisis.cali.df %>% select(one_of(metricas.predios)) %>%
  mutate_all(ntile,10)->metricas.predios.ntl

# metricas.estruct.qn<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.estruct.qn)
metricas.predios.ntl<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.predios.ntl)

# datos en formato long para graficacion de small multiples

metricas.predios.ntl.long<-gather(metricas.predios.ntl, 
                                  key = metricas.predios,
                                  value = valores,
                                  casa:LEA.porcentaje)


# small multiple datos predios -----

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.predios.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.predios %in% metricas.predios[1:5]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.predios, nrow = 2)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.predios.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.predios %in% metricas.predios[6:8]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.predios, nrow = 1)+
  tema_lgnd_abajo()


su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.predios.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.predios %in% metricas.predios[9:12]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.predios, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.predios.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.predios %in% metricas.predios[13:15]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.predios, nrow = 1)+
  tema_lgnd_abajo()

#histogramas predios ----

analisis.cali.df %>% select(one_of(metricas.predios)) %>%
  gather( key = metricas.predios,
          value = valores,
          casa:LEA.porcentaje) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~metricas.predios, scales = "free", ncol = 3)
# relacion entre varibles sobre predios -----

ggpairs(
  analisis.cali.df[,metricas.predios], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.predios[1:5]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.predios[6:8]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.predios[9:12]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.predios[13:15]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

# relacion varibles a predicir y metricas predios

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.predios[1:5],
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.predios[6:8],
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.predios[9:15],
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.predios[9:15],
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

presel.predios<-metricas.predios[9:12]

# variables sobre poblacion del censo -----


metricas.poblacion<- c(#"personas_edad", 
                       "edad_promedio",
                       "superior_postgrado",
                       "ningun_estudio", 
                      # "personas_estudio",
                       "con_alguna_limitacion",
                       "sin_limitacion",
                       #"personas_limitacion",
                       "indigena",
                       #"rom",
                       #"raizal_SAI_Providencia",
                       #"palenquero",
                       "negro_mulato_afrocolombiano", 
                       "ninguno_de_los_anteriores"
                       #"no_informa"#,
                       #"personas_etnia"
)

metricas.poblacion.mod <-c("densidad_poblacion", 
                           "afro.porcentaje",
                           #"indigena.porcentaje",
                           #"rom.porcentaje",
                           #"SAI.porcentaje",
                           #"palenquero.porcentaje", 
                           "con_alguna_limitacion.porcentaje",
                           "sin_limitacion.porcentaje",
                           "ningun_estudio.porcentaje",
                           "superior_postgrado.porcentaje",
                           "poblacion.porcentaje",
                           "poblacion_afro.porcentaje" 
                           )

# analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
#   mutate_all(cut_number,7) -> metricas.estruct.qn

analisis.cali.df %>% select(one_of(metricas.poblacion)) %>%
  mutate_all(ntile,10)->metricas.poblacion.ntl

analisis.cali.df %>% select(one_of(metricas.poblacion.mod)) %>%
  mutate_all(ntile,10)->metricas.poblacion.mod.ntl

# metricas.estruct.qn<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.estruct.qn)
metricas.poblacion.ntl<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.poblacion.ntl)
metricas.poblacion.mod.ntl<-analisis.cali.df %>% select(SETU_CCDGO) %>% bind_cols(metricas.poblacion.mod.ntl)

# datos en formato long para graficacion de small multiples

metricas.poblacion.ntl.long<-gather(metricas.poblacion.ntl, 
                                  key = metricas.poblacion,
                                  value = valores,
                                  edad_promedio:ninguno_de_los_anteriores)

metricas.poblacion.mod.ntl.long<-gather(metricas.poblacion.mod.ntl, 
                                    key = metricas.poblacion.mod,
                                    value = valores,
                                    densidad_poblacion:poblacion_afro.porcentaje)


# small multiple datos poblacion -----

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion %in% metricas.poblacion[1:3]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion %in% metricas.poblacion[4:5]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion %in% metricas.poblacion[6:8]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion, nrow = 1)+
  tema_lgnd_abajo()

pl_poblacion<-plots_map_su_df(analisis.cali.df,metricas.poblacion[c(2,3,4,6,7)])
grid.arrange(grobs =pl_poblacion, nrow =2)

# histogramas datos poblacion  
analisis.cali.df %>% select(one_of(metricas.poblacion)) %>%
  gather( key = metricas.poblacion,
          value = valores,
          edad_promedio:ninguno_de_los_anteriores) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="magenta")+
  facet_wrap(~metricas.poblacion, scales = "free", ncol = 3)


# datos porcentuales relativos al sector urbano
su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.mod.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion.mod %in% metricas.poblacion.mod[c(1,7)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion.mod, nrow = 1)+
  tema_lgnd_abajo()


su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.mod.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion.mod %in% metricas.poblacion.mod[c(3,4)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion.mod, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.mod.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion.mod %in% metricas.poblacion.mod[c(5,6)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion.mod, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.poblacion.mod.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.poblacion.mod %in% metricas.poblacion.mod[c(2,8)]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.poblacion.mod, nrow = 1)+
  tema_lgnd_abajo()


# histogramas datos poblacion relativos ----
analisis.cali.df %>% select(one_of(metricas.poblacion.mod)) %>%
  gather( key = metricas.poblacion.mod,
          value = valores,
          densidad_poblacion:poblacion_afro.porcentaje) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="magenta")+
  facet_wrap(~metricas.poblacion.mod, scales = "free", ncol = 3)


ggpairs(
  analisis.cali.df[,metricas.poblacion], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.poblacion.mod], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)


# relacion entre las aribles a predecir y poblacion 

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.poblacion,
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.poblacion,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY =dep.arboles, 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)




# Exploracion de los datos de acceso ----


# indices de acceso a espacios verdes (especios publicos) ----


metricas.acceso<- c( "ia.costoviaje",
                     "ia.costo.n",
                     "ia.1000",
                     "ia.1000.n",
                     "ia.1000.inv",
                     "ia.r300",
                     "ia.mindist",
                     "area_ep",
                     "area_ep.porcentaje",
                     "ia.areas.1000",
                     "ia.areas.1000.porcentaje",
                     "ia.area.mindist",
                     "ia.areas.dist",
                     "ia.A.D",
                     "ia.r300.Amedia" ,
                     "ia.r300.Amediana" )


# analisis.cali.df %>% select(one_of(metricas.estruct)) %>%
#   mutate_all(cut_number,7) -> metricas.estruct.qn

analisis.cali.df %>% select(one_of(metricas.acceso)) %>%
  mutate_all(ntile,10)->metricas.acceso.ntl

metricas.acceso.ntl<-analisis.cali.df %>% 
  select(SETU_CCDGO) %>% 
  bind_cols(metricas.acceso.ntl)

# datos en formato long para graficacion de small multiples

metricas.acceso.ntl.long<-gather(metricas.acceso.ntl, 
                                  key = metricas.acceso,
                                  value = valores,
                                 ia.costoviaje:ia.r300.Amediana)

# mapa de los espacios
ep.cali.f<-fortify( ep.cali,region = "id_ap")
ep.cali.f<- merge(ep.cali.f,ep.cali@data,by.x="id",by.y="id_ap")
s_1000<-gBuffer(centroides.su, byid = T, width = 1000)
s_1000_df<-fortify(s_1000, region = "setu_ccgdo")


# mapa base de manzanas 
base_plot.manzanas2<- ggplot()+
  geom_path(data = manzanas.su.f,
               aes(x = long, y = lat, group = group),
               color="grey80",size=0.2) +coord_equal() + theme_void()

# espacios publicos (verdes)
base_plot.manzanas2 +
  geom_path(data = su.f,aes(x=long,y=lat,group=group),
            color="lightskyblue",
            size=0.5)+
  geom_polygon(data = ep.cali.f ,
               aes(x=long,y=lat,group=group),
               fill="deeppink",
               alpha=0.7)+
                                
  theme_void()


# facets por tipo de espacio 
  ggplot()+
  geom_path(data = su.f,aes(x=long,y=lat,group=group),
            color="grey80",
            size=0.2)+
  geom_polygon(data = ep.cali.f ,
               aes(x=long,y=lat,group=group),
               fill="deeppink",
               alpha=1)+
    coord_equal()+
  theme_void()+
  facet_wrap(~categoria, nrow = 2)


# crar paleta de 10 elementos
palFantasticFox <- colorRampPalette(wes_palette("FantasticFox"))
palDarjeeling <- colorRampPalette(wes_palette("Darjeeling"))
palgen <- colorRampPalette(wes_palettes$Moonrise1)
#ep.cali$categoria %>% unique -> nombreCat
palKata<-c("green3",
           "green4",
           "greenyellow",
           "tan1",
           "sienna1",
           "steelblue4",
           "violetred4",
           "red3",
           "steelblue3",
           "gold1")

base_plot.manzanas2 + 
  geom_polygon(data = ep.cali.f ,
               aes(x=long,y=lat,group=group,
               fill=categoria),
               alpha=1)+
  
  theme_void()+
  scale_fill_manual(values = palKata[c(9,8,7,6,1,5,4,10,2,3)])

# mapa de radios de 1km al desde los centriodes
# espacios publicos (verdes)
ggplot()+
  geom_polygon(data = s_1000_df, 
               aes(x=long,y=lat,group=group),
               fill = "salmon",
               alpha = 0.2)+
  geom_path(data = su.f,aes(x=long,y=lat,group=group),
            color="grey40",
            size=0.4)+
  coord_equal()+
  theme_void()


ggplot()+
  geom_polygon(data = s_1000_df, 
               aes(x=long,y=lat,group=group),
               fill = "salmon",
               alpha = 0.2)+
  geom_path(data = su.f,aes(x=long,y=lat,group=group),
            color="grey40",
            size=0.4)+
  geom_polygon(data = ep.cali.f ,
               aes(x=long,y=lat,group=group),
               fill="royalblue",
               alpha=0.5)+
  coord_equal()+
  theme_void()

# ggsave("./images/mapa-arboles-facet-emplazamiento.png",
#        scale = 1, width = 11, height = 8,
#        dpi = 300)

# small multiple datos predios -----

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.acceso.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.acceso %in% metricas.acceso[1:2]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.acceso, nrow = 1)+
  tema_lgnd_abajo()


su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.acceso.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.acceso %in% metricas.acceso[3:7]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.acceso, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.acceso.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.acceso %in% metricas.acceso[8:11]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.acceso, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.acceso.ntl.long, by = c("id"="SETU_CCDGO")) %>%
  filter(metricas.acceso %in% metricas.acceso[12:16]) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = factor(valores)))+
  coord_equal()+
  scale_fill_viridis( name = "deciles",
                      direction = 1, 
                      discrete = T, 
                      na.value = "grey50",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
  facet_wrap(~metricas.acceso, nrow = 1)+
  tema_lgnd_abajo()



metricas.acceso.df<-analisis.cali.df %>% select(SETU_CCDGO,one_of(metricas.acceso)) 



pl_acceso<-plots_map_su_df(metricas.acceso.df,metricas.acceso)
grid.arrange(grobs =pl_acceso, ncol =4)

pl_acceso_dist<-plots_map_su_df(metricas.acceso.df,metricas.acceso[c(1:7)])
grid.arrange(grobs =pl_acceso_dist, ncol =4)

pl_acceso_area<-plots_map_su_df(metricas.acceso.df,metricas.acceso[c(8:11)])
grid.arrange(grobs =pl_acceso_area, ncol =4)

pl_acceso_mix<-plots_map_su_df(metricas.acceso.df,metricas.acceso[c(12:16)])
grid.arrange(grobs =pl_acceso_mix, ncol =3)



su.f %>% dplyr::select(-area_su)  %>%
  left_join(metricas.acceso.df, by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(aes(x= long, y = lat, group = group, fill = ia.areas.1000.porcentaje))+
  coord_equal()+
  scale_fill_viridis(
    direction = 1, 
    na.value = "grey50"
  )+
  theme_void()
  

# histogramas datos acceso relativos ----
analisis.cali.df %>% select(one_of(metricas.acceso)) %>%
  gather( key = metricas.acceso,
          value = valores,
          ia.costoviaje:ia.r300.Amediana) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="magenta")+
  facet_wrap(~metricas.acceso, scales = "free", ncol = 3)

ggpairs(
  analisis.cali.df[,metricas.acceso], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.acceso[1:7]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.acceso[8:11]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  analisis.cali.df[,metricas.acceso[12:16]], 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)



# relacion con varibles independientes y acceso a ev -----


ggduo(analisis.cali.df,
      columnsY =metricas.acceso, 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso, 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[1:7], 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)


ggduo(analisis.cali.df,
      columnsY = metricas.acceso[8:11], 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[12:16], 
      columnsX =metricas.poblacion.mod,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)



ggduo(analisis.cali.df,
      columnsY = metricas.acceso[1:7], 
      columnsX =metricas.estruct,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[8:11], 
      columnsX =metricas.estruct,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[12:16], 
      columnsX =metricas.estruct,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)



ggduo(analisis.cali.df,
      columnsY = metricas.acceso[1:7], 
      columnsX =metricas.predios,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[8:11], 
      columnsX =metricas.predios,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggduo(analisis.cali.df,
      columnsY = metricas.acceso[12:16], 
      columnsX =metricas.predios,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

