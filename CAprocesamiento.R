

# procesamiento CA20015 y CP2005 ----

# librerias ----

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap") 
#install.packages(x) # warning: this may take a number of minutes 
install.packages("devtools")
library(devtools)
install_github("wilkox/ggfittext")
install_github("wilkox/treemapify")

lapply(x, library, character.only = TRUE) # load the required packages
library(readr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)
library(gridExtra)
library(sp)
library(reshape2)
library(tidyverse)
library(ggfittext)
library(treemapify)
library(raster)


# cargar datos CA ---- 
#eliminar espacios
df_CA <- read.csv("~/Documents/UNIGIS/Tesis/Analisys/CA2015/CAutf8.csv", sep=";")
df_CA <- as.data.frame(lapply(df_CA,function(x) if(is.character(x)|is.factor(x)) trimws(x) else x))

## inspeción  de los diferentes niveles(varibles ordinales y nominales) y varobles continuas ----
summary(df_CA)
summary(df_CA$id)
df_CA$id %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(df_CA$familia) 
summary(df_CA$vitalidad) 
df_CA$vitalidad %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(df_CA$emplazamiento)
df_CA$emplazamiento %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(df_CA$edad) 
df_CA$edad %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(df_CA$cobertura) 
df_CA$cobertura %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()

summary(df_CA$vegetacion) 
df_CA$vegetacion %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()

summary(df_CA$familia) %>% barplot()
summary(df_CA$vitalidad) %>% barplot()
summary(df_CA$emplazamiento) %>% barplot()
summary(df_CA$edad) %>% barplot()
summary(df_CA$cobertura) %>% barplot()
summary(df_CA$vegetacion) %>% barplot()


# Correcion de los diferentes niveles ----

#familias
df_CA$familia<-gsub("Piperáceae" ,"Piperaceae",df_CA$familia)
df_CA$familia<-as.factor(df_CA$familia)
df_CA$familia %>% as.character() %>% trimws()%>% as.factor() ->df_CA$efamilia
summary(df_CA$familia) %>% barplot()
summary(df_CA$familia)%>%sort(decreasing = T) %>% barplot()
summary(df_CA$familia)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#nombre cientifico
df_CA$nombre_cienticico<-gsub("Annona cherimolla" ,"Annona cherimola" ,df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-as.factor(df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-gsub("Artocarpus integrifolius" ,"Artocarpus integrifolia" ,df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-as.factor(df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-gsub("Attalea butyraceae" ,"Attalea butyracea" ,df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-as.factor(df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-as.factor(gsub("Calliandra hematosephala" ,"Calliandra haematocephala" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Calliandra tweedii" ,"Calliandra tweediei" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Citrus lemon" ,"Citrus limon" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Erythroxylon coca" ,"Erythroxylum coca" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Eucalyptus globulus" ,"Eucalyptus globulus" ,df_CA$nombre_cienticico))#caso raro, parece identicos 
df_CA$nombre_cienticico<-as.factor(gsub("Guaicum officinale" ,"Guaiacum officinale" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Guaiacum officinarum" ,"Guaiacum officinale" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-gsub("Miconia chlorocarpa" ,"Miconia chlorocarpa" ,df_CA$nombre_cienticico)#caso raro, parece identicos
df_CA$nombre_cienticico<-as.factor(df_CA$nombre_cienticico)
df_CA$nombre_cienticico<-as.factor(gsub("Miconia Spicellata" ,"Miconia spicellata" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Pereskia bledo" ,"Pereskia bleo" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Phoenix robellina" ,"Phoenix roebelenii" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Platysmicium pinnatum","Platymiscium pinnatum" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico<-as.factor(gsub("Terminalia amazonica","Terminalia amazonia" ,df_CA$nombre_cienticico))
df_CA$nombre_cienticico %>% as.character() %>% trimws()%>% as.factor() ->df_CA$nombre_cienticico
summary(df_CA$nombre_cienticico)%>%sort(decreasing = T) %>% barplot()
summary(df_CA$nombre_cienticico)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#barrios
df_CA$Nombrebarrio<-as.factor(gsub("Alfonso Bonilla Aragon", "Alfonso Bonilla Aragón" ,df_CA$Nombrebarrio))
df_CA$Nombrebarrio<-as.factor(gsub("Rep·blica de Israel", "República de Israel",df_CA$Nombrebarrio))
df_CA$Nombrebarrio<-as.factor(gsub("Sector Alto Jordan", "Sector Alto Jordán",df_CA$Nombrebarrio))
df_CA$Nombrebarrio<-as.factor(gsub("Sector Altos de Normandia - Bataclan", "Sector Altos de Normandia - Bataclán",df_CA$Nombrebarrio))
summary(df_CA$Nombrebarrio) %>% barplot()
df_CA$Nombrebarrio %>% as.character() %>% trimws()%>% as.factor() ->df_CA$Nombrebarrio
summary(df_CA$Nombrebarrio) %>% barplot()
summary(df_CA$Nombrebarrio)%>%sort(decreasing = T) %>% barplot()
summary(df_CA$Nombrebarrio)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#emplazamiento
df_CA$emplazamiento<-as.factor(gsub("Rondas de rios", "Ronda de rios",df_CA$emplazamiento))
df_CA$emplazamiento<-as.factor(gsub("Escenario depor/cult", "Escenario deportivo y/o Cultural",df_CA$emplazamiento))
df_CA$emplazamiento<-as.factor(gsub("Bahia de estacionami", "Bahias de estacionamiento",df_CA$emplazamiento))
df_CA$emplazamiento %>% as.character() %>% trimws()%>% as.factor() ->df_CA$emplazamiento
summary(df_CA$emplazamiento)%>%sort(decreasing = T) %>% barplot()
summary(df_CA$emplazamiento)%>%sort(decreasing = T) %>%  .[1:4] %>%barplot()

#vitalidad
df_CA$vitalidad %>% as.character() %>% trimws()%>% as.factor() ->df_CA$vitalidad
summary(df_CA$vitalidad) %>% barplot()

#edad
df_CA$edad %>% as.character() %>% trimws()%>% as.factor() ->df_CA$edad
df_CA$edad <- factor(df_CA$edad,levels(df_CA$edad)[c(1,3,2)])
summary(df_CA$edad) %>% barplot()

#cobertura
df_CA$cobertura %>% as.character() %>% trimws()%>% as.factor() ->df_CA$cobertura
summary(df_CA$cobertura) %>% barplot()

#vegetacion
df_CA$vegetacion %>% as.character() %>% trimws()%>% as.factor() ->df_CA$vegetacion
summary(df_CA$vegetacion) %>% 
barplot()

#id
#buscar duliplicados
df_CA %>%
  group_by(id) %>% 
  filter(n()>1) #%>% View()
#eliminar duliplicados
df_CA<-df_CA %>%
  distinct(id,.keep_all=TRUE)


# columnas a incluir en analsis ----
#colAnalisys<-c("id","idarbol","grupo","nombre_comun","nombre_cienticico","familia","vegetacion","edad","emplazamiento","Norte","Este","Norte0","Este0","altura_arbol","diametro_copa","vitalidad")
AU_analsis<-df_CA %>% select(id,nombre_cienticico,familia,vegetacion,
                             altura_arbol,diametro_copa,edad,vitalidad,emplazamiento,cobertura,
                             Norte,Este,Norte0,Este0) %>% na.omit()

summary(AU_analsis)

# calculamos la cobertura de copa ----
AU_analsis<-AU_analsis %>% rowwise()%>%
  mutate(area_copa= pi*(diametro_copa/2)^2)


# inspecionar CA graficamente sin agregaciones ----
AU_analsis %>% ggplot()+
#  geom_boxplot(aes(x=edad,y=area_copa))+ coord_flip() +
  geom_boxplot(aes(x=edad,y=area_copa,color=cobertura))+ coord_flip() 
  
AU_analsis %>% ggplot()+
  geom_violin(aes(x=emplazamiento,y=diametro_copa/2,color=emplazamiento))

AU_analsis %>% ggplot()+
  geom_violin(aes(x=edad,y=diametro_copa/2,color=edad))

  
  #geom_boxplot(aes(x=edad,y=area_copa,color=cobertura))+ coord_flip() 
# puede ser bueno calcular porcentaje de arboles longevos por sector urbano
# mirar altura versus copa por varibles categoricas
AU_analsis %>% filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
   geom_point(aes(x=altura_arbol,y=diametro_copa/2,color=vegetacion),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  coord_flip() +
  facet_grid( cobertura ~ emplazamiento )
  #geom_boxplot(aes(x=edad,y=area_copa,color=cobertura))+ coord_flip() 

# agregacion estadistica de variables CA ----
altura_copa_por_vegetacion<-AU_analsis %>% 
group_by(vegetacion) %>% 
summarise(altura_media_vegetacion=mean(altura_arbol),
          diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_cobertura<-AU_analsis %>% 
  group_by(cobertura) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_emplazamiento<-AU_analsis %>% 
  group_by(emplazamiento) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_edad<-AU_analsis %>% 
  group_by(edad) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

# Graficar agragaciones CA

p_por_vegetacion<-ggplot(altura_copa_por_vegetacion,
                         aes(x=diametro_medio_copa_vegetacion,
                             y=altura_media_vegetacion)) 
p_por_cobertura<-ggplot(altura_copa_por_cobertura,
                         aes(x=diametro_medio_copa_vegetacion,
                             y=altura_media_vegetacion)) 

p_por_emplazamiento<-ggplot(altura_copa_por_emplazamiento,
                         aes(x=diametro_medio_copa_vegetacion,
                             y=altura_media_vegetacion)) 

p_por_edad<-ggplot(altura_copa_por_edad,
                            aes(x=diametro_medio_copa_vegetacion,
                                y=altura_media_vegetacion)) 


#cantidad de arboles por varible nominal
p_por_vegetacion+
  geom_point(aes(size=cantidad,color=vegetacion ) )+ 
  scale_color_brewer(palette = "Dark2")+
  scale_size_area(max_size = 10)

p_por_cobertura+
  geom_point(aes(size=cantidad,color=cobertura ) )+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size_area(max_size = 10)


p_por_emplazamiento+
  geom_point(aes(size=cantidad,color=emplazamiento ) )+ 
#  scale_color_brewer(palette = "Paired")+
  scale_size_area(max_size = 10)



p_por_edad+
  geom_point(aes(size=cantidad,color=edad ) )+ 
  scale_color_brewer(palette = "Dark2")+
  scale_size_area(max_size = 10)


# exploracion variables continuas CA ----
p_copa<-ggplot(AU_analsis, aes(x = diametro_copa)) 
p_altura<-ggplot(AU_analsis, aes(x = altura_arbol)) 


p_copa + geom_histogram(aes(fill = vegetacion), 
                        color = "white", 
                        alpha = 0.4,
                        binwidth = 1,
                        position="identity")+  
  geom_vline(data = altura_copa_por_vegetacion,
             aes(xintercept = diametro_medio_copa_vegetacion,
                 color=vegetacion),
             linetype="dashed") + scale_fill_brewer(palette = "Dark2")+scale_color_brewer(palette = "Dark2")

p_altura + geom_histogram(aes(fill = vegetacion), 
                          color = "white", 
                          alpha = 0.4,
                          binwidth = 1,
                          position="identity")+  
  geom_vline(data = altura_copa_por_vegetacion, 
             aes(xintercept = altura_media_vegetacion,
                 color=vegetacion),
             linetype="dashed") + scale_fill_brewer(palette = "Dark2")



# criterios de seleccion individuos CA ----
#independiente de su vitaidad actual, podemos pensar que hace 10 años 
#no estaban ni enfermos ni secos, ni muertos, asi que los usaremos todos.
#selvitalidad<-c("Regular","Sano")
#Se excluyen las plantas arbustivas y los arbustos, pues interesa individuos de mayor talla
#que 2 metros que provean sombra y sean un beneficio en andes, donde se ubica la mayor cantidad
#de individuos
selvegetacion<-c("Arbol", "Palma" ,"Bambu","Muerto","Seco")
#para reducir la brecha de tiempo entre los datos del CP2005 y los datos del 2015 del CA 
#solo se tomaran en cuanta arboles Maduros y longevos.
seledad<-c("Longevo","Maduro")

#filtrar por criterios
AU_analsis<-filter(AU_analsis, vegetacion %in% selvegetacion) %>%
filter(edad %in% seledad) %>%
  filter(altura_arbol > 2) %>%
  filter(diametro_copa >1.5)

nrow(AU_analsis)

# Inspeccion individuos CA seleccionados ----

AU_analsis %>% 
  #filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
  geom_point(aes(x=altura_arbol,y=diametro_copa/2,color=vegetacion),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  coord_flip() +
  facet_grid( cobertura ~ emplazamiento )

AU_analsis %>% 
  #filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
  geom_point(aes(x=altura_arbol,y=diametro_copa/2,color=cobertura),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  coord_flip() +
  facet_grid( . ~ emplazamiento )


# 
# cargar objetos geograficos ----- 
#listar shapefiles  
list.files("~/Documents/UNIGIS/Tesis/Analisys/shapefiles", pattern="\\.shp$")
file.exists("~/Documents/UNIGIS/Tesis/Analisys/shapefiles/mc_sectorUrbano_Cali.shp")
file.exists("~/Documents/UNIGIS/Tesis/Analisys/shapefiles/mc_manzanas.shp")
file.exists("~/Documents/UNIGIS/Tesis/Analisys/shapefiles/mc_perimetro_idesc.shp")
su<-readOGR(dsn =path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
            layer = "mc_sectorUrbano_Cali")
manzanas<-readOGR(dsn =path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                  layer = "mc_manzanas")
prmtr_urbn_idesc <-readOGR(dsn =path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                           layer = "mc_perimetro_idesc")
espacio_publico_idesc<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                               layer = "mc_espacio_publico_idesc")
 
espacio_publico_EEC<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                           layer = "mc_espacio_publico_EEC")

equipamento_EEC<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                               layer = "mc_equipamientos_colectivos_seleccionados_EEC")

corredores_ambientales<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                             layer = "mc_corredores_ambientales")

humedales<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
                             layer = "mc_humedales")




# inspeccion shapefiles cargados ----
su
summary(su)
names(su)
#calcular area del sector urbano su
su$area_su <- area(su)

su.f<-fortify(su,region = "SETU_CCDGO")
su.f<-su@data%>%
  dplyr::select(SETU_CCDGO,area_su)%>%
  merge(su.f,.,by.x="id",by.y="SETU_CCDGO")

ids_su<-su$SETU_CCDGO
centroids.df<-as.data.frame(coordinates(su))
names(centroids.df) <- c("long", "lat") 
su.setu_ccdgo<-data.frame(ids_su,centroids.df)

ggplot(su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="lightgrey",color="white")+coord_equal()+
  theme_void()+
  with(su.setu_ccdgo, annotate(geom="text", x = long, y=lat, label = ids_su, size = 1.8,color="black")) 
#+ggfittext::geom_fit_text(data = su.setu_ccdgo, aes(label=ids_su) )
  


manzanas
summary(manzanas)
names(manzanas)

prmtr_urbn_idesc
summary(prmtr_urbn_idesc)
names(prmtr_urbn_idesc)



espacio_publico_idesc
summary(espacio_publico_idesc)
names(espacio_publico_idesc)
proj4string(espacio_publico_idesc)

espacio_publico_EEC
summary(espacio_publico_EEC)
names(espacio_publico_EEC)
proj4string(espacio_publico_EEC)

equipamento_EEC
summary(equipamento_EEC)
names(equipamento_EEC)
proj4string(equipamento_EEC)

corredores_ambientales
summary(corredores_ambientales)
names(corredores_ambientales)
proj4string(corredores_ambientales)

humedales
summary(humedales)
names(humedales)
proj4string(humedales)
# CRS Idesc----
crs_mc_idesc<-proj4string(manzanas)
crs_mc_idesc

# creamos la capa de puntos con los arboles seleccionados ----
coords_arboles <- SpatialPoints(AU_analsis[, c("Este", "Norte")])
AU_analsis_spatial <- SpatialPointsDataFrame(coords_arboles, AU_analsis)
proj4string(AU_analsis_spatial) <- crs_mc_idesc
identical(proj4string(su),proj4string(AU_analsis_spatial))

# operaciones espaciales ----
#sectores censales en el perimetro urbano ----
su_prmtr<-su[prmtr_urbn_idesc,]
nrow(su)
nrow(su_prmtr)
plot(su_prmtr) #todos los sectores urbanos se encuentran en el perimetro


#arboles en sectores urbanos ----
inside.su <- !is.na(over(AU_analsis_spatial,as(su,"SpatialPolygons")))
#asiganr sector urbano a cada arbol y manzana dentro del sector
AU_analsis_spatial$setu_ccnct<-over(AU_analsis_spatial,su)$SETU_CCNCT
names(AU_analsis_spatial)
AU_analsis_spatial$setu_ccnct
# manznas en sectores urbanos ----
manzanas$setu_ccnct<-over(manzanas,su)$SETU_CCNCT

#carateristicas fisicas de las manzanas ----

# espacios verdes (EV) publicos (EVP) y manzanas ----

# agregrar altura, cantidad y area de copa por SU ----

AU_stats_por_su<-AU_analsis_spatial@data %>%
  group_by(setu_ccnct) %>%
  dplyr::summarise(area_copa_su=sum(area_copa),
            altura_media_su=mean(altura_arbol),
            diametro_medio_copa_su = mean(diametro_copa),
            cantidad=n())

# join stats y datos CA20015 sectores de espacial SU ----
ca2015su.cali<- left_join(su@data,AU_stats_por_su,by=c("SETU_CCNCT"="setu_ccnct")) %>%
  dplyr::select(SETU_CCNCT,SETU_CCDGO,area_su:cantidad)
ca2015su.cali$cobertura_copa_su<-ca2015su.cali$area_copa_su/su$area_su
ca2015su.cali$arboles_su_area<-ca2015su.cali$area_copa_su/su$area_su
# escalar varibles en [0,1] para los mapas ----
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

ca2015su.cali$area_su_01<-range01(ca2015su.cali$area_su)
ca2015su.cali$cobertura_copa_su_01<-range01(ca2015su.cali$cobertura_copa_su,na.rm = T)
ca2015su.cali$cantidad_01<-range01(ca2015su.cali$cantidad,na.rm = T)
ca2015su.cali$altura_media_su_01<-range01(ca2015su.cali$altura_media_su,na.rm = T)
ca2015su.cali$diametro_medio_copa_su_01<-range01(ca2015su.cali$diametro_medio_copa_su,na.rm = T)
ca2015su.cali$area_copa_su_01<-range01(ca2015su.cali$area_copa_su,na.rm = T)
ca2015su.cali$arboles_su_area_01<-range01(ca2015su.cali$arboles_su_area,na.rm = T)


#datos en formato long ----
ca2015su.cali.long<-ca2015su.cali %>% 
  dplyr::select(SETU_CCNCT,SETU_CCDGO,area_su_01:arboles_su_area_01)%>%
  melt(
                      # ID variables - all the variables to keep but not split apart on
    id.vars=c("SETU_CCNCT", "SETU_CCDGO"),
    variable.name="ca2015_var",
    value.name="ca2015_valor"
    )



# data_long <- melt(olddata_wide,
#         # ID variables - all the variables to keep but not split apart on
#     id.vars=c("subject", "sex"),
#         # The source columns
#     measure.vars=c("control", "cond1", "cond2" ),
#         # Name of the destination column that will identify the original
#         # column that the measurement came from
#     variable.name="condition",
#     value.name="measurement"
# )
#graficar distribuciones varibles CA ----

ca2015su.cali.long %>% 
  filter(ca2015_var=="cobertura_copa_su_01")%>%
  ggplot()+
#  geom_histogram(aes(x="ca2015_valor"),)
  geom_histogram(aes(x=ca2015_valor),fill=viridis(80),color="white",alpha=0.9,bins = 80)
#  geom_histogram(aes(x=ca2015_valor),fill=viridis(),color="white",alpha=0.9,bins = 80)


ca2015su.cali.long %>% 
#  filter(ca2015_var=="cobertura_copa_su")%>%
  ggplot()+
#  geom_histogram(aes(x="ca2015_valor"),)
  geom_histogram(aes(x=ca2015_valor,fill=..count..),alpha=0.9,bins = 80)+
  # scale_fill_viridis()+
  facet_wrap(~ca2015_var,scales = "free")
  

ca2015su.cali.long %>% 
  #filter(ca2015_var!="area_su",ca2015_var!="area_su")%>%
  ggplot()+
#  geom_histogram(aes(x="ca2015_valor"),)
#  geom_histogram(aes(x=ca2015_valor,fill=ca2015_var),alpha=0.9,color="white",bins = 80)+
 # scale_fill_viridis(discrete = T)+
  geom_histogram(aes(x=ca2015_valor),alpha=0.9,color="white",bins = 80)+
  facet_wrap(~ca2015_var,scales = "free",ncol = 1)
  
  
  color_hist<-cut_interval(ca2015su.cali.long$ca2015_valor,80,labels = FALSE) %>% as.numeric()
  
  ca2015su.cali.long %>% bind_cols(data.frame(color_hist)) %>%
  #filter(ca2015_var!="area_su",ca2015_var!="area_su")%>%
  ggplot()+
#  geom_histogram(aes(x="ca2015_valor"),)
#  geom_histogram(aes(x=ca2015_valor,fill=ca2015_var),alpha=0.9,color="white",bins = 80)+
 # scale_fill_viridis(discrete = T)+
  geom_histogram(aes(x=ca2015_valor,fill=as.factor(color_hist)), alpha=0.9,bins = 80)+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  facet_wrap(~ca2015_var,scales = "free",ncol = 1)
  
  #facet_grid(ca2015_var~.,scales = "free")




# graficar mapas para cada varible CA ----
su.ca2015<-su
names(su.ca2015)



# 2nd convertir a dataframe para usar ggplot  ----
su.ca2015.f<-fortify(su.ca2015,region = "SETU_CCDGO")
su.ca2015.f <- merge(su.ca2015.f, ca2015su.cali, by.x = "id", by.y = "SETU_CCDGO")

su.ca2015.f.long<-fortify(su.ca2015,region = "SETU_CCDGO")
su.ca2015.f.long <- merge(su.ca2015.f.long, ca2015su.cali.long, by.x = "id", by.y = "SETU_CCDGO")



#graficar cvarobles ambietales del CA2015 ----
p_su_ca2015.facet<- ggplot() +
  geom_polygon(data=su.ca2015.f.long,
               aes(x=long,y=lat,group=group,fill=ca2015_valor))+
    coord_equal() + scale_fill_viridis()+
    theme_void()+
  facet_wrap(~ca2015_var,ncol = 4)
p_su_ca2015.facet

p_su_area_copa<- ggplot() +
  geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=area_copa_su))+
  coord_equal() + scale_fill_viridis()

p_su_diametro_medio<- ggplot() +
  geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=diametro_medio_copa_su))+
  coord_equal() + scale_fill_viridis(option="magma")+
  geom_polygon(data=manzanas.f,aes(x=long,y=lat,group=group),fill="lightgrey",alpha=0.7)

p_su_altura_media<- ggplot() +
  geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=altura_media_su))+
  coord_equal() + scale_fill_viridis(option="magma")
  

p_su_num_indarb<- ggplot() +
  geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=cantidad_su))+
  coord_equal() + scale_fill_viridis(option="magma")

# graficas distribucion CA ----
p_su_hist_cobertura<-ggplot()+geom_histogram(data=AU_analsis_spatial@data,aes(x=area_copa))

#geom_histogram(aes(color = sex), fill = "white", alpha = 0.6, position="identity")

grid.arrange(p_su_cobertura, p_su_area_copa, p_su_altura_media, p_su_num_indarb, ncol = 2, nrow =2)



# Carga datos CP2005 ----
# datos personas 
CP2005_t_persona_edad <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/CP2005 - t_persona_edad.csv",
col_types = cols(edad_promedio = col_number(),
personas = col_number(), su_id = col_character()))

CP2005_t_nivel_estudios <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/CP2005 - t_nivel_estudios.csv",
col_types = cols(ningun_estudio = col_number(),
su_id = col_character(), superior_postgrado = col_number(),
total_personas = col_number()))

Cp2005_t_limitacion <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/Cp2005 - t_limitacion.csv",
col_types = cols(NO = col_number(), SI = col_number(),
su_id = col_character(), total_personas = col_number()))

CP2005_t_etnia <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/CP2005 - t_etnia.csv",
col_types = cols(indigena = col_number(),
negro_mulato_afrocolombiano = col_number(),
ninguno_de_los_anteriores = col_number(),
no_informa = col_number(), palenquero = col_number(),
raizal_SAI_Providencia = col_number(),
rom = col_number(), su_id = col_character(),
total_personas = col_number()))

#datos de vivienda por SU
CP2005_t_tipo_vivienda <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/CP2005 - t_tipo_vivienda.csv",
col_types = cols(apartamento = col_number(),
casa = col_number(), casa_indigena = col_number(),
otro_tipo_de_vivienda = col_number(),
su_id = col_character(), tipo_cuarto = col_number(),
total_viviendas = col_number()))

CP2005_t_uso_predios <- read_csv("~/Documents/UNIGIS/Tesis/Analisys/CP2005/CP2005 - t_uso_predios.csv",
col_types = cols(su_id = col_character(),
total_viviendas = col_number(), uso_LEA = col_number(),
uso_unidad_economica = col_number(),
uso_vivienda = col_number()))

CP2005_t_ocupacion_viviendas <- read_csv("./CP2005/CP2005 - t_ocupacion_viviendas.csv",
col_types = cols(desocupada_por_uso_temporal = col_number(),
desocupadas = col_number(), ocupada_con_personas_ausentes = col_number(),
ocupada_con_personas_presentes = col_number(),
su_id = col_character(), total_viviendas = col_number()))

# consolidar datos CP2005 en un solo data frame ----
#datos_personas 
cp2005_personas_su<-CP2005_t_persona_edad %>%
  full_join(CP2005_t_nivel_estudios,by="su_id") %>% rename(personas_edad=personas,
                                                           personas_estudio=total_personas) %>%
  full_join(Cp2005_t_limitacion,by="su_id") %>% rename(con_alguna_limitacion=SI,
                                                       sin_limitacion=NO,
                                                       personas_limitacion=total_personas) %>%
  full_join(CP2005_t_etnia,by="su_id") %>%
  rename(personas_etnia=total_personas) %>%
  mutate(SETU_CCNCT = trimws(su_id))
#buscar duplicados
cp2005_personas_su%>%
  group_by(SETU_CCNCT) %>% 
  filter(n()>1) 
#datos viviendas
cp2005_viviendas_su<-CP2005_t_tipo_vivienda%>%
  full_join(CP2005_t_uso_predios,by="su_id") %>% rename(viviendas_tipo=total_viviendas.x,
                                                        viviendas_uso=total_viviendas.y) %>%
  full_join(CP2005_t_ocupacion_viviendas, by = "su_id") %>% 
  rename(viviendas_ocupacion=total_viviendas) %>% 
  mutate(SETU_CCNCT = trimws(su_id))
  
cp2005_personas_su$SETU_CCNCT %in% as.character(su$SETU_CCNCT)
cp2005_viviendas_su$SETU_CCNCT %in% as.character(su$SETU_CCNCT)

#los datos CP2005 DANE con codigos de diferentes longitud ---- 
# remover dos digitos extras que parecen el codigo de comuna, pues coinciden.
#extraemos los SU de los datos del CP2005 del Redatam que coinciden 
#con el codigo de depratamento y de ciudad. 76001 es el codigo de cali
cpSubset_personas <- cp2005_personas_su[grep("76001", cp2005_personas_su$SETU_CCNCT), ]
cpSubset_viviendas <- cp2005_viviendas_su[grep("76001", cp2005_viviendas_su$SETU_CCNCT), ]

#union personas vivienda
cpSubset<-full_join(cpSubset_viviendas,cpSubset_personas,by="su_id") %>% 
  rename(SETU_CCNCT=SETU_CCNCT.x) %>%
  select(-one_of(c("SETU_CCNCT.y")))
  

#buscar duplicados
cpSubset%>%
  group_by(SETU_CCNCT) %>% 
  filter(n()>1) 
cpSubset$SETU_CCNCT %>% nchar()  
#760011 (03) 000000000309
#760011000000000309
as.character(su$SETU_CCNCT) %>% nchar()

#al parecer lo dos digitos extra son de la comuna pues los 4 digitos del SU inician, 
# con excepciones con el mismo numero de lod dos digitos extras.
#axaminamos el SETU_CCNCT
comuna_ids<-cpSubset$SETU_CCNCT %>% substr(., 7, 8)
su_ids<-cpSubset$SETU_CCNCT %>% substr(., 17, 20)
comuna_ids_substr<-su_ids%>% substr(., 1, 2)
c1<-cpSubset$SETU_CCNCT %>% substr(., 1, 6)
c2<-cpSubset$SETU_CCNCT %>% substr(., 9, 20)
setu_ccnct_18<-paste0(c1,c2)
setu_ccnct_20<-cpSubset$SETU_CCNCT
cod_subset<-data.frame(setu_ccnct_20,setu_ccnct_18,comuna_ids,comuna_ids_substr,su_ids)
cod_subset<-cod_subset %>%
rowwise()%>%
  mutate(cod_consistencia=
           if_else(as.character(comuna_ids)==as.character(comuna_ids_substr) | as.character(comuna_ids_substr)=="99",
         "consistente",
         "no-consistente"))

cod_subset$setu_ccnct_18 %in% as.character(su$SETU_CCNCT)
#buscar duplicados
cod_subset%>%
  group_by(setu_ccnct_18) %>% 
  filter(n()>1) 

as.character(cod_subset$setu_ccnct_18) %>% nchar()  

is.na(cod_subset$setu_ccnct_18)
is.na(cpSubset$su_id)
# grafiquemos los datos para ver las diferencias entre su repetidos
#primer los odenamos con base en una de las varibles
cpSubset$su_id <- factor(cpSubset$su_id, levels = cpSubset$su_id[order(cpSubset$viviendas_tipo)])
#cpSubset %>%
  cpSubset%>%
#  group_by(SETU_CCNCT) %>% 
#  filter(n()>1)   
#  arrange(desc(personas_edad)) %>%
#  mutate(su_id_factor=as.factor(su_id)) %>% 
ggplot()+geom_bar(aes(x=su_id,y=viviendas_tipo),stat = "identity")

    
nrow(cod_subset)  
nrow(cpSubset)
as.character(cod_subset$setu_ccnct_20) %in% as.character(cpSubset$su_id)
#añadimos el los codigos modificados a personas_su 
cp2005su.cali<-inner_join(cpSubset,cod_subset,by=c("su_id"="setu_ccnct_20"))
#buscar duplicados
cp2005su.cali%>%
  group_by(setu_ccnct_18) %>% 
  filter(n()>1) %>% arrange(setu_ccnct_18) %>% 
  filter(cod_consistencia=="no-consistente")%>%
  select(su_id)->cp2005_elim #eliminar los duplicados no consistentes
#verificar si hay NA en los datos
is.na(cp2005su.cali) #%>% View()
#verificar los sectores no consistentes
elim_su_cp2005<-as.character(cp2005su.cali$su_id) %in% as.character(cp2005_elim$su_id)  
cp2005su.cali[elim_su_cp2005,]# %>% View()

#verificar si hay datos por SU validos sin valores
cp2005su.cali %>%
  filter(is.na(personas_edad),
         is.na(personas_estudio),
         is.na(personas_etnia),
         is.na(personas_limitacion),
         is.na(viviendas_tipo),
         is.na(viviendas_uso),
         is.na(viviendas_ocupacion)) ->  cp2005su.cali.all.na
cp2005su.cali.all.na
cp2005su.cali %>%
  filter(is.na(personas_edad) |
         is.na(personas_estudio) |
         is.na(personas_etnia) |
         is.na(personas_limitacion) |
         is.na(viviendas_tipo) |
         is.na(viviendas_uso) |
         is.na(viviendas_ocupacion)) ->  cp2005su.cali.any.na

cp2005su.cali.any.na

#los NA de los datos restantes son SU que no tuvo casos de respuesta positiva,
#por ejemplo un SU solo comercial, sin personas habitadando. En estos casos es posible reeemplzar por 0 los NA
cp2005su.cali[is.na(cp2005su.cali)] <-0


#is.na(cp2005su.cali[!elim_su_cp2005,])
cp2005su.cali<-cp2005su.cali[!elim_su_cp2005,]
# su_eliminados_cod<-cp2005su.cali%>% filter(cod_consistencia =="no-consistente")
# cp2005su.cali<-cp2005su.cali%>% filter(cod_consistencia !="no-consistente")



#calcular las varibles porcetual respectos de la poblacion, y respecto del area.----
total_poblacion<-sum(cp2005su.calil$personas_edad,na.rm = T)

#calcular area de sector censal


# Transformacion de las variables persona CP2005 a porcentaje de personas ----
total_poblacion<-sum(cp2005su.calil$personas_edad,na.rm = T)
cp2005su.cali<-cp2005su.cali%>%
  mutate(porcentaje_superior_postgrado=superior_postgrado/personas_estudio,
         porcentaje_ningun_estudio=ningun_estudio/personas_estudio,
         porcentaje_sin_limitacion=sin_limitacion/personas_limitacion,
         porcentaje_con_alguna_limitacion=con_alguna_limitacion/personas_limitacion,
         porcentaje_indigenas=indigena/personas_etnia,
         porcentaje_rom=rom/personas_etnia,
         porcentaje_raizal_SAIyP=raizal_SAI_Providencia/personas_etnia,
         porcentaje_palenquero=palenquero/personas_etnia,
         porcentaje_afro=negro_mulato_afrocolombiano/personas_etnia,
         porcentaje_ninguna_etnia=ninguno_de_los_anteriores/personas_etnia,
         porcentaje_personas_su=personas_edad/total_poblacion)
cp2005su.cali_sel$su_ids %>% levels()


# unir los datos del CP2005 con los datos de la capa geografica  ----
# usando el codigo de 18 digitos
cp2005su.cali_sel<-left_join(su@data,cp2005su.cali,by= c("SETU_CCNCT"="setu_ccnct_18"))
nrow(cp2005su.cali_sel)

#verificamos que no hay datos con algun NA
cp2005su.cali_sel %>%
  filter(is.na(personas_edad) |
           is.na(personas_estudio) |
           is.na(personas_etnia) |
           is.na(personas_limitacion) |
           is.na(viviendas_tipo) |
           is.na(viviendas_uso) |
           is.na(viviendas_ocupacion))  ->  cp2005su.cali_sel.na

cp2005su.cali_sel.na
#se puede ver que existe un SU en la capa geofrafica que no tuvo datos en las consultas del Redatam.

#convertimos los datos preprocesados en un objeto geografico.
# su.cp2005.na<-su
# su.cp2005.na@data<-inner_join(su.cp2005.na@data, cp2005su.cali_sel.na,
su.cp2005<-su
su.cp2005@data<-cp2005su.cali_sel


# 

AU_analsis_spatial
summary(AU_analsis_spatial)
names(AU_analsis_spatial)

# verificar extension de los datos
bbox(AU_analsis_spatial) # the extent, 'bounding box' of stations 
bbox(su.cp2005)


# creacion de data frames para plotear los datos en ggplot2 ----
su.cp2005.f<-fortify(su.cp2005,region = "SETU_CCNCT")
su.cp2005.f <- merge(su.cp2005.f, su.cp2005@data, by.x = "id", by.y = "SETU_CCNCT")
prmtr_urbn_idesc.f<-fortify(prmtr_urbn_idesc,region = "nombre")
manzanas.f<-fortify(manzanas)

# plot(su)
# plot(prmtr_urbn_idesc)
# plot(manzanas)

# p_perimetro<-ggplot()+
#   geom_polygon(data=prmtr_urbn_idesc.f,aes(x=long,y=lat,group=group), fill=NA, colour="red")+
#   coord_equal()
# p_perimetro

# Exploración de variables del CP2005 ----
#los sectores sin datos sobre personas son el Club Campestre, Cosmocentro,
# La Plaza de Toros-Complejo Deportivo Del Coliseo el Pueblo, y la !4 de Calima 

## Graficas  de cantidad de personas por SU ----
p_su.cp2005<- ggplot(data=su.cp2005.f,aes(x=long,y=lat,group=group))
p_su.cp2005.personas<- ggplot() +
  geom_polygon(data=su.cp2005.f,aes(x=long,y=lat,group=group,fill=personas_edad))+
  theme_void()+
  coord_equal()+scale_fill_viridis(option = "magma")+
  labs(title="Personas por SU",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005")

#primer los ordenamos con base en una de la variable de interes
cp2005su.cali_sel$su_ids <- factor(cp2005su.cali_sel$su_ids, levels = cp2005su.cali_sel$su_ids[order(cp2005su.cali_sel$personas_edad)])
cp2005su.cali_sel$su_ids %>% levels()

p_cp2005<-ggplot(data=cp2005su.cali_sel,aes(x=su_ids))

p_cp2005.personas.bar<-p_cp2005+
  geom_segment(aes(y=personas_edad,yend=0,xend=su_ids,color=personas_edad),size=0.8) +
  #geom_point(aes(y=personas_edad,color=personas_edad),size=1)+
#  geom_histogram(aes(y=personas_edad,fill=personas_edad),stat = "identity")+
#  geom_text(aes(y=personas_edad+10, label=su_ids, hjust= 0 ), size=2)+
  theme_void()+
  coord_flip()+
  scale_color_viridis(option = "magma")+
  labs(title="Personas por SU",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005")
  
p_cp2005.personas.bar

p_cp2005<-ggplot(data=cp2005su.cali_sel,aes(x=personas_edad))
p_cp2005.personas.hist<-p_cp2005+
  #geom_segment(aes(y=personas_edad,yend=0,xend=su_ids,color=personas_edad),size=0.8) +
  #geom_point(aes(y=personas_edad,color=personas_edad),size=1)+
    geom_histogram(aes(x=personas_edad),fill=magma(80),color="white",alpha=0.9,bins = 80)+
  #  geom_text(aes(y=personas_edad+10, label=su_ids, hjust= 0 ), size=2)+
  theme_minimal()+
#  coord_flip()+
  labs(title="Histograma Personas en SU",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005",
       x="Personas")

p_cp2005.personas.hist

# Grid Personas ----
grid.arrange(p_su.cp2005.personas, p_cp2005.personas.bar, p_cp2005.personas.hist, layout_matrix = rbind(c(1,2),c(1,3)))


# Graficar casos personas para cada variable

p_cp2005<-ggplot(data=cp2005su.cali_sel,aes(x=su_ids))
p_cp2005.personas.comp<-p_cp2005+
  theme_minimal()+
  geom_point(aes(y=personas_edad),color="red", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=personas_limitacion),color="blue", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=personas_estudio),color="green", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=personas_etnia),color="orange", stat = "identity",size=0.8,alpha=0.5)+
  theme(axis.text.x = element_blank())+
  labs(title="Personas en SU por consulta a variable",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005",
       x="SU",y="Personas")+
  geom_point(x=10, y=22000, color="red") + 
  geom_point(x=10, y=21000, color="blue") + 
  geom_point(x=10, y=20000, color="green") + 
  geom_point(x=10, y=19000, color="orange") + 
  annotate("text", x=20, y=22000, label="edad", color="black") +
  annotate("text", x=26, y=21000, label="limitación", color="black")+
  annotate("text", x=24, y=20000, label="estudios", color="black") +
  annotate("text", x=20, y=19000, label="etnia", color="black")




# Graficacion datos porcentuales personas seleccionados ----
p_cp2005.personas.comp<-ggplot(data=cp2005su.cali_sel, aes(x=reorder(su_ids, 
  #                                                                   porcentaje_superior_postgrado)))+
 # porcentaje_sin_limitacion)))+
  # porcentaje_afro)))+
   porcentaje_ningun_estudio)))+
#  geom_line(aes(y=porcentaje_personas_su,group=su_ids))+
  theme_minimal()+
  #geom_point(aes(y=porcentaje_personas_su),color="black",stat = "",size=0.8)+
  geom_point(aes(y=porcentaje_superior_postgrado),color="red", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=porcentaje_sin_limitacion),color="blue", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=porcentaje_afro),color="green", stat = "identity",size=0.8,alpha=0.5)+
  geom_point(aes(y=porcentaje_ningun_estudio),color="orange", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_superior_postgrado,group=1),color="red", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_sin_limitacion,group=1),color="blue", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_afro,group=1),color="green", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_ningun_estudio,group=1),color="orange", stat = "identity",size=0.8,alpha=0.5)+
  theme(axis.text.x = element_blank())+
  labs(title="Variables en porcentaje de persona en cada SU",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005",
       x="SU",y="Porcentaje de personas")+
  geom_point(x=10, y=0.82, color="red" )+
  geom_point(x=10, y=0.78, color="blue" )+
  geom_point(x=10, y=0.74, color="green" )+
  geom_point(x=10, y=0.7, color="orange" )+
  annotate("text", x=40, y=0.82, label="Estudios Superiores", color="black") +
  annotate("text", x=32, y=0.78, label="Sin limitación", color="black")+
  annotate("text", x=20, y=0.74, label="Afro", color="black") +
  annotate("text", x=34, y=0.7, label="Ningun estudio", color="black")


p_cp2005.personas.comp

# Variables porcentuales por SU en relacion a la poblacion total ----
p_cp2005.personas.comp.ciudad<-ggplot(data=cp2005su.cali_sel,aes(x=su_ids))+
  #  geom_line(aes(y=porcentaje_personas_su,group=su_ids))+
  theme_minimal()+
  #geom_point(aes(y=porcentaje_personas_su),color="black",stat = "identity",size=0.8)+
  geom_line(aes(y=porcentaje_personas_su,group=1),color="black",stat = "identity",size=0.8)+
  #geom_point(aes(y=porcentaje_superior_postgrado*porcentaje_personas_su),color="red", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_superior_postgrado*porcentaje_personas_su,group=1),color="red", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_sin_limitacion*porcentaje_personas_su,group=1),color="blue", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_afro*porcentaje_personas_su,group=1),color="green", stat = "identity",size=0.8,alpha=0.5)+
  geom_line(aes(y=porcentaje_ningun_estudio*porcentaje_personas_su,group=1),color="orange", stat = "identity",size=0.8,alpha=0.5)+
  theme(axis.text.x = element_blank())+
  labs(title="Variables porcentuales por SU en relacion a la poblacion total",
       subtitle="Santiago de Cali",
       caption="Fuente: DANE, Censo de Población 2005",
       x="SU",y="Porcentaje de personas")
p_cp2005.personas.comp.ciudad


# facets del CP2005 porcentual (small multiples).
p_cp2005.personas.xy
su.cp2005.long<-su
cp2005su.cali_sel
cp2005su.prcentaje.facet<-cp2005su.cali_sel
p_su.cp2005.all<- ggplot(data=su.cp2005.f,aes(x=long,y=lat,group=group,fill=value))
 p_su.cp2005.all +
  geom_polygon(aes(x=long,y=lat,group=group))+
  theme_minimal()+
  coord_equal()+
  scale_fill_viridis(option = "magma")+
  facet_wrap(~variable, scales = "free_y") 
# +
#   labs(title="Personas por SU",
#        subtitle="Santiago de Cali",
#        caption="Fuente: DANE, Censo de Población 2005")

#facet_wrap(~variable)
###################
# Normalidad y colinialidad





############
p_su.cp2005.edad_prom<- ggplot() +
  geom_polygon(data=su.cp2005.f,aes(x=long,y=lat,group=group,fill=edad_promedio))+
  coord_equal()+scale_fill_viridis(option = "magma")+
  theme_void()
p_su.cp2005.edad_prom

p_su.cp2005.viviendas<- ggplot() +
  geom_polygon(data=su.cp2005.f,aes(x=long,y=lat,group=group,fill=viviendas_tipo))+
  theme_void()+
  coord_equal()+scale_fill_viridis(option = "magma")
p_su.cp2005.viviendas


p_manzanas<- ggplot() +
  geom_polygon(data=manzanas.f,aes(x=long,y=lat,group=group))+
  coord_equal()


p_AU<- ggplot(data = AU_analsis,aes(x=Este,y = Norte )) 

p_AU+ geom_point(aes(size=diametro_copa,color =cobertura, alpha = 1/100))+
  scale_color_brewer(palette = "Dark2",type = "div") +
  scale_size_area(max_size = 0.001)+
  coord_equal()

ggplot() +
  geom_polygon(data=su.f,aes(x=long,y=lat,group=group),fill=NA, colour="blue")+
  geom_polygon(data=prmtr_urbn_idesc.f,aes(x=long,y=lat,group=group),fill=NA, colour="red")+
  geom_point(data = AU_analsis,aes(x=Este,y = Norte,size=diametro_copa,color =cobertura),alpha=0.2)+
  scale_color_brewer(palette = "Dark2",type = "div") +
  scale_size_area(max_size = 0.001)+
  coord_equal()


p_AU+ geom_point(aes(size=diametro_copa,color =emplazamiento))+
  scale_color_brewer(palette = "Paired") +
  scale_size_area(max_size = 0.001)+
  coord_equal()



  





# anexar codigo del Sector censal urbanos (SU) en formato texto

#cargar datos censales por SU del CP2005

# barriosShp<-readShapePoly("GIS/capas/barrios.shp")
# barriosCentriodes<-readShapePoints("GIS/capas/centriodesBarrios.shp")
# summary(barriosShp)
# str(barriosShp)
# plot(barriosShp)
# identificacionBarrios<-as.data.frame(barriosShp[,c("id_barrio","barrio","comuna","estra_moda","area","sbarrios_i","sbarrios_f")])
# identificacionBarrios$sbarrios_i<-as.numeric(as.character(identificacionBarrios$sbarrios_i))
# 
# arboles_analisis$cod_barrio<-identificacionBarrios$id_barrio[match(arboles_analisis$idBarrio, identificacionBarrios$sbarrios_i)]
# arboles_analisis$nombre_barrio<-identificacionBarrios$barrio[match(arboles_analisis$idBarrio, identificacionBarrios$sbarrios_i)]
# 
# #agregar datos por barrio
# barrio_area_total_copa<-aggregate(arboles_analisis$area_copa,by=list(cod_barrio=arboles_analisis$cod_barrio),sum)
# colnames(barrio_area_total_copa)[2]<-"area_total_copa"
# barrio_area_media_copa<-aggregate(arboles_analisis$area_copa,by=list(cod_barrio=arboles_analisis$cod_barrio),mean)
# colnames(barrio_area_media_copa)[2]<-"area_media_copa"
# barrio_altura_media_arboles<-aggregate(arboles_analisis$altura_arbol,by=list(cod_barrio=arboles_analisis$cod_barrio),mean)
# colnames(barrio_altura_media_arboles)[2]<-"altura_media_arbol"
# barrio_total_arboles<-aggregate(arboles_analisis$id,by=list(cod_barrio=arboles_analisis$cod_barrio),length)
# colnames(barrio_total_arboles)[2]<-"total_arboles"
# 
# #agregar por especie 
# especie_area_total_copa<-aggregate(arboles_analisis$area_copa,by=list(especie=arboles_analisis$nombre_cienticico),sum)
# colnames(especie_area_total_copa)[2]<-"area_total_copa"
# especie_area_media_copa<-aggregate(arboles_analisis$area_copa,by=list(especie=arboles_analisis$nombre_cienticico),mean)
# colnames(especie_area_media_copa)[2]<-"area_media_copa"
# especie_diametro_medio_copa<-aggregate(arboles_analisis$diametro_copa,by=list(especie=arboles_analisis$nombre_cienticico),mean)
# colnames(especie_diametro_medio_copa)[2]<-"diametro_medio_copa"
# especie_altura_media<-aggregate(arboles_analisis$altura_arbol,by=list(especie=arboles_analisis$nombre_cienticico),mean)
# colnames(especie_altura_media)[2]<-"altura_media"
# especie_total_arboles<-aggregate(arboles_analisis$id,by=list(especie=arboles_analisis$nombre_cienticico),length)
# colnames(especie_total_arboles)[2]<-"total_arboles"
# 
# 
# #agregar datos por barrio por especie
# barrio_especie_area_total_copa<-aggregate(arboles_analisis$area_copa,by=list(cod_barrio=arboles_analisis$cod_barrio,especie=arboles_analisis$nombre_cienticico),sum)
# colnames(barrio_especie_area_total_copa)[3]<-"area_total_copa"
# barrio_especie_area_media_copa<-aggregate(arboles_analisis$area_copa,by=list(cod_barrio=arboles_analisis$cod_barrio,especie=arboles_analisis$nombre_cienticico),mean)
# colnames(barrio_especie_area_media_copa)[3]<-"area_media_copa"
# barrio_especie_altura_media<-aggregate(arboles_analisis$altura_arbol,by=list(cod_barrio=arboles_analisis$cod_barrio,especie=arboles_analisis$nombre_cienticico),mean)
# colnames(barrio_especie_altura_media)[3]<-"altura_media"
# barrio_especie_total_arboles<-aggregate(arboles_analisis$id,by=list(cod_barrio=arboles_analisis$cod_barrio,especie=arboles_analisis$nombre_cienticico),length)
# colnames(barrio_especie_total_arboles)[3]<-"total_arboles"
# #consilidar resultados
# #reorganizar los niveles del estrato moda
# identificacionBarrios$estra_moda<-factor(identificacionBarrios$estra_moda,levels = c("1","2","3","4","5","6","10"))
# #barrio
# stats_barrio<-merge(barrio_altura_media_arboles,barrio_area_media_copa,by="cod_barrio")
# stats_barrio<-merge(stats_barrio,barrio_area_total_copa,by="cod_barrio")
# stats_barrio<-merge(stats_barrio,barrio_total_arboles,by="cod_barrio")
# barrio_info_stats<-merge(identificacionBarrios,stats_barrio,by.x="id_barrio", by.y="cod_barrio")
# #especie
# stats_especie<-merge(especie_altura_media,especie_area_media_copa,by="especie")
# stats_especie<-merge(stats_especie,especie_area_total_copa,by="especie")
# stats_especie<-merge(stats_especie,especie_total_arboles,by="especie")
# stats_especie<-merge(stats_especie,especie_diametro_medio_copa,by="especie")
# 
# #calcular porcentaje de cobertura de copa
# stats_especie$cobertura_copa<-stats_especie$area_total_copa/sum(barrio_info_stats$area)
# #barrio_especie
# stats_barrio_especie<-merge(barrio_especie_altura_media,barrio_especie_area_media_copa,by=c("cod_barrio","especie"))
# stats_barrio_especie<-merge(stats_barrio_especie,barrio_especie_area_total_copa,by=c("cod_barrio","especie"))
# stats_barrio_especie<-merge(stats_barrio_especie,barrio_especie_total_arboles,by=c("cod_barrio","especie"))
# 
# #calcular porcentaje de cobertura de copa por barrio
# barrio_info_stats$area<- as.numeric(as.character(barrio_info_stats$area))
# barrio_info_stats$estra_moda<-factor(barrio_info_stats$estra_moda,levels = c("1","2","3","4","5","6","10"))
# #Aplicar analisis 1
# #Exploracion de datos
# qplot(y=altura_media_arbol,x=area_media_copa,data=barrio_info_stats ,asp = 1, colour=estra_moda)
# qplot(y=altura_media_arbol,x=area_total_copa,data=barrio_info_stats , colour=estra_moda, size=area_media_copa)
# qplot(x=altura_media_arbol,y=cobertura_copa,data=barrio_info_stats , colour=estra_moda, size=area_media_copa)
# qplot(x=estrato_moda,y=area_media_copa,data = barrio_info_stats,geom = "boxplot")
# qplot(x=estra_moda,y=cobertura_copa,data = barrio_info_stats,geom = "boxplot")
# qplot(y=total_arboles,x=cobertura_copa,data=barrio_info_stats , colour=estra_moda, size=area_media_copa)
# qplot(x=estrato_moda,y=total_arboles,data = barrio_info_stats,geom = "boxplot")
# qplot(x=estra_moda,y=total_arboles,data = barrio_info_stats,geom = "boxplot")
# 
# 
# #Moran ́s I.
# library(ape)
# barrio_info_stats_centroid<-merge(x=barrio_info_stats,y=barriosCentriodes,by.x="id_barrio",by.y="UID")
# #shape file de los barrios
# barrio_info_stats_poly<-merge(y=barrio_info_stats,x=barriosShp@data,by="id_barrio")
# 
# dist <- as.matrix(dist(cbind(barrio_info_stats_centroid$MEAN_X,barrio_info_stats_centroid$MEAN_Y)))
# dist.inv <- 1/dist
# diag(dist.inv)<-0
# 
# moran_area_total_copa<-Moran.I(barrio_info_stats_centroid$area_total_copa,dist.inv)
# moran_altura_media<-Moran.I(barrio_info_stats_centroid$altura_media_arbol,dist.inv)
# moran_cobertura_copa<-Moran.I(barrio_info_stats_centroid$cobertura_copa,dist.inv)
# # #reorganizar los niveles del estrato moda(se hizo a identificacionBarrios)
# # barrio_info_stats_centroid$estra_moda<-factor(barrio_info_stats_centroid$estra_moda,levels = c("1","2","3","4","5","6","10"))
# estrato_moda<-as.numeric(as.character(barrio_info_stats$estra_moda))
# moran_estrato_moda<-Moran.I(estrato_moda,dist.inv)
# 
# #plotear los datos en el mapa
# barriosShpStats<- na.omit(barrio_info_stats_poly@data)
# plot(barriosShpStats,col=gray(barriosShpStats$cobertura_copa/max(barriosShpStats$cobertura_copa)))
# plot(barrio_info_stats_poly)
# #Ripley ́s K statistic.
# library(spatstat)
# pppBarriosC <-as(barriosCentriodes,"ppp")
# K<-Kest(pppBarriosC,correction="best")
# plot(K)
# plot(envelope(pppBarriosC, Kest))
# 
# #aplicar analsis 2
# stats_especie_scaled<-scale(stats_especie[,2:7])
# especiesCluster<-kmeans(stats_especie_scaled[,c(1,6)],20,nstart = 20)
# arbolesCluster<-as.factor(especiesCluster$cluster)
# stats_especie$cluster<-arbolesCluster
# p<-ggplot(data=stats_especie,aes(x=diametro_medio_copa,y=altura_media,size=total_arboles, asp = 1, colour=cluster))
# p<-p+geom_point()
# especiesCluster


# #guardar resultados en archivos
# write.csv(df_CA, file = "df_CA.csv",sep = "|",fileEncoding = "UTF-8")
# write.csv(arboles_analisis, file = "arboles_analisis.csv" ,fileEncoding = "UTF-8")

