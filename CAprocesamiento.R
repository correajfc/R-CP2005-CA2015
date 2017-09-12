

# procesamiento CA20015 y CP2005 ----

# librerias ----

libs <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap") 
#install.packages(x) # warning: this may take a number of minutes 
install.packages("devtools")
library(devtools)
install_github("wilkox/ggfittext")
install_github("wilkox/treemapify")

lapply(libs, library, character.only = TRUE) # load the required packages
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
library(outliers)
library(MASS)



# cargar datos CA ---- 
#eliminar espacios
df_CA <- read.csv("./CA2015/CAutf8.csv", sep=";")
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
AU_analsis<-df_CA %>% dplyr::select(id,nombre_cienticico,familia,vegetacion,
                             altura_arbol,diametro_copa,edad,vitalidad,emplazamiento,cobertura,
                             Norte,Este,Norte0,Este0) %>% na.omit()

summary(AU_analsis)

rm(df_CA)

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
  coord_flip() +theme_void()+
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

# corredores_ambientales<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
#                              layer = "mc_corredores_ambientales")
# 
# humedales<-readOGR(dsn = path.expand("~/Documents/UNIGIS/Tesis/Analisys/shapefiles"),
#                              layer = "mc_humedales")


espacio_publico_idesc <- gBuffer(espacio_publico_idesc, byid=TRUE, width=0)
espacio_publico_EEC <- gBuffer(espacio_publico_EEC, byid=TRUE, width=0)
equipamento_EEC <- gBuffer(equipamento_EEC, byid=TRUE, width=0)
manzanas<-gBuffer(manzanas, byid=TRUE, width=0)
su<-gBuffer(su, byid=TRUE, width=0)

# inspeccion shapefiles cargados ----
su
summary(su)
names(su)
#eliminar campos sin interes
su@data<-dplyr::select(su@data,SETU_CCDGO,SETU_CCNCT)
#calcular area del sector urbano su

su$area_su <- raster::area(su)

#data para ggplot
su.f<-fortify(su,region = "SETU_CCDGO")
su.f<-su@data%>%
  dplyr::select(SETU_CCDGO,area_su)%>%
  merge(su.f,.,by.x="id",by.y="SETU_CCDGO")

#calcular capa con centroides y etiquetas de cada SU
ids_su<-su$SETU_CCDGO
centroids.df<-as.data.frame(coordinates(su))
names(centroids.df) <- c("long", "lat") 
su.setu_ccdgo<-data.frame(ids_su,centroids.df)

#graficar Su con etiquetas.
ggplot(su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="lightgrey",color="white")+coord_equal()+
  theme_void()+
  with(su.setu_ccdgo, annotate(geom="text", x = long, y=lat, label = ids_su, 
                               size = 1.5,color="black")) +
  labs(title="Sectores Urbanos del Censo del 2005 ",
       subtitle="Los sectores seleccionados están parcial o totalmente contenidos en el perímetro urbano 2015",
       caption="Fuente: Cartografia Censo 2005 - DANE, perímetro urbano - IDESC")
#+ggfittext::geom_fit_text(data = su.setu_ccdgo, aes(label=ids_su) )
  

# inspeccion resumen ----
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
espacio_publico_idesc@data %>% head()


espacio_publico_EEC
summary(espacio_publico_EEC)
names(espacio_publico_EEC)
proj4string(espacio_publico_EEC)

equipamento_EEC
summary(equipamento_EEC)
names(equipamento_EEC)
proj4string(equipamento_EEC)

# corredores_ambientales
# summary(corredores_ambientales)
# names(corredores_ambientales)
# proj4string(corredores_ambientales)
# 
# humedales@data
# summary(humedales)
# names(humedales)
# proj4string(humedales)

# CRS Idesc ----
crs_mc_idesc<-proj4string(manzanas)
crs_mc_idesc

# creamos la capa de puntos con los arboles seleccionados ----
coords_arboles <- SpatialPoints(AU_analsis[, c("Este", "Norte")])
AU_analsis_spatial <- SpatialPointsDataFrame(coords_arboles, AU_analsis)
proj4string(AU_analsis_spatial) <- crs_mc_idesc
identical(proj4string(su),proj4string(AU_analsis_spatial))




# operaciones espaciales ----


#sectores censales en el perimetro urbano ----
su<-su[prmtr_urbn_idesc,]
nrow(su)
#plot(su) #todos los sectores urbanos se encuentran en el perimetro



#arboles en sectores urbanos ----
inside.su <- !is.na(over(AU_analsis_spatial,as(su,"SpatialPolygons")))
inside.su
#asiganr sector urbano a cada arbol y manzana dentro del sector
AU_analsis_spatial$setu_ccnct<-over(AU_analsis_spatial,su)$SETU_CCNCT
names(AU_analsis_spatial)
#eliminamos arboles fuera de los sectores censales
AU_analsis_spatial<-AU_analsis_spatial[!is.na(AU_analsis_spatial$setu_ccnct),]




# manznas en sectores urbanos ----
#algunas manzanas no estan totalemente contenidas los sectores.
#para facilitr el calculo las recortmos con la capa de su y se asigna, 
#pero este calculo resulma muy costozo, entonces seleccionamos solo las manzanas que toca
#los poligonos de  area verde o espacio público

#manz.temp<-manzanas
#su.crop.manz.temp<-intersect(su,manz.temp)

#selecionamos las manzanas que se tocan con un SU
manzanas$setu_ccnct<-over(manzanas,su)$SETU_CCNCT
manzanas@data<-left_join(manzanas@data,su@data[,1:2],by=c("setu_ccnct"="SETU_CCNCT")) 

manzanas<-manzanas[!is.na(manzanas$setu_ccnct),]
manzanas@data
manzanas$id_manzana<-rownames(manzanas@data)
manzanas[,c("SETU_CCDGO", "id_manzana")]

#partimos
manzanas.su <-intersect(manzanas[,c("SETU_CCDGO", "id_manzana")],su)
manzanas.su<-gBuffer(manzanas.su, byid=TRUE, width=0)

#crear y asignar un id para las manzanas
id_manzana<-rownames(manzanas.su@data)
manzanas.su$area_manzana<-raster::area(manzanas.su)
manzanas.su$id_manzana<-id_manzana
manzanas.su$SETU_CCDGO.1!=manzanas.su$SETU_CCDGO.2
summary(manzanas.su)
#eliminar los campos que no son necesarios
manzanas.su<-manzanas.su[,c("id_manzana","SETU_CCDGO.2","area_manzana")]
names(manzanas.su)[names(manzanas.su)=="SETU_CCDGO.2"] <- "SETU_CCDGO"

summary(manzanas.su)
#fortify manzanas
manzanas.su.f<-fortify(manzanas.su,region = "id_manzana")
manzanas.su.f<-manzanas.su@data%>%
  #dplyr::select(id_manzana,area_manzana)%>%
  merge(manzanas.su.f,.,by.x="id",by.y="id_manzana")
#carateristicas fisicas de las manzanas ----
p_manzanas_area<-ggplot(manzanas.su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=area_manzana))+coord_equal()+
  theme_void()+
  scale_fill_viridis(direction = -1)
p_manzanas_area

ggplot(manzanas.su@data,aes(x=area_manzana))+
  geom_histogram(bins = 80,fill=rev(viridis(80)))
#para graficar excluiremos la manzana de la fuerza aerea Marco Fidel Suarez,
#no mermite ver la varibildad

#QQ plot manzanas menores que 100000 m2----
ggplot(manzanas.su@data[manzanas$area_manzana<100000,],aes(sample=area_manzana))+
  stat_qq()

# manzanas.atipicas<-manzanas@data[manzanas$area_manzana>80000,]
# nrow(manzanas.atipicas)
# nrow(manzanas)
# manzanas$area_manzana.log10<-log10(manzanas$area_manzana)
# manzanas$area_manzana.log10.zcore<-base::scale(manzanas$area_manzana.log10) %>% as.vector()

ggplot(manzanas.su@data[manzanas.su$area_manzana<100000,],aes(x=area_manzana))+
  geom_histogram(bins = 80,fill=rev(viridis(80)))

manzanas.su.f%>%
filter(area_manzana<100000)%>%
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill="lightgrey")+
         geom_polygon(aes(fill=area_manzana))+coord_equal()+
         theme_void()+
         scale_fill_viridis(direction = -1)+
  labs(title ="Area de manzanas menores que 100000 m2")



#usar quantile para que los outliers no dominen la escala de color ----
# manzanas.f %>% head()
# qn<-quantile(manzanas.f$area_manzana,seq(0.1, 0.9, 0.1))
# qn.01<-rescale(c(qn, range(manzanas.f$area_manzana)))

#pintar usando colres por quantil
manzanas.su.f%>%
  ggplot(aes(x=long,y=lat,group=group))+
  #geom_polygon(data=manzanas.f ,aes(x=long,y=lat,group=group),fill="lightgrey")+
  geom_polygon(aes(fill=cut_number(area_manzana,n = 20)))+coord_equal()+
  theme_void()+
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)+
  labs(title="Coropleta del tamaño de manzana en Santiago de Cali, Colombia",
       subtitle="20 grupos con aprox. el mismo número de observaciones",
       caption="Fuente: IDESC \n eleborado: @correajfc")
# scale_fill_gradientn (
#       colours = rev(viridis(256, option = "D")),
#       values = c(0, seq(qn.01[1], qn.01[9], length.out = 10), 1))




# espacios verdes (EV) publicos (EVP) y manzanas ----
#equipamento ECC ----
equipamento_EEC %>% head()
equipamento_EEC$CATEGORIA
equipamento_EEC@data
equipamento_EEC<-equipamento_EEC[su,]


names(equipamento_EEC)[names(equipamento_EEC)=="NOMBRE"] <- "nombre"
names(equipamento_EEC)[names(equipamento_EEC)=="CATEGORIA"] <- "categoria"
#eliminar el Colegio Berchmans y Escuela de Carabineros por ser de acceso público
#equipamento_EEC$nombre[su,]<-factor(as.character(equipamento_EEC$nombre))

equipamento_EEC<-equipamento_EEC[!(equipamento_EEC$nombre %in% c("Colegio Berchmans","Escuela de Carabineros")),]
equipamento_EEC$id_ap<-1:nrow(equipamento_EEC)



#area publica (la calle y sus parques)

equipamento_EEC<-equipamento_EEC[,c("id_ap","nombre","categoria")]
names(equipamento_EEC)
proj4string(equipamento_EEC)
summary(equipamento_EEC)
#equipamento_EEC$area.eq.ecc<-raster::area(equipamento_EEC)



eq.ecc.f<-fortify( equipamento_EEC,region = "id_ap")
eq.ecc.f<- merge(eq.ecc.f,equipamento_EEC@data,by.x="id",by.y="id_ap")

ext<-matrix(c(-100,-100,100,100),2)
zum<-bbox(equipamento_EEC@polygons[[1]])
zum<-zum+ext


ggplot(eq.ecc.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.f ,aes(x=long,y=lat,group=group),fill="lightgrey")+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.5)+
         geom_polygon(aes(fill=as.factor(id)),alpha=0.3)+
        coord_equal()+
         theme_void() #+  coord_cartesian(xlim = zum[1,],ylim = zum[2,])



intersect(espacio_publico_idesc,equipamento_EEC)
intersect(espacio_publico_EEC,equipamento_EEC)
#equipamento_EEC no se intersecta con ninguno

# espacio publico de la EEC
espacio_publico_EEC<-espacio_publico_EEC[su,]
espacio_publico_EEC$ID_EP
espacio_publico_EEC%>% head()
names(espacio_publico_EEC)
summary(espacio_publico_EEC)
espacio_publico_EEC
espacio_publico_EEC

names(espacio_publico_EEC)[names(espacio_publico_EEC)=="CATEGORIA"] <- "categoria"
espacio_publico_EEC$nombre<-paste(espacio_publico_EEC$ID_EP,espacio_publico_EEC$AMBITO,sep = "-")
espacio_publico_EEC$id_ap<-(nrow(equipamento_EEC)+1):(nrow(equipamento_EEC)+nrow(espacio_publico_EEC))
espacio_publico_EEC<-espacio_publico_EEC[,c("id_ap","nombre","categoria")]



ep.ecc.f<-fortify( espacio_publico_EEC,region = "id_ap")
ep.ecc.f<- merge(ep.ecc.f,espacio_publico_EEC@data,by.x="id",by.y="id_ap")


ext<-matrix(c(-100,-100,100,100),2)
zum<-bbox(espacio_publico_EEC@polygons[[4]])
zum<-zum+ext

ggplot(eq.ecc.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.f ,aes(x=long,y=lat,group=group),fill="lightgrey")+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.5)+
  geom_polygon(fill="blue",alpha=0.3)+
  geom_polygon(data = ep.ecc.f,aes(x=long,y=lat,group=group),fill="red",alpha=0.3)+
  coord_equal()+
  theme_void() #+  coord_cartesian(xlim = zum[1,],ylim = zum[2,])

#espacio publico idesc
espacio_publico_idesc
espacio_publico_idesc<-espacio_publico_idesc[su,]
espacio_publico_idesc %>% head()
names(espacio_publico_idesc)
summary(espacio_publico_idesc)
espacio_publico_idesc
espacio_publico_idesc$categoria
espacio_publico_idesc %>% nrow()

espacio_publico_idesc@data[is.na(espacio_publico_idesc$categoria),]


espacio_publico_idesc$id_ap<-rownames(espacio_publico_idesc@data)
espacio_publico_idesc<-espacio_publico_idesc[,c("id_ap","nombre","categoria")]


espacio_publico_idesc

#capa total de espacio ----
intersect(espacio_publico_idesc,equipamento_EEC)
inter.epeec.idesc<-intersect(espacio_publico_idesc,espacio_publico_EEC)
inter.epeec.idesc

names(inter.epeec.idesc)[names(inter.epeec.idesc)=="id_ap.1"] <- "id_ap"
names(inter.epeec.idesc)[names(inter.epeec.idesc)=="nombre.1"] <- "nombre"
names(inter.epeec.idesc)[names(inter.epeec.idesc)=="categoria.1"] <- "categoria"
inter.epeec.idesc<-inter.epeec.idesc[,c("id_ap","nombre","categoria")]

dif.epeec.idesc<-espacio_publico_EEC - espacio_publico_idesc
dif.epeec.idesc

ep.cali<-union(espacio_publico_idesc,equipamento_EEC)
ep.cali<-union(ep.cali,dif.epeec.idesc)

names(ep.cali)
ep.cali[is.na(ep.cali$id_ap),]


ep.cali@data[,c("nombre.1","nombre.2")]
ep.cali@data[,c("categoria.1","categoria.2")]
ep.cali@data[,c("id_ap.1","id_ap.2")]

 names(ep.cali)[names(ep.cali)=="id_ap.1"] <- "id_ap"
names(ep.cali)[names(ep.cali)=="nombre.1"] <- "nombre"
 names(ep.cali)[names(ep.cali)=="categoria.1"] <- "categoria"


ep.cali$id_ap<-rownames(ep.cali@data)
ep.cali<-ep.cali[,c("id_ap","nombre","categoria")]

# ep.idesc.f<-fortify( espacio_publico_idesc,region = "id_ap")
# ep.idesc.f<- merge(ep.idesc.f,espacio_publico_idesc@data,by.x="id",by.y="id_ap")
# 
# 
# ext<-matrix(c(-100,-100,100,100),2)
# zum<-bbox(ep.cali@polygons[[50]])
# zum<-zum+ext
# 
# 
# ggplot(eq.ecc.f,aes(x=long,y=lat,group=group))+
#   geom_polygon(data=manzanas.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
#   geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.5)+
#   geom_polygon(fill="blue",alpha=0.3)+
#   geom_polygon(data = ep.ecc.f,aes(x=long,y=lat,group=group),fill="green",alpha=0.3)+
#   geom_polygon(data = ep.idesc.f,aes(x=long,y=lat,group=group),fill="red",alpha=0.3)+
#   coord_equal()+
#   theme_void() #+  coord_cartesian(xlim = zum[1,],ylim = zum[2,])


ep.cali.f<-fortify( ep.cali,region = "id_ap")
ep.cali.f<- merge(ep.cali.f,ep.cali@data,by.x="id",by.y="id_ap")

# ext<-matrix(c(-300,-300,300,300),2)
# 
# punto<-matrix(rep(c(1057658.9287038001 ,866161.98958216363),2),2,2)
# zum2<-punto+ext

ep.cali.f$categoria %>%unique()
ggplot(ep.cali.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.6)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="tomato",alpha=0.5)+
  coord_equal()+
  theme_void() #+coord_fixed(xlim = zum[1,],ylim = zum[2,])


ep.cali.f$categoria %>%unique()
ggplot(ep.cali.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.6)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=categoria),alpha=0.5)+
  coord_equal()+
  theme_void() #+coord_fixed(xlim = zum[1,],ylim = zum[2,])

ggplot(ep.cali.f,aes(x=long,y=lat,group=group))+
  #geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.1)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="red",alpha=0.5)+
  coord_equal()+
  theme_void()+
  facet_wrap(~categoria)#+coord_fixed(xlim = zum[1,],ylim = zum[2,])

# refinamos la capa el espacio publico
#clip del espacio publico por sector

ep.cali.su<-intersect(ep.cali,su)
ep.cali.su$id_ap<-rownames(ep.cali.su@data)

nrow(ep.cali.su)
summary(ep.cali.su)
ep.cali.su<-ep.cali.su[,c("id_ap","nombre","categoria","SETU_CCDGO")]
ep.cali.su$area_ep<-raster::area(ep.cali.su)




#clip de EP por manzanas
ep.cali.manzanas<-intersect(manzanas.su,ep.cali)

ep.cali.manzanas<-gBuffer(ep.cali.manzanas, byid=TRUE, width=0)
ep.cali.manzanas$id_ap.m<-rownames(ep.cali.manzanas@data)
ep.cali.manzanas<-ep.cali.manzanas[,c("SETU_CCDGO","id_manzana","id_ap","id_ap.m","nombre","categoria")]
ep.cali.manzanas$area_ep.manzana<-raster::area(ep.cali.manzanas)
summary(ep.cali.manzanas)
#ver resultado
ep.cali.manzanas.f<-fortify( ep.cali.manzanas,region = "id_ap.m")
ep.cali.manzanas.f<- merge(ep.cali.manzanas.f,ep.cali.manzanas@data,by.x="id",by.y="id_ap.m")

ext<-matrix(c(-100,-100,100,100),2)
nrow(ep.cali)
epi<-sample(1:nrow(ep.cali),size = 1)
bbi<-bbox(ep.cali@polygons[[epi]])
zum<-bbi+ext

ggplot(ep.cali.manzanas.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
  geom_polygon(data=ep.cali.f ,aes(x=long,y=lat,group=group),fill="black",alpha=0.6)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.5)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=categoria))+
  coord_equal()+
  theme_void()+coord_fixed(xlim = zum[1,],ylim = zum[2,])

manzanas.su


# ep.cali.manzanas<-ep.cali.manzanas[,c("id_ap",
#                                       "nombre",
#                                       "categoria",
#                                       "setu_ccnct",
#                                       "SETU_CCDGO",
#                                       "id_manzana",
#                                       "id_ap",
#                                       "nombre","area_ep.manzana")]

ev.cat<-ep.cali.su$categoria %>% levels()%>% .[c(1,4,6,7,9)]
ev.cali.su<-ep.cali.su[ep.cali.su$categoria %in% ev.cat,]
names(ev.cali.su)[names(ev.cali.su)=="area_ep"] <- "area_ev"



gIsValid(ep.cali.su, reason = T)
gIsValid(ep.cali.manzanas, reason = T)
gIsValid(manzanas, reason = T)

# calculos de indices de acceso a EV ----
ep.cali$area_ep<-raster::area(ep.cali)
ep.cali

ev.cali.su
ev.cali<-ep.cali[ep.cali$categoria %in% ev.cat,]
ev.cali$categoria
names(ev.cali)[names(ev.cali)=="area_ep"] <- "area_ev"

centroides.su<-gCentroid(su, byid = T)
centroides.su$setu_ccgdo<-over(centroides.su,su)$SETU_CCDGO
centroides.su$setu_ccgdo
plot(centroides.su)
# centroides.ep<-gCentroid(ep.cali, byid = T)
# centroides.ep$setu_ccgdo<-over(centroides.ep,su)$SETU_CCDGO
# centroides.ep$setu_ccgdo
# plot(centroides.ep)
# centroides.ev<-gCentroid(ev.cali, byid = T)
# plot(centroides.ev,add=T,col="red")
# centroides.ev$setu_ccgdo<-over(centroides.ev,su)$SETU_CCDGO
# centroides.ev$setu_ccgdo

m.dist.ctrdsu.ep<-gDistance(ev.cali,centroides.su, byid = T)
is.1000.ep<-gWithinDistance(ev.cali,centroides.su,1001, byid = T)
a<-m.dist.ctrdsu.ep*is.1000.ep
dim(m.dist.ctrdsu.ep)
dim(a)

ia.mindist<-apply(m.dist.ctrdsu.ep,1,function(x)  min(x[x!=0]))
ia.costoviaje<-apply(m.dist.ctrdsu.ep,1,sum)
ia.1000<-apply(a,1,function (x) sum(x))

m.dist.ctrdsu.1000.ep.inv<-1/a
b<-m.dist.ctrdsu.1000.ep.inv*is.finite(m.dist.ctrdsu.1000.ep.inv)
b[2,2]
#Matrix::rowSums(m.dist.ctrdsu.1000.ep.inv*is.finite(m.dist.ctrdsu.1000.ep.inv),na.rm=TRUE)
ia.1000.inv<-apply(b,1,function (x) sum(x,na.rm = T))
class(ia.costoviaje)
summary(ia.costoviaje)
length(ia.costoviaje)
summary(ia.1000.inv)

ia.ev<-data.frame(su$SETU_CCDGO,ia.costoviaje)
ia.ev$ia.costo.n<-ia.ev$ia.costoviaje/dim(m.dist.ctrdsu.ep)[2]
ia.ev<-bind_cols(ia.ev,data.frame(ia.1000,ia.1000.inv))
ia.ev$ia.r300<-300*ia.1000.inv
ia.ev<-ia.ev%>%dplyr::rename(SETU_CCDGO=su.SETU_CCDGO)
ia.ev$ia.mindist<-ia.mindist

summary(ia.ev)

ggplot(ia.ev[ia.ev$ia.r300<500,],aes(x=ia.r300,fill=cut_interval(ia.r300,40)))+
  geom_histogram(bins = 40)+scale_fill_viridis(discrete = T)

ia.ev.su<-su

ia.ev.su@data<-left_join(ia.ev.su@data,ia.ev,by=c("SETU_CCDGO"="SETU_CCDGO"))

ia.ev.su.f<-fortify( ia.ev.su,region = "SETU_CCDGO")
ia.ev.su.f<- merge(ia.ev.su.f,ia.ev.su@data,by.x="id",by.y="SETU_CCDGO")

ggplot(ia.ev.su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=cut_interval(ia.mindist,40)))+
  coord_equal() + scale_fill_viridis(discrete = T)+
  theme_void()


# centroides.manzanas<-gCentroid(manzanas.su, byid = T)
# plot(centroides.ev,col="lightblue")
# agregados de manzanas por SU: area calle, area manzanas privadas y publicas ----
manzanas.stats.su<-manzanas.su@data%>%
  group_by(SETU_CCDGO) %>%
  dplyr::summarise(area_manzanas=sum(area_manzana),
                   area_media_manzana=mean(area_manzana),
                   num_manzanas=n())
ep.cali.stats.su<-ep.cali.su@data%>%
  group_by(SETU_CCDGO) %>%
  dplyr::summarise(area_ep=sum(area_ep),
                   area_media_ep=mean(area_ep),
                   num_ep=n())
ep.cali.manzanas.stats.su<-ep.cali.manzanas@data%>%
  group_by(SETU_CCDGO) %>%
  dplyr::summarise(area_manzanas.ep=sum(area_ep.manzana),
                   area_media_manzana.ep=mean(area_ep.manzana),
                   num_manzanas.ep=n())

ev.cali.stats.su<-ev.cali.su@data%>%
  group_by(SETU_CCDGO) %>%
  dplyr::summarise(area_ev=sum(area_ev),
                   area_media_ev=mean(area_ev),
                   num_ev=n())

#join stats manzanas, areas publicas y espacio verdes con SU ----
#data frame
estructura.cali<-full_join(manzanas.stats.su,ep.cali.stats.su, by="SETU_CCDGO") %>%
  full_join(ev.cali.stats.su, by="SETU_CCDGO") %>%
  full_join(ep.cali.manzanas.stats.su, by="SETU_CCDGO") %>% 
  full_join(ia.ev, by="SETU_CCDGO") %>% 
  full_join(su@data,by="SETU_CCDGO")

is.na(estructura.cali)
estructura.cali[is.na(estructura.cali)] <-0 # los sectores con NA, sin datos
summary(estructura.cali)

#calulamos los indicadores con base en las variables de estructura de los SU e indeces de ep y ev
#area de la calle es todo lo que no es una manzana dentro del SU
estructura.cali$area_calle<-estructura.cali$area_su-estructura.cali$area_manzanas
estructura.cali[estructura.cali$area_calle<0,]
estructura.cali[estructura.cali$area_calle==0,]

#area publica es el area de la calle más las secciones de manzanas que son espacio publico.
estructura.cali$area_publica<-estructura.cali$area_calle+estructura.cali$area_manzanas.ep
estructura.cali$area_privada<-estructura.cali$area_su-estructura.cali$area_publica
estructura.cali

# agregados del CA por SU: altura, cantidad y area de copa  ----

AU_stats_por_su<-AU_analsis_spatial@data %>%
  group_by(setu_ccnct) %>%
  dplyr::summarise(area_copa=sum(area_copa),
            altura_media=mean(altura_arbol),
            diametro_medio_copa = mean(diametro_copa),
            num_arboles=n())


AU_stats_por_su %>% summary()


analisis.cali<-full_join(estructura.cali,AU_stats_por_su,by=c("SETU_CCNCT"="setu_ccnct"))
  
analisis.cali$cobertura_copa.su<-analisis.cali$area_copa/analisis.cali$area_su
analisis.cali$arboles_area.su<-analisis.cali$num_arboles/analisis.cali$area_su
analisis.cali$cobertura_copa.ap<-analisis.cali$area_copa/analisis.cali$area_publica
analisis.cali$arboles_area.ap<-analisis.cali$num_arboles/analisis.cali$area_publica

# drops <- c("SETU_CCNCT")
# analisis.cali[ , !(names(analisis.cali) %in% drops)]

summary(analisis.cali)
names(analisis.cali) %>%sort()




# join stats CA20015 con SU ----
# ca2015su.cali<- analisis.cali%>% dplyr::select(SETU_CCDGO,
#                                              area_copa,
#                                              altura_media,
#                                              diametro_medio_copa,
#                                              num_arboles,cobertura_copa.su,
#                                              arboles_area.su,
#                                              cobertura_copa.ap,
#                                              arboles_area.ap) 
# 




# ca2015su.cali$area_su_01<-range01(ca2015su.cali$area_su)
# ca2015su.cali$cobertura_copa_su_01<-range01(ca2015su.cali$cobertura_copa.su,na.rm = T)
# ca2015su.cali$cobertura_copa_ap_01<-range01(ca2015su.cali$cobertura_copa.ap,na.rm = T)
# ca2015su.cali$cantidad_01<-range01(ca2015su.cali$num_arboles,na.rm = T)
# ca2015su.cali$altura_media_su_01<-range01(ca2015su.cali$altura_media,na.rm = T)
# ca2015su.cali$diametro_medio_copa_su_01<-range01(ca2015su.cali$diametro_medio_copa,na.rm = T)
# ca2015su.cali$area_copa_su_01<-range01(ca2015su.cali$area_copa,na.rm = T)
# ca2015su.cali$arboles_su_area_01<-range01(ca2015su.cali$arboles_area.su,na.rm = T)
# ca2015su.cali$arboles_ap_area_01<-range01(ca2015su.cali$arboles_area.ap,na.rm = T)
# 
# 
# #datos CA en formato long ----
# ca2015su.cali.long<-ca2015su.cali %>% 
#   dplyr::select(SETU_CCDGO,cobertura_copa_su_01:arboles_ap_area_01)%>%
#   melt(
#                       # ID variables - all the variables to keep but not split apart on
#     id.vars=c("SETU_CCDGO"),
#     variable.name="ca2015_var",
#     value.name="ca2015_valor"
#     )
# 
# 
# 
# #graficar distribuciones varibles CA ----
# 
# ca2015su.cali.long %>% 
#   filter(ca2015_var=="cobertura_copa_ap_01")%>%
#   ggplot()+
# #  geom_histogram(aes(x="ca2015_valor"),)
#   geom_histogram(aes(x=ca2015_valor),fill=viridis(80),color="white",alpha=0.9,bins = 80)
# #  geom_histogram(aes(x=ca2015_valor),fill=viridis(),color="white",alpha=0.9,bins = 80)
# 
# 
# ca2015su.cali.long %>% 
# #  filter(ca2015_var=="cobertura_copa_su")%>%
#   ggplot()+
# #  geom_histogram(aes(x="ca2015_valor"),)
#   geom_histogram(aes(x=ca2015_valor,fill=..count..),alpha=0.9,bins = 80)+
#   # scale_fill_viridis()+
#   facet_wrap(~ca2015_var,scales = "free")
#   
# 
# ca2015su.cali.long %>% 
#   #filter(ca2015_var!="area_su",ca2015_var!="area_su")%>%
#   ggplot()+
# #  geom_histogram(aes(x="ca2015_valor"),)
# #  geom_histogram(aes(x=ca2015_valor,fill=ca2015_var),alpha=0.9,color="white",bins = 80)+
#  # scale_fill_viridis(discrete = T)+
#   geom_histogram(aes(x=ca2015_valor),alpha=0.9,color="white",bins = 80)+
#   facet_wrap(~ca2015_var,scales = "free",ncol = 1)
#   
#   
#   color_hist<-cut_interval(ca2015su.cali.long$ca2015_valor,40,labels = FALSE) %>% as.numeric()
#   
#   ca2015su.cali.long %>% bind_cols(data.frame(color_hist)) %>%
#   #filter(ca2015_var!="area_su",ca2015_var!="area_su")%>%
#   ggplot()+
# #  geom_histogram(aes(x="ca2015_valor"),)
# #  geom_histogram(aes(x=ca2015_valor,fill=ca2015_var),alpha=0.9,color="white",bins = 80)+
#  # scale_fill_viridis(discrete = T)+
#   geom_histogram(aes(x=ca2015_valor,fill=as.factor(color_hist)), alpha=0.9,bins = 40)+
#   scale_fill_viridis(discrete = T)+
#   theme_minimal()+
#   facet_wrap(~ca2015_var,scales = "free",ncol = 1)
#   
#   #facet_grid(ca2015_var~.,scales = "free")
# 
# 
# 
# 
# # graficar mapas para cada varible CA ----
# su.ca2015<-su
# names(su.ca2015)
# 
# 
# 
# # 2nd convertir a dataframe para usar ggplot  ----
# su.ca2015.f<-fortify(su.ca2015,region = "SETU_CCDGO")
# su.ca2015.f <- merge(su.ca2015.f, ca2015su.cali, by.x = "id", by.y = "SETU_CCDGO")
# 
# su.ca2015.f.long<-fortify(su.ca2015,region = "SETU_CCDGO")
# su.ca2015.f.long <- merge(su.ca2015.f.long, ca2015su.cali.long, by.x = "id", by.y = "SETU_CCDGO")
# 
# 
# 
# #graficar cvarobles ambietales del CA2015 ----
# p_su_ca2015.facet<- ggplot() +
#   geom_polygon(data=su.ca2015.f.long,
#                aes(x=long,y=lat,group=group,fill=ca2015_valor))+
#     coord_equal() + scale_fill_viridis()+
#     theme_void()+
#   facet_wrap(~ca2015_var,ncol = 4)+labs(title=)
# p_su_ca2015.facet
# 
# p_su_area_copa<- ggplot() +
#   geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=area_copa_su))+
#   coord_equal() + scale_fill_viridis()
# 
# p_su_diametro_medio<- ggplot() +
#   geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=diametro_medio_copa_su))+
#   coord_equal() + scale_fill_viridis(option="magma")+
#   geom_polygon(data=manzanas.su.f,aes(x=long,y=lat,group=group),fill="lightgrey",alpha=0.7)
# 
# p_su_altura_media<- ggplot() +
#   geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=altura_media_su))+
#   coord_equal() + scale_fill_viridis(option="magma")
#   
# 
# p_su_num_indarb<- ggplot() +
#   geom_polygon(data=su.ca2015.f,aes(x=long,y=lat,group=group,fill=cantidad_su))+
#   coord_equal() + scale_fill_viridis(option="magma")
# 
# # graficas distribucion CA ----
# p_su_hist_cobertura<-ggplot()+geom_histogram(data=AU_analsis_spatial@data,aes(x=area_copa))

#geom_histogram(aes(color = sex), fill = "white", alpha = 0.6, position="identity")

#grid.arrange(p_su_cobertura, p_su_area_copa, p_su_altura_media, p_su_num_indarb, ncol = 2, nrow =2)



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

cp2005.personas<-CP2005_t_persona_edad %>%
  full_join(CP2005_t_nivel_estudios,by="su_id") %>% dplyr::rename(personas_edad=personas,
                                                           personas_estudio=total_personas) %>%
  full_join(Cp2005_t_limitacion,by="su_id") %>% dplyr::rename(con_alguna_limitacion=SI,
                                                       sin_limitacion=NO,
                                                       personas_limitacion=total_personas) %>%
  full_join(CP2005_t_etnia,by="su_id") %>%
  dplyr::rename(personas_etnia=total_personas) %>%
  mutate(SETU_CCNCT = trimws(su_id))
#buscar duplicados
cp2005.personas%>%
  group_by(SETU_CCNCT) %>% 
  filter(n()>1) 
#datos viviendas
cp2005.viviendas<-CP2005_t_tipo_vivienda%>%
  full_join(CP2005_t_uso_predios,by="su_id") %>% dplyr::rename(viviendas_tipo=total_viviendas.x,
                                                        viviendas_uso=total_viviendas.y) %>%
  full_join(CP2005_t_ocupacion_viviendas, by = "su_id") %>% 
  dplyr::rename(viviendas_ocupacion=total_viviendas) %>% 
  mutate(SETU_CCNCT = trimws(su_id))
  
cp2005.personas$SETU_CCNCT %in% as.character(su$SETU_CCNCT)
cp2005.viviendas$SETU_CCNCT %in% as.character(su$SETU_CCNCT)

#los datos CP2005 DANE con codigos de diferentes longitud ---- 
# remover dos digitos extras que parecen el codigo de comuna, pues coinciden.
#extraemos los SU de los datos del CP2005 del Redatam que coinciden 
#con el codigo de depratamento y de ciudad. 76001 es el codigo de cali
cpSubset_personas <- cp2005.personas[grep("76001", cp2005.personas$SETU_CCNCT), ]
cpSubset_viviendas <-cp2005.viviendas[grep("76001", cp2005.viviendas$SETU_CCNCT), ]

#union personas vivienda
cpSubset<-full_join(cpSubset_viviendas,cpSubset_personas,by="su_id") %>% 
  dplyr::rename(SETU_CCNCT=SETU_CCNCT.x) %>%
  dplyr::select(-one_of(c("SETU_CCNCT.y")))
  

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
setu_ccdgo<-cpSubset$SETU_CCNCT %>% substr(., 17, 20)
comuna_substr<-setu_ccdgo%>% substr(., 1, 2)
c1<-cpSubset$SETU_CCNCT %>% substr(., 1, 6)
c2<-cpSubset$SETU_CCNCT %>% substr(., 9, 20)
setu_ccnct_18<-paste0(c1,c2)
setu_ccnct_20<-cpSubset$SETU_CCNCT
cod_subset<-data.frame(setu_ccnct_20,setu_ccnct_18,comuna_ids,comuna_substr,setu_ccdgo)
cod_subset<-cod_subset %>%
rowwise()%>%
  mutate(cod_consistencia=
           if_else(as.character(comuna_ids)==as.character(comuna_substr) | as.character(comuna_substr)=="99",
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
cp2005.cali<-inner_join(cpSubset,cod_subset,by=c("su_id"="setu_ccnct_20"))
#buscar duplicados
cp2005.cali %>%names()
cp2005.cali%>%
  group_by(setu_ccnct_18) %>% 
  filter(n()>1) %>% arrange(setu_ccnct_18) %>% 
  filter(cod_consistencia=="no-consistente")%>%
  dplyr::select(su_id)->cp2005_elim #eliminar los duplicados no consistentes
#verificar si hay NA en los datos

#verificar los sectores no consistentes
elim_su_cp2005<-as.character(cp2005.cali$su_id) %in% as.character(cp2005_elim$su_id)  
cp2005.cali[elim_su_cp2005,] %>% View()

#verificar si hay datos por SU validos sin valores
cp2005.cali %>%
  filter(is.na(personas_edad),
         is.na(personas_estudio),
         is.na(personas_etnia),
         is.na(personas_limitacion),
         is.na(viviendas_tipo),
         is.na(viviendas_uso),
         is.na(viviendas_ocupacion)) ->  cp2005su.cali.all.na
cp2005su.cali.all.na
cp2005.cali %>%
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
summary(cp2005.cali)
#cp2005.cali[is.na(cp2005.cali)] <-0


#is.na(cp2005su.cali[!elim_su_cp2005,])
cp2005.cali<-cp2005.cali[!elim_su_cp2005,]
summary(cp2005.cali)

# su_eliminados_cod<-cp2005su.cali%>% filter(cod_consistencia =="no-consistente")
# cp2005su.cali<-cp2005su.cali%>% filter(cod_consistencia !="no-consistente")



#calcular las varibles porcetual respectos de la poblacion, y respecto del area.----
total_poblacion<-sum(cp2005.cali$personas_edad,na.rm = T)



# Transformacion de las variables persona CP2005 a porcentaje de personas ----

names(cp2005.cali)
#son muchas variables. Cuales escoger? 
# el primer paso es seleccionar las varibles que en la literatura y para el problema son importantes.
#el segundo es verificar cuanta informacion contienen los datos. 
summary(cp2005.cali) 

#condiciones y rasgos depoblacion
cp2005.personas.sel<-cp2005.cali %>%
  dplyr::select(setu_ccdgo,
         personas_etnia,negro_mulato_afrocolombiano,#indigena, #rom, SAI ,palenquero tienen muy pocs casos
         personas_limitacion,con_alguna_limitacion,
         personas_estudio,ningun_estudio,superior_postgrado,
         personas_edad,edad_promedio)

cp2005.personas.sel[,2:10]%>%plot()

#calculemos estas variables como porcentajes de la poblacion para tener una escala similar

cp2005.personas.sel<-cp2005.personas.sel%>%
  mutate(afro.porcentaje=negro_mulato_afrocolombiano/personas_etnia,
        # indigena.porcentaje=indigena/personas_etnia,
         con_alguna_limitacion.porcentaje=con_alguna_limitacion/personas_limitacion,
         ningun_estudio.porcentaje=ningun_estudio/personas_estudio,
         superior_postgrado.porcentaje=superior_postgrado/personas_estudio,
         poblacion.porcentaje=personas_edad/total_poblacion
         #dominio="personas"
        )
    
summary(cp2005.personas.sel)    
cp2005.personas.sel[,grep("porcentaje",names(cp2005.personas.sel))]%>%plot()
cp2005.personas.sel[,grep("porcentaje",names(cp2005.personas.sel))]


#podemos descartar la variable de poblacion indigena

#variables sobre el uso y tipo de los sectores urbanos ----
summary(cp2005.cali) 

cp2005.predios.sel<-cp2005.cali %>%
  dplyr::select(setu_ccdgo,
                viviendas_tipo,casa,apartamento,tipo_cuarto,
                viviendas_uso,uso_vivienda,uso_unidad_economica#,
                #viviendas_ocupacion,desocupadas
                )
    
#calculemos estas variables como porcentajes de la cantidad de predios para tener una escala similar
cp2005.predios.sel<-cp2005.predios.sel%>%
  mutate(casa.porcentaje=casa/viviendas_tipo,
         apartamento.porcentaje=apartamento/viviendas_tipo,
         cuarto.porcentaje=tipo_cuarto/viviendas_tipo,
         viviendas.porcentaje=uso_vivienda/viviendas_uso,
         unidad_economica.porcentaje=uso_unidad_economica/viviendas_uso
         #desocupadas.porcentaje=desocupadas/viviendas_ocupacion
  #       dominio="predios"
         )
  
summary(cp2005.predios.sel)
plot(cp2005.predios.sel[,grep("porcentaje",names(cp2005.predios.sel))])
#podemos descartar la varible de ocupacion de las viviendas
cp2005.personas.sel



# Consolidacion total datos analisis ----
# ahora a consolidar todas las varibles para hacer una seleccion y depuracion
# de los datos para el analsis

analisis.cali %>% names()
cp2005.predios.sel
cp2005.personas.sel

# drop.cols<-c("SETU_CCNCT","dominio.y","dominio.x")
drop.cols<-c("SETU_CCNCT")


analisis.cali.sel<-analisis.cali%>% 
  full_join(cp2005.predios.sel, by =c("SETU_CCDGO"="setu_ccdgo"))%>%
  full_join(cp2005.personas.sel,by = c("SETU_CCDGO"="setu_ccdgo")) %>%
#  full_join(ia.ev,by=c("SETU_CCDGO"="su.SETU_CCDGO"))%>%
  dplyr::select(-one_of(drop.cols))

# caluclamos indicadores entre las diferentes dimensiones
analisis.cali.sel %>%names()
analisis.cali.sel %>% summary()
analisis.cali.sel<-analisis.cali.sel %>% mutate(arboles_habitante=num_arboles/personas_edad,
                             area_ev_habitante=area_ev/personas_edad,
                             densidad_poblacion=personas_edad/area_su,
                             area_ep.porcentaje=area_ep/area_su,
                             area_ev.porcentaje=area_ev/area_su,
                             area_ev.pub.porcentaje=area_ev/area_publica,
                             area_publica.porcentaje=area_publica/area_su)

analisis.cali.sel %>% summary()
# que indicadores tienene NA?

nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}

nacols(analisis.cali.sel)
analisis.cali.sel %>% summary()
names(analisis.cali.sel)

write.csv(x = analisis.cali.sel,file = "outputData/analisis_datos_su.csv",fileEncoding = "UTF-8",col.names = T)
write.csv(x = AU_analsis,file = "outputData/AU_analisis.csv",fileEncoding = "UTF-8",col.names = T)
write.csv(x = ,file = "outputData/analisis_datos_su.csv",fileEncoding = "UTF-8",col.names = T)

# los NaN, NA e Inf que existen es porque las divisiones por 0 cuando no viven personas o 
# en estos casos podemos reeemplzar por 0 el valor de los NA pues representa numericamente 
# la ausencia de personas 
#o podemos elimianr esos casos del analsis.

analisis.cali.sel.0<-analisis.cali.sel
analisis.cali.sel.0[is.na(analisis.cali.sel.0)]<-0
analisis.cali.sel.0[is.infinite(analisis.cali.sel.0$arboles_habitante),"arboles_habitante"]<-0
analisis.cali.sel.0[is.infinite(analisis.cali.sel.0$area_ev_habitante),"area_ev_habitante"]<-0


summary(analisis.cali.sel.0)

#variables selecciondas 


#solo sectores con los datos completos
analisis.cali.sel.completos<-na.omit(analisis.cali.sel)
#analisis.cali.sel[complete.cases(analisis.cali.sel[,c()]),]
summary(analisis.cali.sel.completos)
nrow(analisis.cali.sel.completos)
names(analisis.cali.sel.completos)

summary(analisis.cali.sel.0)
nrow(analisis.cali.sel.0)
names(analisis.cali.sel.0)

#subconjuntos de variables ----
dependientes<-c("cobertura_copa.su","cobertura_copa.ap",
                "area_ev","area_ev.porcentaje","area_ev_habitante","area_ev.pub.porcentaje",
                "ia.costoviaje","ia.costo.n","ia.mindist","ia.1000", "ia.1000.inv" ,"ia.r300")
cobertura.arborea<-c("cobertura_copa.su","cobertura_copa.ap")
acceso.ev<-c("area_ev","area_ev.porcentaje","area_ev.pub.porcentaje",
             "ia.costoviaje","ia.costo.n","ia.mindist","ia.1000", "ia.1000.inv" ,"ia.r300")

#posibles predictores 
ambientales<-c("altura_media", "diametro_medio_copa",
               "arboles_area.su","arboles_area.ap","arboles_habitante","num_arboles"
               )
poblacion<-c( "densidad_poblacion","edad_promedio",
              "afro.porcentaje","ningun_estudio.porcentaje", "superior_postgrado.porcentaje",
              "con_alguna_limitacion.porcentaje")
predios.uso<-c("unidad_economica.porcentaje","viviendas.porcentaje",
           "casa.porcentaje","apartamento.porcentaje","cuarto.porcentaje")
estructurales<-c("area_media_manzana","num_manzanas",
                 "area_publica","area_publica.porcentaje","area_ev.porcentaje",
                 "area_ep.porcentaje", "area_calle","area_su")


# exclusion de sectores urbanos sin poblacion y sin arboles para la cobertura.
#los sectores atipicos los definiremos porque contienen principalmente una instalacion 
#privada o el porcentaje de espacio verde del SU  es mayor que el 60%  (1712) 
su.exc.atipicos<-c("1709","0304","0703","0701","1317","1802","1710","1712","1928","1917")

analisis.ca.exc<-analisis.cali.sel.0[!(analisis.cali.sel.0$SETU_CCDGO %in% su.exc.atipicos),c("SETU_CCDGO",
                                        cobertura.arborea,
                                        ambientales,
                                        poblacion,
                                        predios.uso,
                                        estructurales)]%>% 
  filter(densidad_poblacion==0 | num_arboles==0 | area_publica.porcentaje>0.65)

plot(analisis.cali.sel.0$area_ev.porcentaje)
plot(analisis.cali.sel.0$area_ep.porcentaje)
plot(analisis.cali.sel.0$area_publica.porcentaje)

plot(analisis.cali.sel.0$area_calle/analisis.cali.sel.0$area_su)
plot(analisis.ca.exc$area_ep.porcentaje)
plot(analisis.ca.exc$area_ev.porcentaje)
plot(analisis.ca.exc$area_publica.porcentaje)

analisis.ca.exc %>% summary()

#generacion de data.frame CA u IA ----
analisis.ca<-analisis.cali.sel.0[!(analisis.cali.sel.0$SETU_CCDGO %in% su.exc.atipicos),c("SETU_CCDGO",
                                                                                              cobertura.arborea,
                                                                                              ambientales,
                                                                                              poblacion,
                                                                                              predios.uso,
                                                                                              estructurales)]%>% 
  filter(densidad_poblacion!=0 & num_arboles!=0 & area_publica.porcentaje<0.65)
analisis.ca %>% summary()

analisis.ia<-analisis.cali.sel.0[!(analisis.cali.sel.0$SETU_CCDGO %in% su.exc.atipicos),c("SETU_CCDGO",
                                                                                          acceso.ev,
                                                                                          poblacion,
                                                                                          predios.uso,"area_publica.porcentaje")]%>% 
  filter(densidad_poblacion!=0,area_publica.porcentaje<0.65)

analisis.ia %>% summary()


analisis.ia.long<-analisis.ia%>%
  melt(
    # ID variables - all the variables to keep but not split apart on
    id.vars=c("SETU_CCDGO"),
    variable.name="variables",
    value.name="valor"
  )

analisis.ca.long<-analisis.ca%>%
  melt(
    # ID variables - all the variables to keep but not split apart on
    id.vars=c("SETU_CCDGO"),
    variable.name="variables",
    value.name="valor"
  )

summary(analisis.ca.long)

# #con NAs
# analisis.cali.sel.long<-analisis.cali.sel[, c("SETU_CCDGO",dependientes,
#                                               ambientales,
#                                               poblacion,
#                                               predios.uso,
#                                               estructurales
#                                               )]%>%
# melt(
#   # ID variables - all the variables to keep but not split apart on
#   id.vars=c("SETU_CCDGO"),
#   variable.name="variables",
#   value.name="valor"
# )
# 
# #con 0s
# analisis.cali.sel.0.long<-analisis.cali.sel.0[, c("SETU_CCDGO",dependientes,
#                                               ambientales,
#                                               poblacion,
#                                               predios.uso,
#                                               estructurales
# )]%>%
#   melt(
#     # ID variables - all the variables to keep but not split apart on
#     id.vars=c("SETU_CCDGO"),
#     variable.name="variables",
#     value.name="valor"
#   )
# 
# #solo observaciones completas para todo el dataset
# analisis.cali.sel.completos.long<-analisis.cali.sel.completos[, c("SETU_CCDGO",dependientes,
#                                                                   ambientales,
#                                                                   poblacion,
#                                                                   predios.uso,
#                                                                   estructurales)]%>%
#   melt(
#     # ID variables - all the variables to keep but not split apart on
#     id.vars=c("SETU_CCDGO"),
#     variable.name="variables",
#     value.name="valor"
#   )



# ver distribucion de los datos ----


#graficar histogramas, cajas y QQplot
analisis.ca.long %>%
#analisis.ca.long[analisis.ca.long$variables %in% c(cobertura.arborea,estructurales),] %>%
# analisis.cali.sel.long[analisis.cali.sel.long$variables %in% predios.uso,] %>%
  ggplot()+
#  geom_histogram(aes(x=valor,fill=cut_interval(valor,20)) ,bins = 40)+
  geom_histogram(aes(x=valor),bins = 20)+
#    scale_fill_viridis(discrete = T)+
  facet_wrap(~variables, scales = "free",ncol = 4)

qqnorm_data <- function(x){
  Q <- as.data.frame(qqnorm(x, plot = FALSE))
  names(Q) <- c("xq", substitute(x))
  Q
}

analisis.ca.long.qq <- analisis.ca.long %>%
  group_by(variables) %>%
  do(with(., qqnorm_data(valor)))

ggplot(data = analisis.ca.long.qq, aes(x = xq, y = valor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Teórico") +
  ylab("Muestra") +
  facet_wrap(~variables,scales = "free")

#La pregunta importante aqui es si debemos elimianar los outlier o 
#  usar otro modelo distinto para ajutar los valores o un metodo robusto
# que reduzca el efecto de los outliers.


analisis.ca.long %>%
#analisis.cali.sel.long[analisis.cali.sel.long$variables %in% predios.uso,] %>%
  inner_join(su.f,.,by=c("id"="SETU_CCDGO")) %>%  
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=cut_number(valor,30)))+
  scale_fill_viridis(discrete = T)+
  coord_equal()+
  theme_void()+
  facet_wrap(~variables)



analisis.ca.long[analisis.ca.long$variables %in% cobertura.arborea,] %>%
  inner_join(su.f,.,by=c("id"="SETU_CCDGO")) %>% 
  ggplot()+
  geom_polygon(data=su.f,aes(x=long,y=lat,group=group),fill="lightgrey")+
  geom_polygon(aes(x=long,y=lat,group=group,fill=cut_interval(valor,20)))+
  scale_fill_viridis(discrete = T,option = "plasma")+
  coord_equal()+
  theme_void()+
  facet_wrap(~variables)
  # labs(title="Porcentaje de viviendas tipo cuarto por SU",
  #      subtitle= "",
  #      caption="Fuente: DANE \n eleborado: @correajfc")
  # 


# correlaciones entre variables ----
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Correlacion Cobertura Arborea y estructura SU ----
cormat.ca.strct.pearson <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                   estructurales)],
                                     use = "pairwise.complete.obs"),2)
head(cormat.ca.strct.pearson)
# Reorder the correlation matrix
cormat.ca.strct.pearson <- reorder_cormat(cormat.ca.strct.pearson)
utri.ca.strct.pearson <- get_upper_tri(cormat.ca.strct.pearson)
# Melt the correlation matrix
melted_utri.ca.strct.pearson <- melt(utri.ca.strct.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.strct.pearson)


ggplot(data = melted_utri.ca.strct.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1),axis.title = element_blank())+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de la estructura de los SU",
                     subtitle="Matriz de coeficientes de Pearson",
                     caption="Fuente:CA2015, IDESC\nCálculos propios")



cormat.ca.strct.spearman <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                     estructurales)],
                                      use = "pairwise.complete.obs",
                             method = "spearman"),2)
head(cormat.ca.strct.spearman)
# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
utri.ca.strct.spearman <- get_upper_tri(cormat.ca.strct.spearman)
# Melt the correlation matrix
melted_utri.ca.strct.spearman <- melt(utri.ca.strct.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.strct.spearman)


ggplot(data = melted_utri.ca.strct.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de la estructura de los SU",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015, IDESC\nCálculos propios",x=NULL,y=NULL)


melted_utri.ca.strct.pearson[(melted_utri.ca.strct.pearson$value>0.49 | melted_utri.ca.strct.pearson$value< -0.49) &
                               melted_utri.ca.strct.pearson$Var1!=melted_utri.ca.strct.pearson$Var2 ,] %>% arrange(value)
melted_utri.ca.strct.spearman[(melted_utri.ca.strct.spearman$value>0.49 | melted_utri.ca.strct.spearman$value< -0.49 )&
                                melted_utri.ca.strct.spearman$Var1!=melted_utri.ca.strct.spearman$Var2,] 

estructurales
estructurales.sel<-c("area_media_manzana","area_ev.porcentaje")
#seleccion de varibles estructurales
#el area media de manzana es el que mejor 


# Correlacion Cobertura Arborea y uso de predios SU ----
cormat.ca.predios.pearson <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                    predios.uso)],
                                     use = "pairwise.complete.obs"),2)
head(cormat.ca.predios.pearson)
# Reorder the correlation matrix
cormat.ca.predios.pearson <- reorder_cormat(cormat.ca.predios.pearson)
utri.ca.predios.pearson <- get_upper_tri(cormat.ca.predios.pearson)
# Melt the correlation matrix
melted_utri.ca.predios.pearson <- melt(utri.ca.predios.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.predios.pearson)


ggplot(data = melted_utri.ca.predios.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson")  +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de uso de predios de los SU",
       subtitle="Matriz de coeficientes de Pearson",
       caption="Fuente:CA2015, DANE\nCálculos propios",x=NULL,y=NULL)




cormat.ca.predios.spearman <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                      predios.uso)],
                                      use = "pairwise.complete.obs",
                                      method = "spearman"),2)
head(cormat.ca.predios.spearman)
# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
utri.ca.predios.spearman <- get_upper_tri(cormat.ca.predios.spearman)
# Melt the correlation matrix
melted_utri.ca.predios.spearman <- melt(utri.ca.predios.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.predios.spearman)


ggplot(data = melted_utri.ca.predios.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white",  
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de uso de predios de los SU",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015, DANE\nCálculos propios",x=NULL,y=NULL)

melted_ltri.ca.predios.pearson[(melted_ltri.ca.predios.pearson$value>0.49 | melted_ltri.ca.predios.pearson$value< -0.49) &
                                 melted_ltri.ca.predios.pearson$Var1!=melted_ltri.ca.predios.pearson$Var2 ,] %>% arrange(value)
melted_utri.ca.predios.spearman[(melted_utri.ca.predios.spearman$value>0.49 | melted_utri.ca.predios.spearman$value< -0.49 )&
                                  melted_utri.ca.predios.spearman$Var1!=melted_utri.ca.predios.spearman$Var2,] 

#para los tipo de uso de los predios las variables elegidas son
#
predios.uso
predios.uso.sel<-c("apartamento.porcentaje","cuarto.porcentaje")


# Correlacion Cobertura Arborea y poblacion SU ----
cormat.ca.poblacion.pearson <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                      poblacion)],
                                       use = "pairwise.complete.obs"),2)
head(cormat.ca.poblacion.pearson)
# Reorder the correlation matrix
cormat.ca.poblacion.pearson <- reorder_cormat(cormat.ca.poblacion.pearson)
utri.ca.poblacion.pearson <- get_upper_tri(cormat.ca.poblacion.pearson)
# Melt the correlation matrix
melted_utri.ca.poblacion.pearson <- melt(utri.ca.poblacion.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.poblacion.pearson)


ggplot(data = melted_utri.ca.poblacion.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white",  
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de población de los SU",
       subtitle="Matriz de coeficientes de Pearson",
       caption="Fuente:CA2015, DANE\nCálculos propios",x=NULL,y=NULL)



cormat.ca.poblacion.spearman <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                       poblacion)],
                                        use = "pairwise.complete.obs",
                                        method = "spearman"),2)
head(cormat.ca.poblacion.spearman)
# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
utri.ca.poblacion.spearman <- get_upper_tri(cormat.ca.poblacion.spearman)
# Melt the correlation matrix
melted_utri.ca.poblacion.spearman <- melt(utri.ca.poblacion.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.poblacion.spearman)


ggplot(data = melted_utri.ca.poblacion.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables de población de los SU",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015, DANE\nCálculos propios",x=NULL,y=NULL)

melted_utri.ca.poblacion.pearson[(melted_utri.ca.poblacion.pearson$value>0.49 | melted_utri.ca.poblacion.pearson$value< -0.49) &
                                  melted_utri.ca.poblacion.pearson$Var1!=melted_utri.ca.poblacion.pearson$Var2 ,] %>% arrange(value)
melted_utri.ca.poblacion.spearman[(melted_utri.ca.poblacion.spearman$value>0.49 | melted_utri.ca.poblacion.spearman$value< -0.49 )&
                                    melted_utri.ca.poblacion.spearman$Var1!=melted_utri.ca.poblacion.spearman$Var2,] 

#para los tipo de uso de los poblacion las variables elegidas son
#
poblacion
poblacion.sel<-c("superior_postgrado.porcentaje","afro.porcentaje")



# Correlacion Cobertura Arborea y variables ambientales SU ----

cormat.ca.ambientales.pearson <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                        ambientales)],
                                         use = "pairwise.complete.obs"),2)
head(cormat.ca.ambientales.pearson)
# Reorder the correlation matrix
cormat.ca.ambientales.pearson <- reorder_cormat(cormat.ca.ambientales.pearson)
utri.ca.ambientales.pearson <- get_upper_tri(cormat.ca.ambientales.pearson)
# Melt the correlation matrix
melted_utri.ca.ambientales.pearson <- melt(utri.ca.ambientales.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.ambientales.pearson)


ggplot(data = melted_utri.ca.ambientales.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables ambientales de los SU",
       subtitle="Matriz de coeficientes de Pearson",
       caption="Fuente:CA2015\nCálculos propios",x=NULL,y=NULL)



cormat.ca.ambientales.spearman <- round(cor(analisis.ca[,c(cobertura.arborea,
                                                         ambientales)],
                                          use = "pairwise.complete.obs",
                                          method = "spearman"),2)
head(cormat.ca.ambientales.spearman)
# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
utri.ca.ambientales.spearman <- get_upper_tri(cormat.ca.ambientales.spearman)
# Melt the correlation matrix
melted_utri.ca.ambientales.spearman <- melt(utri.ca.ambientales.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_utri.ca.ambientales.spearman)


ggplot(data = melted_utri.ca.ambientales.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Cobertura arbórea y variables ambientales de los SU",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015\nCálculos propios",x=NULL,y=NULL)

melted_utri.ca.ambientales.pearson[(melted_utri.ca.ambientales.pearson$value>0.49 | melted_utri.ca.ambientales.pearson$value< -0.49) &
                                     melted_utri.ca.ambientales.pearson$Var1!=melted_utri.ca.ambientales.pearson$Var2 ,] %>% arrange(value)
melted_utri.ca.ambientales.spearman[(melted_utri.ca.ambientales.spearman$value>0.49 | melted_utri.ca.ambientales.spearman$value< -0.49 )&
                                      melted_utri.ca.ambientales.spearman$Var1!=melted_utri.ca.ambientales.spearman$Var2,] %>% arrange(value)

#para los tipo de uso de los ambientales las variables elegidas son
#
ambientales
ambientales.sel<-c("diametro_medio_copa","arboles_area.ap")

predictores.ca<-c(ambientales.sel,poblacion.sel,estructurales.sel,predios.uso.sel)
predictores.ca

#corelacion entre los predictores.ca ----
cormat.prdca.pearson <- round(cor(analisis.ca[,c(predictores.ca)],
                                 use = "pairwise.complete.obs"),2)
head(cormat.prdca.pearson)
# Reorder the correlation matrix
cormat.prdca.pearson <- reorder_cormat(cormat.prdca.pearson)
upper_tri.prdca.pearson <- get_upper_tri(cormat.prdca.pearson)
# Melt the correlation matrix
melted_cormat.prdca.pearson <- melt(upper_tri.prdca.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_cormat.prdca.pearson)


ggplot(data = melted_cormat.prdca.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Correlación entre posibles predictores de coberturata arbórea",
       subtitle="Matriz de coeficientes de Pearson",
       caption="Fuente:CA2015,DANE e IDESC\nCálculos propios",x=NULL,y=NULL)

cormat.prdca.spearman <- round(cor(analisis.ca[,c(predictores.ca)],
                                 use = "pairwise.complete.obs",
                                 method = "spearman"),2)
head(cormat.prdca.spearman)
# Reorder the correlation matrix
cormat.prdca.spearman <- reorder_cormat(cormat.prdca.spearman)
upper_tri.prdca.spearman <- get_upper_tri(cormat.prdca.spearman)
# Melt the correlation matrix
melted_cormat.prdca.spearman <- melt(upper_tri.prdca.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_cormat.prdca.spearman)


ggplot(data = melted_cormat.prdca.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Correlación entre posibles predictores de coberturata arbórea",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015,DANE e IDESC\nCálculos propios",x=NULL,y=NULL)


melted_cormat.prdca.pearson[(melted_cormat.prdca.pearson$value>0.49 | melted_cormat.prdca.pearson$value< -0.49 )&
                              melted_cormat.prdca.pearson$Var1!=melted_cormat.prdca.pearson$Var2,]%>% arrange(value)
melted_cormat.prdca.spearman[(melted_cormat.prdca.spearman$value>0.5 | melted_cormat.prdca.spearman$value< -0.5 )&
                               melted_cormat.prdca.spearman$Var1!=melted_cormat.prdca.spearman$Var2,]%>% arrange(value)


#corelacion entre los predictores.ca y la cobertura arborea ----
cormat.all.pearson <- round(cor(analisis.ca[,c(cobertura.arborea,predictores.ca)],
                                use = "pairwise.complete.obs"),2)
head(cormat.all.pearson)
# Reorder the correlation matrix
cormat.all.pearson <- reorder_cormat(cormat.all.pearson)
upper_tri.all.pearson <- get_upper_tri(cormat.all.pearson)
# Melt the correlation matrix
melted_cormat.all.pearson <- melt(upper_tri.all.pearson, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_cormat.all.pearson)


ggplot(data = melted_cormat.all.pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Correlación entre posibles predictores de coberturata arbórea",
       subtitle="Matriz de coeficientes de Pearson",
       caption="Fuente:CA2015,DANE e IDESC\nCálculos propios",x=NULL,y=NULL)

cormat.all.spearman <- round(cor(analisis.ca[,c(cobertura.arborea, predictores.ca)],
                                 use = "pairwise.complete.obs",
                                 method = "spearman"),2)
head(cormat.all.spearman)
# Reorder the correlation matrix
cormat.all.spearman <- reorder_cormat(cormat.all.spearman)
upper_tri.all.spearman <- get_upper_tri(cormat.all.spearman)
# Melt the correlation matrix
melted_cormat.all.spearman <- melt(upper_tri.all.spearman, na.rm = TRUE)
#melted_cormat <- melt(cormat)
head(melted_cormat.all.spearman)


ggplot(data = melted_cormat.all.spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
  #scale_fill_viridis(option = "magma")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  labs(title="Correlación entre posibles predictores de coberturata arbórea",
       subtitle="Matriz de coeficientes de Spearman",
       caption="Fuente:CA2015,DANE e IDESC\nCálculos propios",x=NULL,y=NULL)


melted_cormat.all.pearson %>% arrange(Var1,value)
melted_cormat.all.spearman
# podemos hacer una selccion de las variables a incluir/descartar en el modelo
# con base en los coeficientes calculados.
#podemos hacer algunas conjeturas que nos permitan formular modelos a aprobar.
# el porcentaje afro y las personas con estudios superiores presenta un fuerte relacion inversamente proporcional
# podemos usar este par de varibles establecer modelos por dominio y compararlos
# o modelos que mezclan carateristicas o simplemente las varibles mejor rankeadas en teminos de correlacion.
#busco privilejiar las combinaciones razonadas para compararalas con las mejor ajustdas.

# Modelos OLS ----

library(ggfortify)
predictores.ca
cobertura.arborea
analisis.ca %>%summary()
#analisis.ca.long %>%
 analisis.ca.long[analisis.ca.long$variables %in% c("cobertura_copa.ap",predictores.ca,"ningun_estudio.porcentaje"),] %>%
  # analisis.cali.sel.long[analisis.cali.sel.long$variables %in% predios.uso,] %>%
  ggplot()+
  #  geom_histogram(aes(x=valor,fill=cut_interval(valor,20)) ,bins = 40)+
  geom_histogram(aes(x=valor),bins = 20)+
  #    scale_fill_viridis(discrete = T)+
  facet_wrap(~variables, scales = "free",ncol = 4)

#algunas nomalizaciones 
 # log.var<-c("cobertura_copa.ap","area_media_manzana","area_ev.porcentaje")
 log.var<-c("cobertura_copa.ap",predictores.ca)
 log.var.nom<-lapply(log.var,paste0,".log")
 analisis.ca<-analisis.ca[,log.var]%>%
   mutate_each(funs(log(.+1))) %>%
   setNames(nm = as.character(log.var.nom)) %>% 
   bind_cols(analisis.ca,.)
 summary(analisis.ca)
 
 zscore.var<-names(analisis.ca%>%dplyr::select(-SETU_CCDGO))
 zscore.var.nom<-sapply(zscore.var,paste0,".z")
 analisis.ca<-analisis.ca[,zscore.var]%>%
   mutate_each(funs(base::scale(.) %>% as.vector)) %>%
   setNames(nm = zscore.var.nom) %>% 
   bind_cols(analisis.ca,.)
 summary(analisis.ca)
 
 analisis.ca.long<-analisis.ca%>%
   melt(
     # ID variables - all the variables to keep but not split apart on
     id.vars=c("SETU_CCDGO"),
     variable.name="variables",
     value.name="valor"
   )
 
 as.factor(as.character(log.var.nom))
 analisis.ca.long[analisis.ca.long$variables %in% c(as.character(zscore.var.nom)),] %>%
   # analisis.cali.sel.long[analisis.cali.sel.long$variables %in% predios.uso,] %>%
   ggplot()+
   #  geom_histogram(aes(x=valor,fill=cut_interval(valor,20)) ,bins = 40)+
   geom_histogram(aes(x=valor),bins = 20)+
   #    scale_fill_viridis(discrete = T)+
   facet_wrap(~variables, scales = "free",ncol = 4) 

#modelos variables ambientales  ----
mod.amb.lm<-lm(formula = cobertura_copa.ap~arboles_area.ap+diametro_medio_copa,
             analisis.ca)
mod.log.amb.lm<-lm(formula = cobertura_copa.ap.log~arboles_area.ap.log.z+diametro_medio_copa.log,
               analisis.ca)
mod.log.std.amb.lm<-lm(formula = cobertura_copa.ap.log.z~arboles_area.ap.log.z+diametro_medio_copa.z,
               analisis.ca)
mod.std.amb.lm<-lm(formula = cobertura_copa.ap.z~arboles_area.ap.z+diametro_medio_copa.z,
                       analisis.ca)
#mod.amb.lm$residuals%>%shapiro.test()
modelo<-mod.log.amb.lm
modelo<-mod.log.std.amb.lm
modelo<-mod.std.amb.lm
modelo<-mod.amb.lm
#modelo<-mod.log.std.amb.lm.out
summary(modelo)
autoplot(modelo,which = 1:6,alpha = 0.6,label.size = 3,label.n = 5)
AIC(modelo)

data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  ggplot()+
  geom_histogram(aes(x=residuals,fill=cut_interval(residuals,20)))+
  scale_fill_viridis(discrete = T)


data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  left_join(su.f,.,by=c("id"="SETU_CCDGO"))%>%
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=residuals))+
  scale_fill_viridis()+coord_fixed()+theme_void()

amb.out<-c(241,47,280,308)
analisis.ca.out<-analisis.ca[-amb.out,]


mod.log.std.amb.lm.out<-lm(formula = cobertura_copa.ap.log.z~arboles_area.ap.log.z+diametro_medio_copa.z,
                           analisis.ca.out)

modelo<-mod.log.std.amb.lm.out
summary(modelo)
autoplot(modelo,which = 1:6)
AIC(modelo)

data.frame(analisis.ca.out$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.out.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  ggplot()+
  geom_histogram(aes(x=residuals,fill=cut_interval(residuals,20)))+
  scale_fill_viridis(discrete = T)



data.frame(analisis.ca.out$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.out.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  left_join(su.f,.,by=c("id"="SETU_CCDGO"))%>%
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=residuals))+
  scale_fill_viridis()+coord_fixed()+theme_void()

  
#modelos variables poblacion  ----
melted_utri.ca.poblacion.spearman %>%
  filter(Var1!=Var2 & (value>0.5 | value< -0.5))%>%
  arrange(value)

mod.pob.lm<-lm(formula = cobertura_copa.ap~afro.porcentaje+ningun_estudio.porcentaje+con_alguna_limitacion.porcentaje,
               analisis.ca)

mod.log.pob.lm<-lm(formula = cobertura_copa.ap.log~afro.porcentaje.z+con_alguna_limitacion.porcentaje.z,
               analisis.ca)

mod.std.pob.lm<-lm(formula = cobertura_copa.ap.z~afro.porcentaje.z+ningun_estudio.porcentaje.z+con_alguna_limitacion.porcentaje.z,
               analisis.ca)

mod.log.std.pob.lm<-lm(formula = cobertura_copa.ap.log.z~afro.porcentaje.log.z+ningun_estudio.porcentaje.z,
                   analisis.ca)

mod.log.std.sup.lm<-lm(formula = cobertura_copa.ap.log.z~superior_postgrado.porcentaje.log.z,
                   analisis.ca)

mod.log.std.afro.lm<-lm(formula = cobertura_copa.ap.log.z~afro.porcentaje.log.z,
                       analisis.ca)

modelo<-mod.pob.lm
modelo<-mod.log.pob.lm
modelo<-mod.std.pob.lm
modelo<-mod.log.std.pob.lm
modelo<-mod.log.std.afro.lm
summary(modelo)
autoplot(modelo,which = 1:6)
AIC(modelo)

data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  ggplot()+
  geom_histogram(aes(x=residuals,fill=cut_interval(residuals,20)))+
  scale_fill_viridis(discrete = T)


data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  left_join(su.f,.,by=c("id"="SETU_CCDGO"))%>%
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=residuals))+
  scale_fill_viridis()+coord_fixed()+theme_void()


#modelos predios -----
melted_utri.ca.predios.spearman %>%
  filter(Var1!=Var2 & (value>0.4 | value< -0.4))%>%
  arrange(value)

mod.predios.lm<-lm(formula = cobertura_copa.ap~apartamento.porcentaje+cuarto.porcentaje,
                analisis.ca)

summary(mod.predios.lm)
autoplot(mod.predios.lm,which = 1:6)

mod.predios.lm$residuals%>%shapiro.test()
data.frame(analisis.ca$SETU_CCDGO,mod.predios.lm$residuals)%>%
  ggplot()+geom_histogram(aes(x=mod.predios.lm.residuals))
AIC(mod.predios.lm)

#modelos estructura SU ----

melted_utri.ca.strct.spearman %>%
  filter(Var1!=Var2,Var1=="cobertura_copa.ap" |Var2=="cobertura_copa.ap" )%>%
  arrange(value)
mod.strct.lm<-lm(formula = cobertura_copa.ap~area_media_manzana+area_ev.porcentaje,
                   analisis.ca)
mod.log.strct.lm<-lm(formula = cobertura_copa.ap.log~area_media_manzana.log+area_ev.porcentaje.log,
                 analisis.ca)

mod.log.std.strct.lm<-lm(formula = cobertura_copa.ap.log.z~area_media_manzana.log.z+area_ev.porcentaje.log.z,
                     analisis.ca)


modelo<-mod.strct.lm
modelo<-mod.log.strct.lm
modelo<-mod.log.std.strct.lm

summary(modelo)
autoplot(modelo,which = 1:6)
AIC(modelo)

data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=modelo.residuals)%>%
  ggplot()+
  geom_histogram(aes(x=residuals,fill=cut_interval(residuals,20)))+
  scale_fill_viridis(discrete = T)


data.frame(analisis.ca$SETU_CCDGO,mod.amb.lm$residuals)%>%
  dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
  dplyr::rename(residuals=mod.amb.lm.residuals)%>%
  left_join(su.f,.,by=c("id"="SETU_CCDGO"))%>%
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=residuals))+
  scale_fill_viridis()+coord_fixed()+theme_void()

# modelos con terminos por dominio ----
  melted_cormat.all.spearman%>%
  filter(Var1!=Var2,Var1=="cobertura_copa.ap" |Var2=="cobertura_copa.ap" )%>%
  arrange(value)
  
  mod.mix.lm<-lm(formula = cobertura_copa.ap~superior_postgrado.porcentaje.z+area_media_manzana.log.z,
                 analisis.ca)
  
  mod.log.std.mix.lm<-lm(formula = cobertura_copa.ap.log.z~superior_postgrado.porcentaje.log.z+afro.porcentaje.log.z+apartamento.porcentaje.log.z+area_ev.porcentaje.log.z,
                   analisis.ca)
  
  mod.mix.lm<-lm(formula = cobertura_copa.ap~afro.porcentaje+apartamento.porcentaje+area_ev.porcentaje,
                 analisis.ca)
  
  mod.mix.lm<-lm(formula = cobertura_copa.ap~afro.porcentaje+apartamento.porcentaje+num_manzanas,
                 analisis.ca)
  
  modelo<-mod.log.std.mix.lm
  modelo<-mod.mix.lm
  summary(modelo)
  autoplot(modelo,which = 1:6)
  AIC(modelo)
  
  data.frame(analisis.ca$SETU_CCDGO,modelo$residuals)%>%
    dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
    dplyr::rename(residuals=modelo.residuals)%>%
    ggplot()+
    geom_histogram(aes(x=residuals,fill=cut_interval(residuals,20)))+
    scale_fill_viridis(discrete = T)
  
  
  data.frame(analisis.ca$SETU_CCDGO,mod.amb.lm$residuals)%>%
    dplyr::rename(SETU_CCDGO=analisis.ca.SETU_CCDGO)%>%
    dplyr::rename(residuals=mod.amb.lm.residuals)%>%
    left_join(su.f,.,by=c("id"="SETU_CCDGO"))%>%
    ggplot()+
    geom_polygon(aes(x=long,y=lat,group=group,fill=residuals))+
    scale_fill_viridis()+coord_fixed()+theme_void()  
  
  #normaizacion y deteccion de outlier ----
#algunos sectore aparecen frecuentemente como valores poco ajustados
  
# outliers de con base en el los OLS
  




#analisis.ca %>%
#Box Plot 
analisis.ca.long[analisis.ca.long$variables %in% predictores.ca,]%>%
ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))

analisis.ca.long[analisis.ca.long$variables %in% c("area_media_ev","area_copa"),] %>%
  ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))

analisis.ca.long[analisis.cali.sel.long$variables %in% c("cobertura_copa.su","cobertura_copa.ap"),] %>%
  ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))

analisis.cali.sel.long[analisis.cali.sel.long$variables %in% c("num_ev","altura_media", "diametro_medio_copa"),] %>%
  ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))

analisis.cali.sel.long[analisis.cali.sel.long$variables %in% c("arboles_area.su","arboles_area.ap"),] %>%
  ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))

analisis.cali.sel.long[analisis.cali.sel.long$variables =="arboles_habitante",] %>%
  ggplot()+
  geom_boxplot(aes(x=variables,y=valor,fill=variables))








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

p_AU+ 
  geom_polygon(data=manzanas.f,aes(x=long,y=lat,group=group), fill= "lightgrey")+
  geom_point(aes(size=diametro_copa,color =emplazamiento, alpha = 1/100))+
  scale_color_brewer(palette = "Dark2",type = "div") +
  scale_size_area(max_size = 0.001)+
  coord_equal()+theme_void()

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

