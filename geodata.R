##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Procesar capas de informacion geografica.
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
##################################################


# cargar objetos geograficos ----- 
#listar shapefiles  
list.files("./shapefiles", pattern="\\.shp$")
file.exists("./shapefiles/mc_sectorUrbano_Cali.shp")
file.exists("./shapefiles/mc_manzanas.shp")
file.exists("./shapefiles/mc_perimetro_idesc.shp")
su<-readOGR(dsn =path.expand("./shapefiles"),
            layer = "mc_sectorUrbano_Cali")
manzanas<-readOGR(dsn =path.expand("./shapefiles"),
                  layer = "mc_manzanas")
prmtr_urbn_idesc <-readOGR(dsn =path.expand("./shapefiles"),
                           layer = "mc_perimetro_idesc")
espacio_publico_idesc<-readOGR(dsn = path.expand("./shapefiles"),
                               layer = "mc_espacio_publico_idesc")

espacio_publico_EEC<-readOGR(dsn = path.expand("./shapefiles"),
                             layer = "mc_espacio_publico_EEC")

equipamento_EEC<-readOGR(dsn = path.expand("./shapefiles"),
                         layer = "mc_equipamientos_colectivos_seleccionados_EEC")


colombia_shp <- readOGR(dsn = path.expand("./shapefiles"),
                        layer = "COLOMBIA")


#verificar y corregir la consistencia geometrica de las capas ----

espacio_publico_idesc <- gBuffer(espacio_publico_idesc, byid=TRUE, width=0)
espacio_publico_EEC <- gBuffer(espacio_publico_EEC, byid=TRUE, width=0)
equipamento_EEC <- gBuffer(equipamento_EEC, byid=TRUE, width=0)
manzanas<-gBuffer(manzanas, byid=TRUE, width=0)
su<-gBuffer(su, byid=TRUE, width=0)

# inspeccion shapefiles cargados ----
su
summary(su)
names(su)
crs.su<-proj4string(su)
#eliminar campos sin interes
su@data<-dplyr::select(su@data,SETU_CCDGO,SETU_CCNCT)
#calcular area del sector urbano su
su@data[1:3,]
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
proj4string(manzanas)
identicalCRS(su,manzanas)

prmtr_urbn_idesc
summary(prmtr_urbn_idesc)
names(prmtr_urbn_idesc)
identicalCRS(su,prmtr_urbn_idesc)

library(OpenStreetMap)
prmtr_urbn_idesc.wgs <- spTransform(prmtr_urbn_idesc,CRS("+proj=longlat"))
prmtr_urbn_idesc.df <- fortify(prmtr_urbn_idesc)
prmtr_urbn_idesc.wgs.df <-  fortify(prmtr_urbn_idesc.wgs)
bbcali <- bbox(prmtr_urbn_idesc.wgs)
map <- openmap(c(3.505871+0.01,-76.59076-0.01),
               c(3.331819-0.01,-76.46125+0.01), 
               zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[6],
               mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
OSMap <- autoplot(map.latlon)
OSMap+
  geom_path(data=prmtr_urbn_idesc.wgs.df,  
            aes(long,lat,group=group), fill="none",color="red") +
  coord_equal() +
  ggsn::scalebar(prmtr_urbn_idesc.wgs.df, dist = 2, dd2km = TRUE, model = 'WGS84',st.size = 4)+
  labs(title ="Santiago de Cali", x = "Long", y="Lat",caption="Datos en grados decimales, sistema de coordenas WGS83\nFuentes: OSM e IDESC\nElaboración Propia")



g1 <- OSMap+
  geom_path(data=prmtr_urbn_idesc.wgs.df,  
            aes(long,lat,group=group), fill="none",color="red") +
  coord_equal() +
  ggsn::scalebar(prmtr_urbn_idesc.wgs.df, dist = 2, dd2km = TRUE, model = 'WGS84',st.size = 4)+
  labs(title ="Santiago de Cali", x = "Long", y="Lat",caption="Datos en grados decimales, sistema de coordenas WGS83\nFuentes: OSM e IDESC\nElaboración Propia")


# mapa de colombia

colombia_continental <- colombia_shp[!str_detect(colombia_shp$DPTO_CCDGO,"88"),]
colombia_continental_s <- gSimplify(colombia_continental,0.01, topologyPreserve=TRUE)
colombia_continental_f <- fortify(colombia_continental_s,region = "DPTO_CCDGO")


ggplot(colombia_continental_f,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="lightgrey",color="white")+
  geom_point( y=3.359889,x= -76.638565, color = "red",size = 4)+
  coord_map()+
  theme_void()



g2 <-ggplot(colombia_continental_f,aes(x=long,y=lat,group=group))+
    geom_polygon(fill="lightgrey",color="white",size = 0.2)+
    geom_point( y=3.359889,x= -76.638565, color = "red",size = 2)+
    coord_map()+
    theme_void()+
    theme(panel.background = element_rect(fill = "white"))


# g3 <- g1 + annotation_custom(
#   grob = ggplotGrob(g2),
#   ymin = 3.32,
#   ymax = 3.32 + (3.52 - 3.32)/4,
#   xmin = -76.6,
#   xmax = -76.6 + (-76.45 +76.6)/4
# ) 


grid.newpage()
vp1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp2 <- viewport(width = 0.15, height = 0.2, x = 0.37, y = 0.25)  # the inset in upper right
print(g1, vp = vp1)
print(g2, vp = vp2)


espacio_publico_idesc
summary(espacio_publico_idesc)
names(espacio_publico_idesc)
proj4string(espacio_publico_idesc)
espacio_publico_idesc@data %>% head()
identicalCRS(su,espacio_publico_idesc)


espacio_publico_EEC
summary(espacio_publico_EEC)
names(espacio_publico_EEC)
proj4string(espacio_publico_EEC)
identicalCRS(su,espacio_publico_EEC)

equipamento_EEC
summary(equipamento_EEC)
names(equipamento_EEC)
proj4string(equipamento_EEC)
identicalCRS(su,equipamento_EEC)

# CRS Idesc ----
crs_mc_idesc<-proj4string(manzanas)
crs_mc_idesc


# manznas en sectores urbanos ----
#algunas manzanas no estan totalemente contenidas los sectores.
#para facilitr el cálculo las recortmos con la capa de su, 
#pero este calculo resulta muy costozo, entonces seleccionamos solo las manzanas que toca
#los poligonos de  area verde o espacio público

#selecionamos las manzanas que se tocan con un SU
manzanas$setu_ccnct<-over(manzanas,su)$SETU_CCNCT
manzanas@data<-left_join(manzanas@data,su@data[,1:2],by=c("setu_ccnct"="SETU_CCNCT")) 

manzanas[is.na(manzanas$setu_ccnct),]
manzanas<-manzanas[!is.na(manzanas$setu_ccnct),]
manzanas@data %>% head(10)
manzanas$id_manzana<-rownames(manzanas@data)
manzanas[,c("SETU_CCDGO", "id_manzana")]

#partimos
manzanas.su <- raster::intersect(manzanas[,c("SETU_CCDGO", "id_manzana")],su)
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
manzanas.su@data %>% filter(area_manzana<50)

#fortify manzanas
manzanas.su.f<-fortify(manzanas.su,region = "id_manzana")

manzanas.su.f<-manzanas.su@data%>%
  right_join(manzanas.su.f,by =c("id_manzana"="id") )



#carateristicas fisicas de las manzanas ----
p_manzanas_area<-ggplot(manzanas.su.f)+
  geom_polygon(fill="grey",aes(x=long,y=lat,group=group))+
  coord_equal()+
  geom_polygon(data=filter(manzanas.su.f,area_manzana<100),
               aes(x=long,y=lat,group=group),fill="red")+
  theme_void()#+
#  scale_fill_viridis(direction = -1)
p_manzanas_area

manzanas.su@data %>% filter(area_manzana<20000) %>%
ggplot(aes(x=area_manzana))+
  geom_histogram(bins = 80,fill=rev(viridis(80)))
#para graficar excluiremos la manzana de la fuerza aerea Marco Fidel Suarez,
#no permite ver la variabildad


manzanas.su@data %>% 
  filter(area_manzana<100000) %>%
  ggplot(aes(x=area_manzana))+
  geom_histogram(bins = 80,fill=rev(viridis(80)))

manzanas.su.f%>%
  filter(area_manzana<50000)%>%
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

#pintar usando colores por quantil
manzanas.su.f%>%
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=cut_number(area_manzana,n = 7)))+
  coord_equal()+
  theme_void()+
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1)+
  labs(title="Coropleta del tamaño de manzana en Santiago de Cali, Colombia",
       subtitle="7 grupos con aprox. el mismo número de observaciones",
       caption="Fuente: IDESC \n")

# #pintar manzanas por sector
# manzanas.su.f%>%
#   ggplot(aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=SETU_CCDGO))+
#   coord_equal()+
#   theme_void()+
#   viridis::scale_fill_viridis(discrete = TRUE, direction = -1)+
#   theme(legend.position = "none")
#   labs(title="Manzanas por sector",
#        caption="Fuente: IDESC \n")



# espacios verdes (EV) publicos (EP) y manzanas ----

## equipamento ECC ----
equipamento_EEC %>% head()
equipamento_EEC$CATEGORIA
equipamento_EEC@data
equipamento_EEC
#verificar no incluir equipamentos fuera de los SU
equipamento_EEC<-equipamento_EEC[su,]


names(equipamento_EEC)[names(equipamento_EEC)=="NOMBRE"] <- "nombre"
names(equipamento_EEC)[names(equipamento_EEC)=="CATEGORIA"] <- "categoria"
#eliminar el Colegio Berchmans y Escuela de Carabineros por ser de acceso público

equipamento_EEC<-equipamento_EEC[!(equipamento_EEC$nombre %in% c("Colegio Berchmans",
                                                                 "Escuela de Carabineros")),]


#crear un id_ap id area publica
#area publica (la calle y lugares de acceso publico dentro de los equipamentos)
equipamento_EEC$id_ap<-1:nrow(equipamento_EEC@data)


equipamento_EEC<-equipamento_EEC[,c("id_ap","nombre","categoria")]
names(equipamento_EEC)
proj4string(equipamento_EEC)
summary(equipamento_EEC)



eq.ecc.f<-fortify( equipamento_EEC,region = "id_ap")
eq.ecc.f<- merge(eq.ecc.f,equipamento_EEC@data,by.x="id",by.y="id_ap")

# ext<-matrix(c(-100,-100,100,100),2)
# zum<-bbox(equipamento_EEC@polygons[[1]])
# zum<-zum+ext


ggplot(eq.ecc.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,
               aes(x=long,y=lat,group=group),
               fill="lightgrey")+
  geom_polygon(data = su.f,
               aes(x=long,y=lat,group=group),
               fill=NA, color="lightsteelblue",
               size=0.5)+
  geom_polygon(aes(fill=as.factor(nombre)),alpha=0.6)+
  scale_fill_brewer(palette = "Set1")+
  coord_equal()+
  theme_void() 
#+  coord_cartesian(xlim = zum[1,],ylim = zum[2,])


#comprar interseccion
raster::intersect(espacio_publico_idesc,equipamento_EEC)
raster::intersect(espacio_publico_EEC,equipamento_EEC)

#raster::intersect(espacio_publico_EEC,espacio_publico_idesc)
#equipamento_EEC no se intersecta con ninguno

# espacio publico de la EEC
espacio_publico_EEC<-espacio_publico_EEC[su,]
espacio_publico_EEC$ID_EP
espacio_publico_EEC%>% head()
names(espacio_publico_EEC)
summary(espacio_publico_EEC)
espacio_publico_EEC

names(espacio_publico_EEC)[names(espacio_publico_EEC)=="CATEGORIA"] <- "categoria"
espacio_publico_EEC$nombre<-paste(espacio_publico_EEC$ID_EP,
                                  espacio_publico_EEC$AMBITO,sep = "-")
espacio_publico_EEC$id_ap<-(nrow(equipamento_EEC)+1):(nrow(equipamento_EEC)+
                                                        nrow(espacio_publico_EEC))
espacio_publico_EEC<-espacio_publico_EEC[,c("id_ap","nombre","categoria")]



ep.eec.f<-fortify( espacio_publico_EEC,region = "id_ap")
ep.eec.f<- merge(ep.eec.f,espacio_publico_EEC@data,by.x="id",by.y="id_ap")


# ext<-matrix(c(-100,-100,100,100),2)
# zum<-bbox(espacio_publico_EEC@polygons[[4]])
# zum<-zum+ext

ggplot(eq.ecc.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill="lightgrey")+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.5)+
  geom_polygon(fill="blue",alpha=0.5)+
  geom_polygon(data = ep.eec.f,aes(x=long,y=lat,group=group),fill="magenta",alpha=0.5)+
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

# Nos necesario correr el siguiente codigo comentado pues es para verificar si la interseccion 
# entre espacio_publico_idesc y equipamento_EEC, que es no nula.

# raster::intersect(espacio_publico_idesc,equipamento_EEC)
# inter.epeec.idesc<-raster::intersect(espacio_publico_idesc,espacio_publico_EEC)
# inter.epeec.idesc
# 
# names(inter.epeec.idesc)[names(inter.epeec.idesc)=="id_ap.1"] <- "id_ap"
# names(inter.epeec.idesc)[names(inter.epeec.idesc)=="nombre.1"] <- "nombre"
# names(inter.epeec.idesc)[names(inter.epeec.idesc)=="categoria.1"] <- "categoria"
# inter.epeec.idesc<-inter.epeec.idesc[,c("id_ap","nombre","categoria")]

# Diferencia ntre espacio_publico_idesc y equipamento_EEC 
dif.epeec.idesc<-espacio_publico_EEC - espacio_publico_idesc
dif.epeec.idesc

ep.cali<-raster::union(espacio_publico_idesc,equipamento_EEC)
ep.cali<-raster::union(ep.cali,dif.epeec.idesc)

names(ep.cali)
ep.cali[is.na(ep.cali$id_ap.2),]


ep.cali@data[,c("nombre.1","nombre.2")]
ep.cali@data[,c("categoria.1","categoria.2")]
ep.cali@data[,c("id_ap.1","id_ap.2")]

names(ep.cali)[names(ep.cali)=="id_ap.1"] <- "id_ap"
names(ep.cali)[names(ep.cali)=="nombre.1"] <- "nombre"
names(ep.cali)[names(ep.cali)=="categoria.1"] <- "categoria"


ep.cali$id_ap<-rownames(ep.cali@data)
ep.cali<-ep.cali[,c("id_ap","nombre","categoria")]


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


ggplot(ep.cali.f,aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.1)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.3)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=categoria),alpha=0.9)+
  scale_fill_discrete(na.value = "orange")+
  coord_equal()+
  theme_void() #+coord_fixed(xlim = zum[1,],ylim = zum[2,])

ggplot(ep.cali.f,aes(x=long,y=lat,group=group))+
  #geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.2)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightgrey",size=0.1)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="red",alpha=0.9)+
  coord_equal()+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))+
  facet_wrap(~categoria,nrow = 2)#+coord_fixed(xlim = zum[1,],ylim = zum[2,])

# refinamos la capa el espacio publico
#clip del espacio publico por sector

ep.cali.su<-raster::intersect(ep.cali,su)
ep.cali.su$id_ap<-rownames(ep.cali.su@data)

nrow(ep.cali.su)
summary(ep.cali.su)
ep.cali.su<-ep.cali.su[,c("id_ap","nombre","categoria","SETU_CCDGO")]
ep.cali.su$area_ep<-raster::area(ep.cali.su)




#clip de EP por manzanas ()
ep.cali.manzanas<-raster::intersect(manzanas.su,ep.cali)
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

# epi<-sample(1:nrow(ep.cali.manzanas[ep.cali.manzanas$area_ep.manzana<10,]),size = 1)
# bbi<-bbox(ep.cali.manzanas@polygons[[epi]])
# zum<-bbi+ext

ep.cali.manzanas.f %>% filter(area_ep.manzana < 10 ) %>%
ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.1)+
  geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.3)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=categoria),alpha=0.9)+
  scale_fill_discrete(na.value = "orange")+
  coord_equal()+
  theme_void() + coord_fixed(xlim = zum[1,],ylim = zum[2,])

# ep.cali.manzanas.f %>% filter(area_ep.manzana < 25 ) %>%
#   ggplot(aes(x=long,y=lat,group=group))+
#   #geom_polygon(data=manzanas.su.f ,aes(x=long,y=lat,group=group),fill=NA,color="lightgrey",size=0.1)+
#   #geom_polygon(data = su.f,aes(x=long,y=lat,group=group),fill=NA, color="lightsteelblue",size=0.3)+
#   geom_polygon(aes(x=long,y=lat,group=group))+
#   #scale_fill_discrete(na.value = "orange")+
#   coord_equal()+
#   theme_void() +
#   facet_wrap(~id_ap)
  

# algunos espacio verdes se pierden con el refinamiento por manzanas, no es
# necesario por ahora trabajar con ellas.

# Otros espacio son muy pequeños. Vamos a considerar espacios verdes o publico
# los elementos con un area mayor a 25 m2

# eliminar espacio menores a 25 m2 y llamaremoa a todo ev para lo que se considra verde ficha.
# todas las categorias deben ser consideras dentro de las mediciones de espacio verde

ep.cali.su<-ep.cali.su[ep.cali.su$area_ep >25,]

#clasificacion del Espacio publico en espacio verde ----

ep.cali.su$categoria <-as.character(ep.cali.su$categoria)
summary(ep.cali.su)
#stringr::str_replace(ep.cali.su$categoria,"NA","Zona Verde no clasificada")
ep.cali.su@data%<>%
  replace_na(list(categoria="Zona Verde no clasificada"))

ep.cali$categoria <-as.character(ep.cali$categoria)
ep.cali@data%<>%
  replace_na(list(categoria="Zona Verde no clasificada"))
   
ep.cat<-ep.cali.su$categoria %>% unique()
#ev.cat<-ep.cat[c(1,2,3,7,9)] # matendrmos todas las categorias 
# ev.cali.su<-ep.cali.su[ep.cali.su$categoria %in% ev.cat,]
# names(ev.cali.su)[names(ev.cali.su)=="area_ep"] <- "area_ev"



gIsValid(ep.cali.su, reason = T)
gIsValid(ep.cali.manzanas, reason = T)
gIsValid(manzanas, reason = T)

# calculos de indices de acceso a EV ----
ep.cali$area_ep<-raster::area(ep.cali)
ep.cali
ep.cali.su
# ev.cali<-ep.cali[ep.cali$categoria %in% ev.cat,]
#ev.cali$categoria
#names(ev.cali)[names(ev.cali)=="area_ep"] <- "area_ev"

centroides.su<-gCentroid(su, byid = T)
centroides.su$setu_ccgdo<-over(centroides.su,su)$SETU_CCDGO
centroides.su$setu_ccgdo
plot(centroides.su)


# usamos ep.cali pues tiene sitios menos fracmentados y el acceso es al sitio completo,
# aunque las fronteras administrativas las partan

# matriz distancia entre centriodes y espacios
m.dist.ctrdsu.ep<-gDistance(ep.cali,centroides.su, byid = T) 
#cuando el punto esta dentro del poligono el valor que rertorna es 0
# por ese motivo le pondremos 10 a valores = 0 o entre 0 y 10 para 
# evitar problemas al invertir la matriz y mejorar la conistencia del indices
# con valores inversos a la distancia
m.dist.ctrdsu.ep[m.dist.ctrdsu.ep < 10]<- 10
# matriz consicion de estar a 1000 del centriodo
is.1000.ep<-gWithinDistance(ep.cali,centroides.su,1001, byid = T)
# cantidad de espacio verdes en radio de 1000 m de los centriodes del sector urbano
num.ep.1000<-apply(is.1000.ep,1,function(x)  sum(x,na.rm =T))
#distancias de Espacio publicos a 1000 del centriode de SU
a<-m.dist.ctrdsu.ep*is.1000.ep

# distancia minima distinta de 0
ia.mindist<-apply(m.dist.ctrdsu.ep,1,function(x)  min(x[x!=0]))
index.min<-apply(m.dist.ctrdsu.ep,1,function(x)  which.min(x[x!=0]))
ia.area.mindist<-ep.cali$area_ep[index.min]/ia.mindist
# suma de las distancias a cada EP por centriode 
ia.costoviaje<-apply(m.dist.ctrdsu.ep,1,sum)
# suma de las distancias a cada EP ubicado a menos de 1000 m del centriode
ia.1000<-apply(a,1,function (x) sum(x))
ia.1000[ia.1000==0]<-NA
ia.1000.n<-ia.1000/num.ep.1000
# indice de la suma de las areas en el rango de un 1 km del sector censal
ia.areas.1000<-is.1000.ep %*% ep.cali$area_ep %>% as.vector()
# indice de area disponible en el radio de 1km como porcentaje del area total 
# disponible 

ia.areas.1000.porcentaje<-ia.areas.1000/sum(ep.cali$area_ep)

# matriz de distancias inversas de centriode su a espacios verdes  
m.dist.ctrdsu.1000.ep.inv<-1/a
b<-m.dist.ctrdsu.1000.ep.inv*is.finite(m.dist.ctrdsu.1000.ep.inv) # eliminar infinitos
# suma de inverso de las distancias a cada EP ubicado a menos de 1000 m del centriode
ia.1000.inv<-apply(b,1,function (x) sum(x,na.rm = T))
ia.1000.inv[ia.1000.inv==0]<-NA
#razon entre Area del EP y distancia al centriode
A.D<-t(t(b)*ep.cali$area_ep)

# sumatoria de la razon entre Area del EP y distancias de ese EP al centriode
ia.A.D<-apply(A.D,1,function (x) sum(x,na.rm = T))
class(ia.costoviaje)
summary(ia.costoviaje)
length(ia.costoviaje)
summary(ia.1000.inv)
length(ia.A.D)
summary(ia.A.D)

# consolidadcionde indices calculados
ia.ev<-data.frame(su$SETU_CCDGO,ia.costoviaje)
ia.ev$ia.costo.n<-ia.ev$ia.costoviaje/dim(m.dist.ctrdsu.ep)[2]
ia.ev<-bind_cols(ia.ev,data.frame(ia.1000,
                                  ia.1000.inv,
                                  ia.1000.n,
                                  ia.areas.1000,
                                  ia.areas.1000.porcentaje
                                  ))
ia.ev$ia.r300<-300*ia.1000.inv
ia.ev<-ia.ev%>%dplyr::rename(SETU_CCDGO=su.SETU_CCDGO)
ia.ev$ia.mindist<-ia.mindist
ia.ev$ia.area.mindist<-ia.area.mindist
ia.ev$ia.A.D<-ia.A.D
smry.area<-summary(ep.cali$area_ep)
ia.ev$ia.r300.Amedia<- 300/smry.area[4]*ia.ev$ia.A.D
ia.ev$ia.r300.Amediana<- 300/smry.area[3]*ia.ev$ia.A.D
ia.ev$ia.areas.dist<-ia.areas.1000/ia.1000#
ia.ev$ia.areas.dist.mn<-ia.ev$ia.areas.dist/max(ia.ev$ia.areas.dist)# escaldo 01

summary(ia.ev)

ggplot(ia.ev,aes(x=ia.area.mindist,fill=cut_interval(ia.area.mindist,40)))+
  geom_histogram(bins = 40)+scale_fill_viridis(discrete = T)

ia.ev.su<-su

ia.ev.su@data<-left_join(ia.ev.su@data,ia.ev,by=c("SETU_CCDGO"="SETU_CCDGO"))

ia.ev.su.f<-fortify( ia.ev.su,region = "SETU_CCDGO")
ia.ev.su.f<- merge(ia.ev.su.f,ia.ev.su@data,by.x="id",by.y="SETU_CCDGO")

ggplot(ia.ev.su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=cut_interval(ia.A.D,40)))+
  coord_equal() + scale_fill_viridis(discrete = T, drop=FALSE)+
  theme_void()

ggplot(ia.ev.su.f,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=cut_interval(ia.r300.Amedia,10)))+
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

# ev.cali.stats.su<-ep.cali.su@data%>%
#   group_by(SETU_CCDGO) %>%
#   dplyr::summarise(area_ev=sum(area_ev),
#                    area_media_ev=mean(area_ev),
#                    num_ev=n())

#join stats manzanas, areas publicas y espacio verdes con SU ----
#data frame
estructura.cali.df<-su@data %>% 
  left_join(manzanas.stats.su,by="SETU_CCDGO") %>%
  left_join(ep.cali.stats.su, by="SETU_CCDGO") %>%
  #full_join(ev.cali.stats.su, by="SETU_CCDGO") %>%
  left_join(ep.cali.manzanas.stats.su, by="SETU_CCDGO") 

is.na(estructura.cali.df)
estructura.cali.df[is.na(estructura.cali.df)] <-0 # los sectores con NA, sin datos
estructura.cali.df<-estructura.cali.df%>% 
  left_join(ia.ev, by="SETU_CCDGO")
summary(estructura.cali.df)

#calulamos los indicadores con base en las variables de estructura de los SU e indeces de EV
#area de la calle es todo lo que no es una manzana dentro del SU
estructura.cali.df$area_calle<-estructura.cali.df$area_su-estructura.cali.df$area_manzanas
estructura.cali.df[estructura.cali.df$area_calle<0,]
estructura.cali.df[estructura.cali.df$area_calle==0,]

#area publica es el area de la calle más las secciones de manzanas que son espacio publico.
estructura.cali.df$area_publica<-estructura.cali.df$area_calle+estructura.cali.df$area_manzanas.ep
estructura.cali.df$area_privada<-estructura.cali.df$area_su-estructura.cali.df$area_publica
estructura.cali.df

ruta= "./outputData/shp/"

shapefile(su, paste0(ruta,"su",".shp"), overwrite=TRUE)
shapefile(ep.cali, paste0(ruta,"ep-cali",".shp"), overwrite=TRUE)
shapefile(ep.cali.su, paste0(ruta,"ep-cali-su",".shp"), overwrite=TRUE)
shapefile(ia.ev.su, paste0(ruta,"ia-ev-su",".shp"), overwrite=TRUE)



write.csv(estructura.cali.df,file = "./outputData/estructura-cali-df.csv")
write.csv(ia.ev,file = "./outputData/ia-ev.csv")
# write.csv(cp2005.viviendas,file = "./outputData/cp2005-viviendas.csv")
# 
save(estructura.cali.df,
     ia.ev,
     ia.ev.su,ia.ev.su,
     ep.cali.su,
     ep.cali,
     centroides.su,
     su,
     manzanas.su,
      file = "geo.RData")

