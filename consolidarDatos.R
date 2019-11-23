##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Consolidar datos geograficos y alfanumericos 
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: geodata.R, arboles.R, censopoblacion.R
##################################################

#debmos haber corrido previamente geodata.R, arboles.R y censopoblacion.R
#cargar datos previamente procesados
# load("arboles.RData")
# load("geo.RData")
# load("cp2005.RData")
# Datos de estructura de los SU 

estructura.cali.df

estructura.cali<-su
estructura.cali@data %<>% dplyr::select(SETU_CCDGO) %>%
  left_join(estructura.cali.df, by ="SETU_CCDGO") 
#plot(estructura.cali)
# Censo Poblacion
# unir todos las varobles censales en un objeto espacial de sectore Urbanos

cp2005.cali<-su
cp2005.cali@data %<>% dplyr::select(SETU_CCNCT) %>%
  left_join(cp2005.personas, by ="SETU_CCNCT") %>%
  dplyr::select(-su_id) %>%
  left_join(cp2005.viviendas,  by ="SETU_CCNCT" ) %>%
  dplyr::select(-su_id)



# Arboles
# creamos la capa de puntos con los arboles seleccionados ----
coords_arboles <- SpatialPoints(AU_analsis[, c("Este", "Norte")])
AU_analsis_spatial <- SpatialPointsDataFrame(coords_arboles, AU_analsis)
proj4string(AU_analsis_spatial) <- crs_mc_idesc
identical(proj4string(su),proj4string(AU_analsis_spatial))


#arboles en sectores urbanos ----
#inside.su <- !is.na(over(AU_analsis_spatial,as(su,"SpatialPolygons")))

#asiganr sector urbano a cada arbol y manzana dentro del sector
#AU_analsis_spatial$SETU_CCNCT<-over(AU_analsis_spatial,su)$SETU_CCNCT
AU_analsis_spatial$SETU_CCDGO<-over(AU_analsis_spatial,su)$SETU_CCDGO
names(AU_analsis_spatial)


#eliminamos arboles fuera de los sectores censales
AU_analsis_spatial<-AU_analsis_spatial[!is.na(AU_analsis_spatial$SETU_CCDGO),]
shapefile(AU_analsis_spatial, paste0(ruta,"arboles-sel-su",".shp"), overwrite=TRUE)
# agregados del CA por SU: altura, cantidad y area de copa  ----

AU_stats_por_su<-AU_analsis_spatial@data %>%
  group_by(SETU_CCDGO) %>%
  dplyr::summarise(area_copa=sum(area_copa),
                   altura_media=mean(altura_arbol),
                   diametro_medio_copa = mean(diametro_copa),
                   num_arboles=n())


AU_stats_por_su %>% summary()

#construimos un nuevo data.frame con toda la informacion conjunta
#para luego seleccionar con diferents criterios provenientes de 
# la literarura citada y los analisis exploratorio de los datos

# unir todo en solo df

analisis.cali.base<-estructura.cali@data %>% 
  left_join(AU_stats_por_su,by="SETU_CCDGO") %>%
  left_join(cp2005.cali@data,  by ="SETU_CCNCT" ) 

# calculo de agregados para la el conjunto de sectore urbanos.
totales.cali<- analisis.cali.base %>% 
  summarise(area_su=sum(na.rm =T,area_su),
            area_manzanas=sum(na.rm =T,area_manzanas),
            num_manzanas=sum(na.rm =T,num_manzanas),
            area_ep=sum(na.rm =T,area_ep),
            area_manzanas.ep=sum(na.rm =T,area_manzanas.ep),
            area_calle=sum(na.rm =T,area_calle),
            area_publica=sum(na.rm =T,area_publica),
            area_privada=sum(na.rm =T,area_privada),
            area_copa=sum(na.rm =T,area_copa),
            num_arboles=sum(na.rm =T,num_arboles),
            poblacion=sum(na.rm =T,personas_edad),
            poblacion_afro=sum(na.rm =T,negro_mulato_afrocolombiano),
            poblacion_indigena=sum(na.rm =T,indigena),
            poblacion_rom=sum(na.rm =T,rom),
            poblacion_palenquero=sum(na.rm =T,palenquero),
            poblacion_SAI=sum(na.rm =T,raizal_SAI_Providencia),
            viviendas_tipo=sum(na.rm =T,viviendas_tipo),
            viviendas_uso=sum(na.rm =T,viviendas_uso),
            viviendas_ocupacion=sum(na.rm =T,viviendas_ocupacion))



# calculo de nuevos indicadores y variables  ----
# indicadores porcentuales y en relacion numero de habitantes
analisis.cali.df<-analisis.cali.base %>% 
  mutate(cobertura_copa.su=area_copa/area_su,
          arboles_area.su=num_arboles/area_su,
          cobertura_copa.ap=area_copa/area_publica,
          arboles_area.ap = num_arboles/area_publica,
          arboles_habitante =num_arboles/personas_edad,
          densidad_poblacion=personas_edad/area_su,
          area_ep.porcentaje=area_ep/area_su,
          area_privada.porcentaje=area_privada/area_su,
          area_calle.porcentaje=area_calle/area_su,
          area_publica.porcentaje=area_publica/area_su,
          area_copa.porcentaje=area_copa/totales.cali$area_copa) %>% 
 
  mutate(afro.porcentaje=negro_mulato_afrocolombiano/personas_etnia,
         indigena.porcentaje=indigena/personas_etnia,
         rom.porcentaje=rom/personas_etnia,
         SAI.porcentaje=raizal_SAI_Providencia/personas_etnia,
         palenquero.porcentaje=palenquero/personas_etnia,
         ninguna_etnia=ninguno_de_los_anteriores/personas_etnia,
         no_informa_etnia=no_informa/personas_etnia,
         con_alguna_limitacion.porcentaje=con_alguna_limitacion/personas_limitacion,
         sin_limitacion.porcentaje=sin_limitacion/personas_limitacion,
         ningun_estudio.porcentaje=ningun_estudio/personas_estudio,
         superior_postgrado.porcentaje=superior_postgrado/personas_estudio,
         poblacion.porcentaje=personas_edad/totales.cali$poblacion,
         poblacion_afro.porcentaje=negro_mulato_afrocolombiano/totales.cali$poblacion_afro) %>% 
    
  mutate(casa.porcentaje=casa/viviendas_tipo,
         apartamento.porcentaje=apartamento/viviendas_tipo,
         cuarto.porcentaje=tipo_cuarto/viviendas_tipo,
         otro_tipo_de_vivienda.porcentaje=otro_tipo_de_vivienda/viviendas_tipo,
         viviendas.porcentaje=uso_vivienda/viviendas_uso,
         unidad_economica.porcentaje=uso_unidad_economica/viviendas_uso,
         LEA.porcentaje=uso_LEA/viviendas_uso) 


nombres_variables_ppales <- c("cobertura_copa.ap","densidad_poblacion","area_copa",
                              "superior_postgrado",
                              "ningun_estudio",
                              "con_alguna_limitacion",
                              "negro_mulato_afrocolombiano",
                              "afro.porcentaje", 
                              "con_alguna_limitacion.porcentaje",
                              "ningun_estudio.porcentaje",
                              "superior_postgrado.porcentaje",
                              "cuarto.porcentaje",
                              "viviendas.porcentaje" ,
                              "unidad_economica.porcentaje",
                              "LEA.porcentaje",
                              "area_ep",
                              "area_ep.porcentaje",
                              "apartamento.porcentaje",
                              "lag.superior_postgrado",
                              "lag.densidad_poblacion",
                              "lag.cuarto.porcentaje",
                              "lag.area_ep",
                              "ia.areas.dist",
                              "casa","apartamento","tipo_cuarto","uso_vivienda",
                              "uso_unidad_economica","uso_LEA","casa.porcentaje",
                              "area_media_manzana",
                              "area_manzanas",
                              "num_manzanas",
                              "area_privada",
                              "area_calle",
                              "area_publica.porcentaje",
                              "area_privada.porcentaje",
                              "area_calle.porcentaje",
                              "area_su",
                              "lag.superior_postgrado.porcentaje",
                              "lag.unidad_economica.porcentaje",
                              "lag.area_media_manzana",
                              "lag.con_alguna_limitacion.porcentaje")
nombres_reemplazo <- c("cobertura de copa [%]","densidad de población","área de copa",
                       "con estudios superiores","ningún estudio","con alguna limitación",
                       "afrocolombianos",
                       "afrocolombianos [%]", 
                       "con alguna limitación [%]",
                       "ningun estudio [%]",
                       "con estudios superiores [%]",
                       "vivienda tipo cuarto [%]",
                       "uso de vivienda [%]" ,
                       "uso de unidad económica [%]",
                       "uso de LEA [%]",
                       "área de EV",
                       "área de EV [%]",
                       "vivienda tipo apartamento [%]",
                       "estudios superiores (retardada)",
                       "densidad de población (retardada)",
                       "vivienda tipo cuarto [%] (retardada)",
                       "área de EV (retardada)",
                       "razón área-distancia",
                       "vivienda tipo casa","vivienda tipo apartamento","vivienda tipo cuarto","uso de vivienda",
                       "uso de unidad económica","uso de LEA","vivienda tipo casa [%]",
                       "área media de manzana",
                       "área de manzanas",
                       "número de manzanas",
                       "área privada",
                       "área de las calles",
                       "área pública [%]",
                       "área privada [%]",
                       "área de las calles [%]",
                       "área del SU",
                       "estudios superiores [%] (retardada)",
                       "uso de unidad económica [%] (retardada)",
                       "área media de manzana (retardada)",
                       "con alguna limitación [%] (retardada)")

df_nombres_variables <- data_frame(nombres_variables_ppales,nombres_reemplazo)

#reemplzar_nombres_df(analisis.cali.df,df_nombres_variables)
