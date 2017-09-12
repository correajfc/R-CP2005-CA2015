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

