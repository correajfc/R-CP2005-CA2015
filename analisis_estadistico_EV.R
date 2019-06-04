
##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Analisis estadistico y OLS
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: analisis_exploratorio.R
##################################################


# Analsis estadistico Acceso a espacio verdes -----
# creterios de excepcion de sectores urbanos a ser incluidos en el analsis de regresion:
# sectores sin personas
## sectores sin viviendas
# sectores area de espacio publico mayor que el 60 % del area del sector
# sectores area de calle mayor que el 80 % del area del sector
# sectores area privada mayor que el 90 % del area del sector

#library(ggrepel)

#var.criterios<-c("personas_edad","area_ep.porcentaje","area_calle.porcentaje","area_privada.porcentaje")

#histogramas EV -----



 # analisis.cali.df %>% 
 #   filter(is.na(area_copa)) %>%
 #   select(SETU_CCDGO) -> sin_arboles_censado
 
 

 su.exc.criteriosEV<- bind_rows(sin_personas,
                              sin_viviendas,
                              # ep_60,
                              privada_85,
                              calle_80)
 su.exc.EV<-bind_rows(su.exc.criteriosEV,data.frame(SETU_CCDGO=su.exc.apriori[-4])) #incluimos la laguna del pondaje
 
 ggplot()+
   geom_polygon(data = su.f,
                aes(x = long ,y = lat, group = group),
                fill = "grey70")+
   geom_polygon(data = subset(su.f, id %in% sin_personas$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "sin personas"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% sin_viviendas$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "sin viviendas"),
                alpha = 0.5)+
   # geom_polygon(data = subset(su.f, id %in% ep_60$SETU_CCDGO),
   #              aes(x = long ,y = lat, group = group,fill = "Ep > 60%"),
   #              alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% privada_85$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "Privado > 85%"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% calle_80$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "Calle > 80%"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% su.exc.apriori[-4]),
                aes(x = long ,y = lat, group = group,fill = "Apriori"),
                alpha = 0.5)+
   geom_text_repel( data = data.frame(centroides.su) %>% 
                filter( setu_ccgdo %in% su.exc.criterios$SETU_CCDGO),
              aes(x = x, y =y, label =setu_ccgdo))+
   
   coord_equal()+
   theme_void()

 
 
ggplot()+
   geom_polygon(data = su.f,
                aes(x = long ,y = lat, group = group),
                fill = "grey70")+
   geom_polygon(data = subset(su.f, id %in% su.exc.EV$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "descartados"),
                alpha = 0.5)+
   coord_equal()+
   theme_void()
 
regresion.EV<-analisis.cali.df %>% filter(!(SETU_CCDGO %in% su.exc.EV$SETU_CCDGO)) 
summary(regresion.EV)


#subconjuntos de variables ----
# Seleccionamos 
dependientes.EV<-metricas.acceso[c(2,6,9,13,14)]
dependientes.EV.sel<-metricas.acceso[c(9,13)]
# small multiple datos arboles -----
regresion.EV %>% select(one_of(dependientes.EV)) %>%
  mutate_all(ntile,10)->dep.EV.ntl

dep.EV.ntl<-regresion.EV %>% select(SETU_CCDGO) %>% bind_cols(dep.EV.ntl)

# datos en formato long para graficacion de small multiples

dep.EV.ntl.long<-gather(dep.EV.ntl, 
                             key =dep.EV,
                             value = valores,
                             dependientes.EV)


su.f %>% dplyr::select(-area_su)  %>%
  left_join(dep.EV.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(dep.EV %in% dependientes.EV) %>%
  ggplot()+
  geom_polygon(data = su.f, aes(x= long, y = lat, group = group), fill = "grey60")+
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
  facet_wrap(~dep.EV, nrow = 1)+
  tema_lgnd_abajo()

su.f %>% dplyr::select(-area_su)  %>%
  left_join(dep.EV.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(dep.EV %in% dependientes.EV.sel) %>%
  ggplot()+
  geom_polygon(data = su.f, aes(x= long, y = lat, group = group), fill = "grey60")+
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
  facet_wrap(~dep.EV, nrow = 1)+
  tema_lgnd_abajo()

# independientes EV
# este trabajo se enfoca en la relacion entre las variables sociales y de poblacion,
# resulta interesante ver si la combinacion de dimensiones explica mejor el desarrollo y distribucion
# de los EV en la ciudad.

# variables de poblacion ----

# en la medidad que se trata de relacionar varibles que expresen condiciones o
# dificutades de ciertos grupos poblacionales y condiciones de desarrollo en los sctores de la ciudad

#indep.poblacion.abs<- metricas.poblacion[c(2,3,4,7)]
# indep.poblacion.percent<- metricas.poblacion.mod[c(1,3,5,6,2)]

# indep.poblacion<- c(indep.poblacion.abs,indep.poblacion.percent)




# entre predictoras y dependientes no lineal
ggduo(regresion.EV,
      columnsX =indep.poblacion, 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor))
)

# # entre predictoras y dependientes no lineal
# ggduo(regresion.EV,
#       columnsX =indep.poblacion, 
#       columnsY =dependientes.EV.sel,
#       types = list(continuous = wrap(lm_with_cor,
#                                      method_cor = "spearman",
#                                      method_smooth= "loess"))
# )



pintar_corrmatrix_XY(regresion.EV,x=indep.poblacion, y=dependientes.EV.sel)+
  labs(title="Coeficiente Pearson entre dependiente e independientes")

pintar_corrmatrix_XY(regresion.EV,x=indep.poblacion, y=dependientes.EV.sel, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes")

# # histogramas 
# 
# regresion.EV %>% select(one_of(indep.poblacion)) %>%
#   gather( key = indep.poblacion,
#           value = valores,
#           1:length(indep.poblacion)) %>%
#   ggplot()+
#   geom_histogram(aes(x = valores),bins = 30, 
#                  color = "white", fill="steelblue")+
#   facet_wrap(~indep.poblacion, scales = "free", ncol = 3)
# 


# valores en m2
regresion.EV %>% select(one_of(dependientes.EV.sel)) %>%
  gather( key = dependientes.EV.sel,
          value = valores,
          1:length(dependientes.EV.sel)) %>%
  ggplot()+
  geom_histogram(aes(x = valores/max(valores) ),bins = 100, 
                 color = "white", fill="springgreen4")+
  facet_wrap(~dependientes.EV.sel, scales = "free", ncol = 1)
#sqrt(valores)
regresion.EV %>% select(one_of(dependientes.EV.sel)) %>%
  gather( key = dependientes.EV.sel,
          value = valores,
          1:length(dependientes.EV.sel)) %>%
  ggplot()+
  geom_histogram(aes(x = sqrt(valores)),bins = 30, 
                 color = "white", fill="springgreen4")+
  facet_wrap(~dependientes.EV.sel, scales = "free", ncol = 1)
#log(valores)
regresion.EV %>% select(one_of(dependientes.EV.sel)) %>%
  gather( key = dependientes.EV.sel,
          value = valores,
          1:length(dependientes.EV.sel)) %>%
  ggplot()+
  geom_histogram(aes(x = log(valores)),bins = 30, 
                 color = "white", fill="forestgreen")+
  facet_wrap(~dependientes.EV.sel, scales = "free", ncol = 1)


#seleccion de varibles dependientes a usar area copa ----
# evitaremaos usar variables altamente correlacionadas entre ellas
# buscado mantener los supuesto de una regresion lineal
# entre ellas priviliegiando las mejor relacionadas con base en las matriesde correlacion

indep.poblacion.ia.areas.dist.sel<-c("densidad_poblacion","con_alguna_limitacion.porcentaje")

indep.poblacion.area_ep.porcentaje.sel<-c("ningun_estudio.porcentaje")

indep.poblacion.EV.sel<-c(indep.poblacion.ia.areas.dist.sel,
                          indep.poblacion.area_ep.porcentaje.sel)
# histogramas 

regresion.EV %>% select(one_of(indep.poblacion.EV.sel)) %>%
  gather( key = indep.poblacion.EV.sel,
          value = valores,
          indep.poblacion.EV.sel) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~indep.poblacion.EV.sel, scales = "free", ncol = 3)

# entre predictoras y dependientes no lineal
ggduo(regresion.EV, #%>%
        # mutate_at(c(indep.poblacion.area_ep.porcentaje.sel,indep.poblacion.ia.areas.dist.sel),.funs = log),
      columnsX =c(indep.poblacion.area_ep.porcentaje.sel,indep.poblacion.ia.areas.dist.sel), 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)


#ninguna varieble de poblacion exhibe una nivel explicativo o correlacion fuerte.

# varibles de las otras dimensiones
############################
# varibles no poblacionales
####################


indep.predios<-metricas.predios[c(1,3,4,6:11,13:15)]
indep.estruct<-metricas.estruct[c(1:3,6,7,9:12)]
# correlaciones entre variables ----

pintar_corrmatrix(regresion.EV,indep.predios)+
  labs(title="Pearson entre carateristicas de uso de predios")

pintar_corrmatrix(regresion.EV,indep.predios, method_cor = "spearman")+
  labs(title="Spearman entre entre carateristicas de uso de predios")

 
pintar_corrmatrix_XY(regresion.EV,x=indep.predios, y=dependientes.EV.sel)+
  labs(title="Coeficiente Pearson entre dependiente EV e independientes uso de predios")

pintar_corrmatrix_XY(regresion.EV,x=indep.predios, y=dependientes.EV.sel, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente EV e independientes uso de predios")

# correlaciones entre variables ----

pintar_corrmatrix(regresion.EV,indep.estruct)+
  labs(title="Pearson entre carateristicas físicas de predios")

pintar_corrmatrix(regresion.EV,indep.estruct, method_cor = "spearman")+
  labs(title="Spearman entre entre carateristicas físicas de predios")

pintar_corrmatrix_XY(regresion.EV,x=indep.estruct, y=dependientes.EV.sel)+
  labs(title="Coeficiente Pearson entre dependientes EV e independientes físicas de predios")

pintar_corrmatrix_XY(regresion.EV,x=indep.estruct, y=dependientes.EV.sel, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependientes EV e independientes físicas de predios")

indep.predios_estruct.areas.dist.sel<-c("unidad_economica.porcentaje","area_media_manzana")
indep.predios_estruct.area_ep.ptje.sel<-c("area_media_manzana","cuarto.porcentaje")
indep.predios_estruct.sel<-c(indep.predios_estruct.area_ep.ptje.sel,indep.predios_estruct.areas.dist.sel) %>% unique()

# ggpairs(  regresion.EV[,indep.predios_estruct.sel] %>% mutate_at(indep.predios_estruct.sel,.funs = log),
#           lower = list(continuous = wrap(lowerFn, method = "lm")),
#   diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
#   upper = list(continuous = wrap("cor", size = 3))
# )
# 
# ggpairs(  regresion.EV[,indep.predios_estruct.sel],
#           # %>% mutate_at(indep.predios_estruct.sel,.funs = log),
#           lower = list(continuous = wrap(lowerFn, method = "lm")),
#           diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
#           upper = list(continuous = wrap("cor", size = 3))
# )


# entre predictoras y version porcentual
ggduo(regresion.EV %>% 
#        mutate_at(indep.predios_estruct.sel,.funs = sqrt) %>%
        mutate_at(dependientes.EV,.funs = sqrt),
      columnsX =indep.predios_estruct.sel, 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y version porcentual
ggduo(regresion.EV,
      columnsX =indep.predios_estruct.sel, 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor))
)



ggduo(regresion.EV ,
      columnsX =indep.predios_estruct.sel, 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)


pintar_corrmatrix(regresion.EV,indep.predios_estruct.sel)+
  labs(title="Pearson entre carateristicas de predios y EV")

pintar_corrmatrix(regresion.EV,indep.predios_estruct.sel, method_cor = "spearman")+
  labs(title="Spearman entre entre carateristicas de predios y EV")



pintar_corrmatrix_XY(regresion.EV ,
                     x=indep.predios_estruct.sel, y=dependientes.EV.sel)+
  labs(title="Coeficiente Pearson entre dependiente e independientes")

pintar_corrmatrix_XY(regresion.EV,
                     x=indep.predios_estruct.sel, y=dependientes.EV.sel, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes")

# Consolidar predictores -----
independientes.EV<-c(indep.predios_estruct.sel,
                  indep.poblacion.ia.areas.dist.sel,
                  indep.poblacion.area_ep.porcentaje.sel) %>% unique()

pintar_corrmatrix(regresion.EV,independientes.EV)+
  labs(title="Pearson entre independientes seleccionadas EV")

pintar_corrmatrix(regresion.EV,independientes.EV, method_cor = "spearman")+
  labs(title="Spearman entre independientes seleccionadas EV")



pintar_corrmatrix_XY(regresion.EV ,
                     x=independientes.EV, y=dependientes.EV.sel)+
  labs(title="Coeficiente Pearson entre dependiente e independientes para EV")

pintar_corrmatrix_XY(regresion.EV,
                     x=independientes.EV, y=dependientes.EV.sel, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes para EV")

ggduo(regresion.EV ,
      columnsX =independientes.EV, 
      columnsY =dependientes.EV.sel,
      types = list(continuous = wrap(lm_with_cor))
)+
  theme(strip.text.x = element_text(size = 7))

# ggduo(regresion.EV ,
#       columnsX =independientes.EV, 
#       columnsY =dependientes.EV.sel,
#       types = list(continuous = wrap(lm_with_cor,
#                                      method_cor = "spearman",
#                                      method_smooth= "loess"))
# )


########### los nuevos modelos
# introducimos las varibles nuevas y vemos que resultados se obtiene.
dependiente <- "area_ep.porcentaje"
#independientes  <- c(indep.predios_estruct.area_ep.ptje.sel,indep.poblacion.area_ep.porcentaje.sel)
independientes  <- independientes.EV
# max normalizado 
var_names<-c(dependiente,names(regresion.EV[,independientes]))
regresion.EV.mn<-max_nomalization(regresion.EV,var_names)
lm.area_ep.ptje<-lm(area_ep.porcentaje.mxn~cuarto.porcentaje.mxn+
                      unidad_economica.porcentaje.mxn+
                      area_media_manzana.mxn+
                      densidad_poblacion.mxn+
                      ningun_estudio.porcentaje.mxn+
                      con_alguna_limitacion.porcentaje.mxn,
                     data = regresion.EV.mn)
sm<-summary(lm.area_ep.ptje)
sm
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.area_ep.ptje$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
# autoplot(lm.area_ep.ptje, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.area_ep.ptje) # la varianza de los residuos no es constante 
shapiro.test(lm.area_ep.ptje$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.area_ep.ptje))) 
# 


AIC(lm.area_ep.ptje)

pintar_mapa_su_lm(regresion.EV,lm.area_ep.ptje,nrow =1)
#pintar_mapa_su_lm_ntl(regresion.EV,lm.area_ep.ptje,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.area_ep.ptje)
lm_augment$SETU_CCDGO<-regresion.EV$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)

best_models<-ols_step_best_subset(lm(area_ep.porcentaje.mxn~cuarto.porcentaje.mxn+
                                  unidad_economica.porcentaje.mxn+
                                  area_media_manzana.mxn+
                                  densidad_poblacion.mxn+
                                  ningun_estudio.porcentaje.mxn+
                                  con_alguna_limitacion.porcentaje.mxn,
                                data = regresion.EV.mn))
best_models
plot(best_models)

lm.area_ep.ptje.sel<-lm(area_ep.porcentaje.mxn~cuarto.porcentaje.mxn+
                      # unidad_economica.porcentaje.mxn+
                      area_media_manzana.mxn#+
                      # densidad_poblacion.mxn+
                      #ningun_estudio.porcentaje.mxn
                      ,
                    data = regresion.EV.mn)


pintar_mapa_su_lm2(regresion.EV,lm.area_ep.ptje.sel,nrow =1)
summary(lm.area_ep.ptje.sel)
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.area_ep.ptje.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
# autoplot(lm.area_ep.ptje.sel, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.area_ep.ptje.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.area_ep.ptje.sel$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.area_ep.ptje.sel))) 
# 


AIC(lm.area_ep.ptje.sel)

## ia.areas.dist

dependiente <- "ia.areas.dist"
independientes  <- independientes.EV
# max normalizado 
var_names<-c(dependiente,names(regresion.EV[,independientes]))
regresion.EV.mn<-max_nomalization(regresion.EV,var_names)
lm.ia.areas.dist<-lm(ia.areas.dist.mxn~cuarto.porcentaje.mxn+
                       unidad_economica.porcentaje.mxn+
                       area_media_manzana.mxn+
                       densidad_poblacion.mxn+
                       ningun_estudio.porcentaje.mxn+
                       con_alguna_limitacion.porcentaje.mxn,
                    data = regresion.EV.mn)

summary(lm.ia.areas.dist)
sm<-summary(lm.ia.areas.dist)
sm
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.ia.areas.dist$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
# autoplot(lm.ia.areas.dist, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.ia.areas.dist) # la varianza de los residuos no es constante 
shapiro.test(lm.ia.areas.dist$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.ia.areas.dist))) 
# 


AIC(lm.ia.areas.dist)

pintar_mapa_su_lm(regresion.EV,lm.ia.areas.dist,nrow =1)
 
#pintar_mapa_su_lm_ntl(regresion.EV,lm.ia.areas.dist,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.ia.areas.dist)
lm_augment$SETU_CCDGO<-regresion.EV$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)

best_models<-ols_step_best_subset(lm(ia.areas.dist.mxn~cuarto.porcentaje.mxn+
                                  unidad_economica.porcentaje.mxn+
                                  area_media_manzana.mxn+
                                  densidad_poblacion.mxn+
                                  ningun_estudio.porcentaje.mxn+
                                  con_alguna_limitacion.porcentaje.mxn,
                                data = regresion.EV.mn))
best_models
plot(best_models)



lm.ia.areas.dist.sel<-lm(ia.areas.dist.mxn~cuarto.porcentaje.mxn+
                           unidad_economica.porcentaje.mxn+
                           area_media_manzana.mxn+
                           densidad_poblacion.mxn+
                           ningun_estudio.porcentaje.mxn+
                           con_alguna_limitacion.porcentaje.mxn,
                     data = regresion.EV.mn)

summary(lm.ia.areas.dist.sel)
sm<-summary(lm.ia.areas.dist.sel)
sm
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.ia.areas.dist.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
# autoplot(lm.ia.areas.dist.sel, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.ia.areas.dist.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.ia.areas.dist.sel$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.ia.areas.dist.sel))) 
# 


AIC(lm.ia.areas.dist.sel)

pintar_mapa_su_lm(regresion.EV,lm.ia.areas.dist.sel,nrow =1)

pintar_mapa_su_lm_ntl(regresion.EV,lm.ia.areas.dist.sel,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.ia.areas.dist.sel)
lm_augment$SETU_CCDGO<-regresion.EV$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
# modelar  ----










