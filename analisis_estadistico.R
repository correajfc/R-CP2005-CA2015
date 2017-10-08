
##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Analisis estadistico y OLS
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: analisis_exploratorio.R
##################################################


# Analsis estadistico cobertura de copa -----
# creterios de excepcion de sectores urbanos a ser incluidos en el analsis de regresion:
# sectores sin personas
## sectores sin viviendas
# sectores area de espacio publico mayor que el 60 % del area del sector
# sectores area de calle mayor que el 80 % del area del sector
# sectores area privada mayor que el 90 % del area del sector

library(ggrepel)

var.criterios<-c("personas_edad","area_ep.porcentaje","area_calle.porcentaje","area_privada.porcentaje")

#histogramas arboles -----

# variable escogencia de criterios 
analisis.cali.df %>% select(one_of(var.criterios)) %>%
  gather( key = var.criterios,
          value = valores,
          1:4) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 20,
                 color = "white", fill="red")+
  facet_wrap(~var.criterios, scales = "free")

# mapas de las varobles de seleccion 
 su.f %>% dplyr::select(-area_su)  %>%
   left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
   ggplot()+
   geom_polygon(aes(x= long, y = lat, group = group, fill = area_calle.porcentaje))+
   coord_equal()+
   theme_void()+
   scale_fill_viridis( direction = 1 )
 
 su.f %>% dplyr::select(-area_su)  %>%
   left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
   ggplot()+
   geom_polygon(aes(x= long, y = lat, group = group, fill = personas_edad))+
   coord_equal()+
   theme_void()+
   scale_fill_viridis( direction = 1 )
 
 su.f %>% dplyr::select(-area_su)  %>%
   left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
   ggplot()+
   geom_polygon(aes(x= long, y = lat, group = group, fill = area_ep.porcentaje))+
   coord_equal()+
   theme_void()+
   scale_fill_viridis( direction = 1 )
 
 su.f %>% dplyr::select(-area_su)  %>%
   left_join(analisis.cali.df,by = c("id"="SETU_CCDGO")) %>%
   ggplot()+
   geom_polygon(aes(x= long, y = lat, group = group, fill = area_privada.porcentaje))+
   coord_equal()+
   theme_void()+
   scale_fill_viridis( direction = 1 )


 
 analisis.cali.df %>% 
   filter(is.na(personas_edad)) %>%
   select(SETU_CCDGO) -> sin_personas
 
 analisis.cali.df %>% 
   filter(uso_vivienda == 0) %>%
   select(SETU_CCDGO) -> sin_viviendas

 analisis.cali.df %>% 
   filter(area_ep.porcentaje > 0.6 ) %>%
   select(SETU_CCDGO)  -> ep_60
 
 analisis.cali.df %>% 
   filter(area_privada.porcentaje > 0.85 ) %>%
   select(SETU_CCDGO) -> privada_85
 
 analisis.cali.df %>% 
   filter(area_calle.porcentaje > 0.8 ) %>%
   select(SETU_CCDGO) -> calle_80
 
 analisis.cali.df %>% 
   filter(is.na(area_copa)) %>%
   select(SETU_CCDGO) -> sin_arboles_censado
 
 
 su.exc.apriori<-c("0204","1736",# sector con alto porcentaje no urbanizado
                   "1709",# maroria del area por fuera del perimetro urbano
                   "1317")# laguna del pandaje
 
 su.exc.criterios<- bind_rows(sin_personas,
                              sin_viviendas,
                              ep_60,
                              privada_85,
                              calle_80,
                              sin_arboles_censado)
 su.exc<-bind_rows(su.exc.criterios,data.frame(SETU_CCDGO=su.exc.apriori))
 
 ggplot()+
   geom_polygon(data = su.f,
                aes(x = long ,y = lat, group = group),
                fill = "grey70")+
   geom_polygon(data = subset(su.f, id %in% sin_arboles_censado$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "sin arboles censados"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% sin_personas$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "sin personas"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% sin_viviendas$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "sin viviendas"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% ep_60$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "Ep > 60%"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% privada_85$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "Privado > 85%"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% calle_80$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "Calle > 80%"),
                alpha = 0.5)+
   geom_polygon(data = subset(su.f, id %in% su.exc.apriori),
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
   geom_polygon(data = subset(su.f, id %in% su.exc$SETU_CCDGO),
                aes(x = long ,y = lat, group = group,fill = "descartados"),
                alpha = 0.5)+
   coord_equal()+
   theme_void()
 
regresion.arboles<-analisis.cali.df %>% filter(!(SETU_CCDGO %in% su.exc$SETU_CCDGO)) 
summary(regresion.arboles)


#subconjuntos de variables ----
dependientes.arboles<-metricas.ca[c(1,6)]

# small multiple datos arboles -----
regresion.arboles %>% select(one_of(dependientes.arboles)) %>%
  mutate_all(ntile,10)->dep.arboles.ntl

dep.arboles.ntl<-regresion.arboles %>% select(SETU_CCDGO) %>% bind_cols(dep.arboles.ntl)

# datos en formato long para graficacion de small multiples

dep.arboles.ntl.long<-gather(dep.arboles.ntl, 
                             key =dep.arboles,
                             value = valores,
                             area_copa:cobertura_copa.ap)


su.f %>% dplyr::select(-area_su)  %>%
  left_join(dep.arboles.ntl.long,by = c("id"="SETU_CCDGO")) %>%
  filter(dep.arboles %in% dependientes.arboles) %>%
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
  facet_wrap(~dep.arboles, nrow = 1)+
  tema_lgnd_abajo()

# independientes arboles
# este trabajo se enfoca en la relacion entre las variables sociales y de poblacion,
# resulta interesante ver si la combinacion de dimensiones explica mejor el desarrollo y distribucion
# de los arboles en la ciudad.

# variables de poblacion ----

# en la medidad que se trata de relacionar varibles que expresen condiciones o
# dificutades de ciertos grupos poblacionales y condiciones de desarrollo en los sctores de la ciudad

indep.poblacion.abs<- metricas.poblacion[c(2,3,4,7)]
indep.poblacion.percent<- metricas.poblacion.mod[c(1,3,5,6,2)]

indep.poblacion<- c(indep.poblacion.abs,indep.poblacion.percent)




# disibuciones bivariadas entre variables independientes
#  colinealidad
ggpairs(
  regresion.arboles[,indep.poblacion], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)


ggpairs(
  regresion.arboles[,indep.poblacion.abs], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

ggpairs(
  regresion.arboles[,indep.poblacion.percent], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

# entre predictoras y version porcentual
ggduo(regresion.arboles,
      columnsX =indep.poblacion.abs, 
      columnsY =indep.poblacion.percent,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y version porcentual no lineal
ggduo(regresion.arboles,
      columnsX =indep.poblacion.abs, 
      columnsY =indep.poblacion.percent,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)
# correlaciones entre variables ----

pintar_corrmatrix(regresion.arboles,indep.poblacion)+
  labs(title="Coeficiente Pearson entre varibles de población")

pintar_corrmatrix(regresion.arboles,indep.poblacion, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre varibles de población")

pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion.abs, y=indep.poblacion.percent)+
  labs(title="Coeficiente Pearson dependientes y versiones porcentuales")

 pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion.abs, y=indep.poblacion.percent,method_cor = "spearman")+
  labs(title="Coeficiente Spearman dependientes y versiones porcentuales")



# entre predictoras y dependientes no lineal
ggduo(regresion.arboles,
      columnsX =indep.poblacion, 
      columnsY =dependientes.arboles,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y dependientes no lineal
ggduo(regresion.arboles,
      columnsX =indep.poblacion, 
      columnsY =dependientes.arboles,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)



pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion, y=dependientes.arboles)+
  labs(title="Coeficiente Pearson entre dependiente e independientes")

pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion, y=dependientes.arboles, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes")

# histogramas 

regresion.arboles %>% select(one_of(indep.poblacion)) %>%
  gather( key = indep.poblacion,
          value = valores,
          1:length(indep.poblacion)) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~indep.poblacion, scales = "free", ncol = 3)



# valores en m2
regresion.arboles %>% select(one_of(dependientes.arboles)) %>%
  gather( key = dependientes.arboles,
          value = valores,
          1:length(dependientes.arboles)) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="forestgreen")+
  facet_wrap(~dependientes.arboles, scales = "free", ncol = 1)
#sqrt(valores)
regresion.arboles %>% select(one_of(dependientes.arboles)) %>%
  gather( key = dependientes.arboles,
          value = valores,
          1:length(dependientes.arboles)) %>%
  ggplot()+
  geom_histogram(aes(x = sqrt(valores)),bins = 30, 
                 color = "white", fill="forestgreen")+
  facet_wrap(~dependientes.arboles, scales = "free", ncol = 1)
#log(valores)
regresion.arboles %>% select(one_of(dependientes.arboles)) %>%
  gather( key = dependientes.arboles,
          value = valores,
          1:length(dependientes.arboles)) %>%
  ggplot()+
  geom_histogram(aes(x = log(valores)),bins = 30, 
                 color = "white", fill="forestgreen")+
  facet_wrap(~dependientes.arboles, scales = "free", ncol = 1)


#seleccion de varibles dependientes a usar area copa ----
# evitaremaos usar variables altamente correlacionadas entre ellas
# buscado mantener los supuesto de una regresion lineal
# entre ellas priviliegiando las mejor relacionadas con base en las matriesde correlacion

indep.poblacion.copa.sel<-c("superior_postgrado", 
                           "densidad_poblacion",
                           "con_alguna_limitacion.porcentaje",
                           "afro.porcentaje")

indep.poblacion.copa.ap.sel<-c("superior_postgrado.porcentaje",
                               "densidad_poblacion",
                               "con_alguna_limitacion.porcentaje",
                               "afro.porcentaje")


# histogramas 

regresion.arboles %>% select(one_of(indep.poblacion)) %>%
  gather( key = indep.poblacion,
          value = valores,
          1:length(indep.poblacion)) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~indep.poblacion, scales = "free", ncol = 3)


# relaciones inversas pueden estar relacionadas mejor con el inverso del indicador
# calulemos algunas las que potencialmente se comportan asi
# regresion.arboles<-regresion.arboles %>% 
#   mutate(afro.inv = 1/negro_mulato_afrocolombiano) %>%
#   mutate(ningun_estudio.inv =1/ningun_estudio) %>%
#   mutate(con_alguna_limitacion.inv =1/(con_alguna_limitacion+1)) %>%# evita valores infinitos
#   mutate()

inv.var<-c(indep.poblacion)
inv.var.name<-lapply(inv.var,paste0,".inv") %>% as.character()
regresion.arboles<-regresion.arboles[,inv.var]%>%
  mutate_all(funs(1/(.+1))) %>% # sumamos uno para evitar los valores infinitos
  setNames(nm = inv.var.name)%>% 
  bind_cols(regresion.arboles,.)
summary(regresion.arboles)


# entre predictoras y dependientes
ggduo(regresion.arboles,
      columnsX =inv.var.name, 
      columnsY =dependientes.arboles,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)

ggpairs(
  regresion.arboles[,inv.var.name], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)



indep.poblacion.area_copa<-c("superior_postgrado",
                             "densidad_poblacion.inv",
                             "con_alguna_limitacion.porcentaje.inv",
                             "afro.porcentaje.inv")

indep.poblacion.cobertura.ap<-c("superior_postgrado.porcentaje",
                                "densidad_poblacion.inv",
                                "con_alguna_limitacion.porcentaje.inv",
                                "afro.porcentaje.inv")

# tranformacion de varibles 
regresion.arboles$log.area_copa<-log(regresion.arboles$area_copa)
regresion.arboles$sqrt.area_copa<-sqrt(regresion.arboles$area_copa)

# regresion.arboles$log.cobertura_copa.ap<-log(regresion.arboles$cobertura_copa.ap)
regresion.arboles$sqrt.cobertura_copa.ap<-sqrt(regresion.arboles$cobertura_copa.ap)

dependientes.copa.trnsf<-c("area_copa","log.area_copa","sqrt.area_copa")
dependientes.copa.ap.trnsf<-c("cobertura_copa.ap","sqrt.cobertura_copa.ap")

# entre predictoras y dependientes
ggduo(regresion.arboles,
      columnsX =c("superior_postgrado",indep.poblacion.cobertura.ap), 
      columnsY =dependientes.copa.trnsf,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y dependientes
ggduo(regresion.arboles,
      columnsX =c("superior_postgrado",indep.poblacion.cobertura.ap), 
      columnsY =dependientes.copa.ap.trnsf,
      types = list(continuous = wrap(lm_with_cor))
)


# histogramas 

regresion.arboles %>% select(one_of(c("superior_postgrado",indep.poblacion.cobertura.ap))) %>%
  gather( key = indep.poblacion,
          value = valores,
          1:(length(indep.poblacion.cobertura.ap)+1)) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="steelblue")+
  facet_wrap(~indep.poblacion, scales = "free", ncol = 2)

# histogramas 

regresion.arboles %>% select(one_of(c(dependientes.copa.trnsf,dependientes.copa.ap.trnsf))) %>%
  gather( key = indep.poblacion,
          value = valores,
          1:5) %>%
  ggplot()+
  geom_histogram(aes(x = valores),bins = 30, 
                 color = "white", fill="forestgreen")+
  facet_wrap(~indep.poblacion, scales = "free", nrow = 2)


# varibles de las otras dimensiones


# modelar la cobertura de copa ----
# modelo area de copa


dependiente <- "area_copa"
independientes  <- indep.poblacion.area_copa
f.area_copa.sel<-as.formula(paste(dependiente, paste(independientes, collapse=" + "), sep=" ~ "))

lm.area_copa.sel<-lm(formula = f.area_copa.sel, data = regresion.arboles )

summary(lm.area_copa.sel)
#test de ajuste
mean(lm.area_copa.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.area_copa.sel, which = 1:4)
ggnostic(lm.area_copa.sel)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.area_copa.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.area_copa.sel$residuals) # los residuos no vienende una distribucion normal
ggplot() + geom_density(aes(residuals(lm.area_copa.sel))) 
car::vif(lm.area_copa.sel)


# mapas del modelo
# no podemos confiar mucho en los resultados del modelo linael pues los supuestos no se cumplen.

pintar_mapa_su_lm(regresion.arboles,lm.area_copa.sel,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.area_copa.sel,num_tiles = 5)
# mapas de residuos estandarizados
lm_augment<-augment(lm.area_copa.sel)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
# combinaciones de las varibles del modelo
best_models<-ols_best_subset(lm.area_copa.sel)
best_models
plot(best_models)

# Max nomalization model
dependiente <- "area_copa"
independientes  <- indep.poblacion.area_copa

var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mn.area_copa.sel<-crear_lm_from_df(regresion.arboles.mn)

sm<-summary(lm.mn.area_copa.sel)
sm
#MSE
mean(sm$residuals^2)
#test de ajuste
mean(lm.mn.area_copa.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mn.area_copa.sel, which = 1:4)
ggnostic(lm.mn.area_copa.sel)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mn.area_copa.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.mn.area_copa.sel$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mn.area_copa.sel))) 


pintar_mapa_su_lm(regresion.arboles.mn,lm.mn.area_copa.sel,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles.mn,lm.mn.area_copa.sel,num_tiles = 10)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mn.area_copa.sel)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
best_models<-ols_best_subset(lm(formula = formula(lm.mn.area_copa.sel),data=lm.mn.area_copa.sel$model))
best_models
plot(best_models)

#variaciones del modelo para obtener un ajuste mmejor

dependiente <- "log.area_copa"
independientes  <- indep.poblacion.area_copa

# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mxn.log.area_copa.sel<-crear_lm_from_df(regresion.arboles.mn)
modelo<-crear_lm_from_df(regresion.arboles.mn)
sm<-summary(lm.mxn.log.area_copa.sel)
sm
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.mxn.log.area_copa.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mxn.log.area_copa.sel, which = 1:4)
ggnostic(lm.mxn.log.area_copa.sel)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mxn.log.area_copa.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.mxn.log.area_copa.sel$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mxn.log.area_copa.sel))) 



pintar_mapa_su_lm(regresion.arboles,lm.mxn.log.area_copa.sel,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mxn.log.area_copa.sel,num_tiles = 5)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mxn.log.area_copa.sel)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)

# combinaciones de las varibles del modelo

best_models<-ols_best_subset(lm(formula = formula(lm.mxn.log.area_copa.sel),data=lm.mxn.log.area_copa.sel$model))
best_models
plot(best_models)

# sqrt model
#regresion.arboles$sqrt.area_copa<-sqrt(regresion.arboles$area_copa)
dependiente <- "sqrt.area_copa"
independientes  <- indep.poblacion.area_copa
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mxn.sqrt.area_copa.sel<-crear_lm_from_df(regresion.arboles.mn)
summary(lm.mxn.sqrt.area_copa.sel)
#test de ajuste
mean(lm.mxn.sqrt.area_copa.sel$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mxn.sqrt.area_copa.sel, which = 1:4)
ggnostic(lm.mxn.sqrt.area_copa.sel)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mxn.sqrt.area_copa.sel) # la varianza de los residuos no es constante 
shapiro.test(lm.mxn.sqrt.area_copa.sel$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mxn.sqrt.area_copa.sel))) 


pintar_mapa_su_lm(regresion.arboles,lm.mxn.sqrt.area_copa.sel,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mxn.sqrt.area_copa.sel,num_tiles = 5)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mxn.sqrt.area_copa.sel)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
# combinaciones de las varibles del modelo

best_models<-ols_best_subset(lm(formula = formula(lm.mxn.sqrt.area_copa.sel),
                                data=lm.mxn.sqrt.area_copa.sel$model))
best_models
plot(best_models)

##### cobertura copa area publica


dependiente <- "cobertura_copa.ap"
independientes  <- indep.poblacion.cobertura.ap
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mxn.cobertura_copa.ap<-crear_lm_from_df(regresion.arboles.mn)
summary(lm.mxn.cobertura_copa.ap)
#test de ajuste
mean(lm.mxn.cobertura_copa.ap$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mxn.cobertura_copa.ap, which = 1:4)
ggnostic(lm.mxn.cobertura_copa.ap)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mxn.cobertura_copa.ap) # la varianza de los residuos no es constante 
shapiro.test(lm.mxn.cobertura_copa.ap$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mxn.cobertura_copa.ap))) 


pintar_mapa_su_lm(regresion.arboles,lm.mxn.cobertura_copa.ap,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mxn.cobertura_copa.ap,num_tiles = 5)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mxn.cobertura_copa.ap)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
# combinaciones de las varibles del modelo

best_models<-ols_best_subset(lm(formula = formula(lm.mxn.cobertura_copa.ap),
                                data=lm.mxn.cobertura_copa.ap$model))
best_models
plot(best_models)



dependiente <- "sqrt.cobertura_copa.ap"
independientes  <- indep.poblacion.cobertura.ap
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mxn.sqrt.cobertura_copa.ap<-crear_lm_from_df(regresion.arboles.mn)
summary(lm.mxn.sqrt.cobertura_copa.ap)
#test de ajuste
mean(lm.mxn.sqrt.cobertura_copa.ap$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mxn.sqrt.cobertura_copa.ap, which = 1:4)
ggnostic(lm.mxn.sqrt.cobertura_copa.ap)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mxn.sqrt.cobertura_copa.ap) # la varianza de los residuos no es constante 
shapiro.test(lm.mxn.sqrt.cobertura_copa.ap$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mxn.sqrt.cobertura_copa.ap))) 



pintar_mapa_su_lm(regresion.arboles,lm.mxn.sqrt.cobertura_copa.ap,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mxn.sqrt.cobertura_copa.ap,num_tiles = 5)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mxn.sqrt.cobertura_copa.ap)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)
# combinaciones de las varibles del modelo

best_models<-ols_best_subset(lm(formula = formula(lm.mxn.sqrt.cobertura_copa.ap),
                                data=lm.mxn.sqrt.cobertura_copa.ap$model))
best_models
plot(best_models)



# mejores modelos de poblacion ----

dependiente <- "log.area_copa"
independientes  <- c("superior_postgrado","densidad_poblacion")
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.best.area_copa<-lm(log.area_copa.mxn~superior_postgrado.mxn+densidad_poblacion.mxn,data = regresion.arboles.mn)
summary(lm.best.area_copa)

#test de ajuste
mean(lm.best.area_copa$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
dev.off()
autoplot(lm.best.area_copa, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.best.area_copa) # la varianza de los residuos no es constante 
shapiro.test(lm.best.area_copa$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.best.area_copa))) 



pintar_mapa_su_lm(regresion.arboles,lm.best.area_copa,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.best.area_copa,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.best.area_copa)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)


# coberbtura AP
dependiente <- "cobertura_copa.ap"
independientes  <- c("superior_postgrado.porcentaje")
# max normalizado 
var_names<-c(dependiente,independientes)
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.best.cobertura.ap<-lm(cobertura_copa.ap.mxn~superior_postgrado.porcentaje.mxn,
                         data = regresion.arboles.mn)
summary(lm.best.cobertura.ap)

#test de ajuste
mean(lm.best.cobertura.ap$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.best.cobertura.ap, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.best.cobertura.ap) # la varianza de los residuos no es constante 
shapiro.test(lm.best.cobertura.ap$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.best.cobertura.ap))) 
# 


pintar_mapa_su_lm(regresion.arboles,lm.best.cobertura.ap,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.best.cobertura.ap,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.best.cobertura.ap)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)


############################
# varibles no poblacionales
####################


indep.predios<-c(metricas.predios[c(13:15,9:11)],"area_ep","area_ep.porcentaje")

# correlaciones entre variables ----

pintar_corrmatrix(regresion.arboles,indep.predios)+
  labs(title="Pearson entre carateristicas de predios y EV")

pintar_corrmatrix(regresion.arboles,indep.predios, method_cor = "spearman")+
  labs(title="Spearman entre entre carateristicas de predios y EV")

pintar_corrmatrix_XY(regresion.arboles,x=indep.predios, y=dependientes.arboles)+
  labs(title="Coeficiente Pearson entre dependiente e independientes")

pintar_corrmatrix_XY(regresion.arboles,x=indep.predios, y=dependientes.arboles, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes")


indep.predios.copa.sel<-c("cuarto.porcentaje","area_ep")
indep.predios.copa.ap.sel<-c("apartamento.porcentaje","cuarto.porcentaje","area_ep.porcentaje")
indep.predios.sel<-c("apartamento.porcentaje","cuarto.porcentaje","area_ep.porcentaje","area_ep")

ggpairs(
  regresion.arboles[,indep.predios.sel], lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "white",fill ="steelblue")),
  upper = list(continuous = wrap("cor", size = 3))
)

# entre predictoras y version porcentual
ggduo(regresion.arboles,
      columnsX =indep.predios.sel, 
      columnsY =dependientes.arboles,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y version porcentual no lineal
ggduo(regresion.arboles,
      columnsX =indep.predios.sel, 
      columnsY =dependientes.arboles,
      types = list(continuous = wrap(lm_with_cor,
                                     method_cor = "spearman",
                                     method_smooth= "loess"))
)


# entre predictoras y dependientes
ggduo(regresion.arboles,
      columnsX =indep.predios.copa.sel, 
      columnsY =dependientes.copa.trnsf,
      types = list(continuous = wrap(lm_with_cor))
)

# entre predictoras y dependientes
ggduo(regresion.arboles,
      columnsX =indep.predios.copa.ap.sel, 
      columnsY =dependientes.copa.ap.trnsf,
      types = list(continuous = wrap(lm_with_cor))
)

########### los nuevos modelos
# introducimos las varibles nuevas y vemos que resultados se obtiene.
dependiente <- "log.area_copa"
independientes  <- c("superior_postgrado","densidad_poblacion",indep.predios.copa.sel)
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mod.area_copa<-lm(log.area_copa.mxn~superior_postgrado.mxn+
                        densidad_poblacion.mxn+
                        cuarto.porcentaje.mxn+
                        area_ep.mxn,
                      data = regresion.arboles.mn)
sm<-summary(lm.mod.area_copa)
sm
#MSE
mean(sm$residuals^2)

#test de ajuste
mean(lm.mod.area_copa$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mod.area_copa, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mod.area_copa) # la varianza de los residuos no es constante 
shapiro.test(lm.mod.area_copa$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mod.area_copa))) 
# 


AIC(lm.mod.area_copa)
AIC(lm.best.area_copa)

pintar_mapa_su_lm(regresion.arboles,lm.mod.area_copa,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mod.area_copa,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mod.area_copa)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)

best_models<-ols_best_subset(lm(log.area_copa.mxn~superior_postgrado.mxn+
                                  densidad_poblacion.mxn+
                                  cuarto.porcentaje.mxn+
                                  area_ep.mxn,
                                data = lm.mod.area_copa$model))
best_models
plot(best_models)


pintar_mapa_su_lm2(regresion.arboles,lm.mod.area_copa,nrow =1)

# coberbtura AP
dependiente <- "cobertura_copa.ap"
independientes  <- c("superior_postgrado.porcentaje",indep.predios.copa.ap.sel)
# max normalizado 
var_names<-c(dependiente,independientes)
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
lm.mod.cobertura.ap<-lm(cobertura_copa.ap.mxn~superior_postgrado.porcentaje.mxn+
                          apartamento.porcentaje.mxn+
                          cuarto.porcentaje.mxn+
                          area_ep.porcentaje.mxn,
                        data = regresion.arboles.mn)
sm<-summary(lm.mod.cobertura.ap)
sm
#MSE
mean(sm$residuals^2)
#test de ajuste
mean(lm.mod.cobertura.ap$residuals) # media de los residuos cercana a 0 (si)
# Homocedasticidad de los residuos o varianza igual
autoplot(lm.mod.cobertura.ap, which = 1:4)
#ggnostic(lm.best.area_copa)
# aun un amuento de la varianza. hagamos un test para verificar este aumento
lmtest::bptest(lm.mod.cobertura.ap) # la varianza de los residuos no es constante 
shapiro.test(lm.mod.cobertura.ap$residuals) # los residuos no exhiben una distribucion normal
ggplot() + geom_density(aes(residuals(lm.mod.cobertura.ap))) 


pintar_mapa_su_lm(regresion.arboles,lm.mod.cobertura.ap,nrow =1)
pintar_mapa_su_lm_ntl(regresion.arboles,lm.mod.cobertura.ap,num_tiles = 6)
# mapas de residuos estandarizados
lm_augment<-augment(lm.mod.cobertura.ap)
lm_augment$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_augment,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(.std.resid,n = 5))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu", drop = FALSE)

best_models<-ols_best_subset(lm(cobertura_copa.ap.mxn~superior_postgrado.porcentaje.mxn+
                                  apartamento.porcentaje.mxn+
                                  cuarto.porcentaje.mxn+
                                  area_ep.porcentaje.mxn,
                                data = lm.mod.cobertura.ap$model))
best_models
plot(best_models)

# es mejor el modelo solo de poblacion para el porcenatje de cobertura de copa.
# lm.best.cobertura.ap

######## acceso a EV #########







