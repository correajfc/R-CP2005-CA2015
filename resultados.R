# resultados estadisticos 

## cobertura y area de copa 

indep.poblacion.abs<- metricas.poblacion[c(2,3,4,7)]
indep.poblacion.percent<- metricas.poblacion.mod[c(1,3,5,6,2)]

indep.poblacion<- c(indep.poblacion.abs,indep.poblacion.percent)

pintar_corrmatrix(regresion.arboles,indep.poblacion)+
  labs(title="Coeficiente Pearson entre varibles de población")

pintar_corrmatrix(regresion.arboles,indep.poblacion, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre varibles de población")


pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion, y=dependientes.arboles)+
  labs(title="Coeficiente Pearson entre dependiente e independientes (población)")

pintar_corrmatrix_XY(regresion.arboles,x=indep.poblacion, y=dependientes.arboles, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes (población)")


indep.poblacion.copa.sel<-c("superior_postgrado", 
                            "densidad_poblacion",
                            "con_alguna_limitacion.porcentaje",
                            "afro.porcentaje")

indep.poblacion.copa.ap.sel<-c("superior_postgrado.porcentaje",
                               "densidad_poblacion",
                               "con_alguna_limitacion.porcentaje",
                               "afro.porcentaje")


indep.predios<-c(metricas.predios[c(13:15,9:11)],"area_ep","area_ep.porcentaje","area_media_manzana")


pintar_corrmatrix(regresion.arboles,indep.predios)+
  labs(title="Coeficiente Pearson entre varibles de predios")

pintar_corrmatrix(regresion.arboles,indep.predios, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre varibles de predios")


pintar_corrmatrix_XY(regresion.arboles,x=indep.predios, y=dependientes.arboles)+
  labs(title="Coeficiente Pearson entre dependiente e independientes (predios)")

pintar_corrmatrix_XY(regresion.arboles,x=indep.predios, y=dependientes.arboles, method_cor = "spearman")+
  labs(title="Coeficiente Spearman entre dependiente e independientes (predios)")


indep.predios.copa.sel<-c("cuarto.porcentaje","area_ep")
indep.predios.copa.ap.sel<-c("apartamento.porcentaje","cuarto.porcentaje","area_ep.porcentaje")
indep.predios.sel<-c("apartamento.porcentaje","cuarto.porcentaje","area_ep.porcentaje","area_ep")



### modelos lineales 


dependiente <- "log.area_copa"
independientes  <- c(indep.poblacion.copa.sel,indep.predios.copa.sel)
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
f <- paste(str_c(dependiente,".mxn"), "~", paste(str_c(independientes,".mxn"), collapse=" + "))
lm.mod.area_copa<-lm(f, data = regresion.arboles.mn)
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

best_models<-ols_best_subset(lm(f,data = lm.mod.area_copa$model))
best_models
plot(best_models)


pintar_mapa_su_lm2(regresion.arboles,lm.mod.area_copa,nrow =1)

# coberbtura AP
dependiente <- "cobertura_copa.ap"
independientes  <- c(indep.poblacion.copa.ap.sel,indep.predios.copa.ap.sel)
# max normalizado 
var_names<-c(dependiente,independientes)
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
f <- paste(str_c(dependiente,".mxn"), "~", paste(str_c(independientes,".mxn"), collapse=" + "))
lm.mod.cobertura.ap<-lm(f, data = regresion.arboles.mn)
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

best_models<-ols_best_subset(lm(f, data = lm.mod.cobertura.ap$model))
best_models
plot(best_models)

#mejores modelos lineales 
dependiente <- "log.area_copa"
independientes  <- c("superior_postgrado",
                     "densidad_poblacion",
                     "cuarto.porcentaje",
                     "area_ep")
# max normalizado 
var_names<-c(dependiente,names(regresion.arboles[,independientes]))
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
f <- paste(str_c(dependiente,".mxn"), "~", paste(str_c(independientes,".mxn"), collapse=" + "))
lm.best.area_copa<-lm(f, data = regresion.arboles.mn)
summary(lm.best.area_copa)


dependiente <- "cobertura_copa.ap"
independientes  <- c("superior_postgrado.porcentaje")
# max normalizado 
var_names<-c(dependiente,independientes)
regresion.arboles.mn<-max_nomalization(regresion.arboles,var_names)
f <- paste(str_c(dependiente,".mxn"), "~", paste(str_c(independientes,".mxn"), collapse=" + "))
lm.best.cobertura.ap<-lm(f, data = regresion.arboles.mn)
summary(lm.best.cobertura.ap)




# resultados geoestadisticos 




