##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Analisis geoestadistico y SAR SLX SE
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: analsis_estadistico.R
##################################################
library(spdep)

nb.su<-poly2nb(su, row.names = su$SETU_CCDGO)
par(mai=c(0,0,0,0))
#plot(su)
plot(nb.su, coordinates(su), col='grey50', pch=19, cex=0.1)
# dev.off()
su.arboles<-su[su$SETU_CCDGO %in% regresion.arboles$SETU_CCDGO ,]
lnb.su<-poly2nb(su.arboles)
W_cont<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(lnb.su, coordinates(su.arboles), col='orchid1',pch=19, cex=0.1, add=TRUE)
# dev.off()
par(mar=rep(0,4))
plot(W_cont,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
# dev.off()

lnb.su<-poly2nb(su.arboles, queen = T)
W_queen<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(W_queen, coordinates(su.arboles), col='orchid1',pch=19, cex=0.1, add=TRUE)
# dev.off()
par(mar=rep(0,4))
plot(W_queen,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
# dev.off()


#otras matrices de pesos W ---- 
coords<-coordinates(su.arboles)
# centroides.su.arboles<-gCentroid(su.arboles, byid = T)
# W_dist_mat<-gDistance(centroides.su.arboles, byid=T)
# W_dist.inv_mat<-1000/W_dist_mat # para mantener los numeros no tan cercanos a 0
# diag(W_dist.inv_mat) <- 0


# matriz distancias inversas en el rango de 1000 metros desde el centriode 
W_dist1000<-dnearneigh(coords,0,1001,longlat = FALSE)
W_dist1000_mat<-nb2mat(W_dist1000, style="W", zero.policy=T)
W_dist1000.inv_mat<-1000/W_dist1000_mat
W_dist1000.inv_mat[!is.finite(W_dist1000.inv_mat)]<- 0
W_dist1000.inv<-mat2listw(W_dist1000.inv_mat,style = "W")
# lnb.su<-poly2nb(su.arboles, queen = T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(W_dist1000.inv, coordinates(su.arboles), col='orchid1',pch=19, cex=0.1, add=TRUE)

  par(mar=rep(5,4))
  moran.plot(lm.mod.area_copa$residuals, 
             listw=W_queen, 
             pch=16, col="black",
             cex=.5, quiet=F, 
             labels=as.character(regresion.arboles$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran")  

  moran.test(regresion.arboles$area_copa, listw=W_queen)
  moran.test(regresion.arboles$sqrt.area_copa, listw=W_queen)
  moran.test(lm.mod.area_copa$residuals, listw=W_queen)
  
  moran.test(regresion.arboles$area_copa, listw=W_dist1000.inv,zero.policy = TRUE)
  moran.test(regresion.arboles$sqrt.area_copa, listw=W_dist1000.inv,zero.policy = TRUE)
  moran.test(lm.mod.area_copa$residuals, listw=W_dist1000.inv,zero.policy = TRUE)
  
  moran.plot(lm.mod.area_copa$residuals, 
             listw=W_dist1000.inv, 
             zero.policy = TRUE,
             pch=16, col="black",
             cex=.5, quiet=T, 
             labels=as.character(regresion.arboles$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran") 
  
  
  pintar_mapa_su_LISA_lmres(regresion.arboles,lm.mod.area_copa,W_queen,  wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_lmres(regresion.arboles,lm.mod.area_copa,W_dist1000.inv, wname = "Wd",nrow =1)
  
  pintar_mapa_su_LISA_var(regresion.arboles,"log.area_copa",W_queen, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"sqrt.area_copa",W_queen, wname = "Wq",nrow =1)
  
  pintar_mapa_su_LISA_var(regresion.arboles,"sqrt.cobertura_copa.ap",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"afro.porcentaje",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"superior_postgrado",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"superior_postgrado.porcentaje",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"densidad_poblacion",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"cuarto.porcentaje",W_queen, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"area_ep.porcentaje",W_queen, wname = "Wd",nrow =1)
  
  
  diagPlts<-diagPlot(lm.mod.area_copa)
  grid.arrange(grobs=diagPlts, ncol =2)
  
  lm_data<-augment(lm.mod.area_copa)
  lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
  
  pl_lm.ac<-plots_map_su_df(lm_data,c("log.area_copa.mxn","superior_postgrado.mxn",".fitted"))
  grid.arrange(grobs =pl_lm.ac, nrow =1)
  

# test de auto correlacion espacial en los residuos del modelo OLS
moran.lm<-lm.morantest(lm.mod.area_copa, W_queen, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.mod.area_copa, W_dist1000.inv, alternative="two.sided",zero.policy = T)
print(moran.lm)

# Cual modelo usar ?
LM.area_copa.wq<-lm.LMtests(lm.mod.area_copa, W_queen, test=c("LMerr","RLMerr","LMlag","RLMlag"))
print(LM.area_copa.wq)
LM.area_copa.wd<-lm.LMtests(lm.mod.area_copa, W_dist1000.inv, test=c("LMerr","RLMerr","LMlag","RLMlag"),
                            zero.policy = T)
print(LM.area_copa.wd)

LM.copa.ap.wd<-lm.LMtests(lm.best.cobertura.ap, W_queen,  test=c("LMerr","RLMerr","LMlag","RLMlag"))
print(LM.copa.ap.wd)


# modelos con terminos autorregresivos o de retardo 
#SAR

sar.mod.sqrt.area_copa.wq<-lagsarlm(formula = as.formula(lm.mod.area_copa),
         data = lm.mod.area_copa$model,
         listw = W_queen, zero.policy = T,tol.solve=1.0e-30)
sar.sm<-summary(sar.mod.sqrt.area_copa.wq,Nagelkerke=T)
moran.test(sar.mod.sqrt.area_copa.wq$residuals,W_queen,zero.policy = T)
bptest.sarlm(sar.mod.sqrt.area_copa.wq)


# SEM
sem.mod.sqrt.area_copa.wq<-errorsarlm(formula = as.formula(lm.mod.area_copa),
                                      data = lm.mod.area_copa$model,
                                      listw = W_queen, zero.policy = T,tol.solve=1.0e-30)
summary(sem.mod.sqrt.area_copa.wq,Nagelkerke=T)
bptest.sarlm(sem.mod.sqrt.area_copa.wq)



#SLX
slx.mod.sqrt.area_copa.wq<-lmSLX(formula = as.formula(lm.mod.area_copa),
                           data = lm.mod.area_copa$model,
                           listw = W_queen, zero.policy = T)

summary(slx.mod.sqrt.area_copa.wq)
moran.test(slx.mod.sqrt.area_copa.wq$residuals,W_queen)
lmtest::bptest(slx.mod.sqrt.area_copa.wq)
AIC(slx.mod.sqrt.area_copa.wq)



#SD

sd.mod.sqrt.area_copa.wq<-lagsarlm(formula = as.formula(lm.mod.area_copa),
                                    data = lm.mod.area_copa$model,
                                    listw = W_queen, zero.policy = T,tol.solve=1.0e-30,
                                    type = "mixed")
summary(sd.mod.sqrt.area_copa.wq,Nagelkerke=T)
moran.test(sd.mod.sqrt.area_copa.wq$residuals,W_queen,zero.policy = T)
bptest.sarlm(sd.mod.sqrt.area_copa.wq)
hist(sd.mod.sqrt.area_copa.wq$residuals)

#reducimos el modelo

#nos quedamos con wd


diagPltsSlX<-diagPlot(slx.mod.sqrt.area_copa.wq)
diagPlts[1:3]
grid.arrange(grobs=diagPlts, ncol =2)

diagPltsSAR<-diagPlotlaglm(sar.mod.sqrt.area_copa.wq)
diagPltsSEM<-diagPlotlaglm(sem.mod.sqrt.area_copa.wq)
diagPltsSD<-diagPlotlaglm(sd.mod.sqrt.area_copa.wq)
diagPltsSLX<-diagPlotlaglm(slx.mod.sqrt.area_copa.wq)
diagPltsOLS<-diagPlotlaglm(lm.mod.area_copa)


diagPlotsAll<-arrangeGrob(grobs = c(diagPltsOLS,diagPltsSLX,diagPltsSD,diagPltsSEM,diagPltsSAR),ncol = 3, nrow = 5)
grid.arrange (arrangeGrob(grobs = diagPltsOLS, ncol = 3,left ="OLS"),
              arrangeGrob(grobs = diagPltsSLX, ncol = 3,left = "SLX"),
              arrangeGrob(grobs = diagPltsSAR, ncol = 3,left =  "SAR"),
              arrangeGrob(grobs = diagPltsSEM, ncol = 3,left = "SEM"),
              arrangeGrob(grobs = diagPltsSD,ncol = 3, left = "SD"),nrow =5)




moran.plot(sar.mod.sqrt.area_copa.wq$residuals, 
           listw=W_queen, 
           zero.policy = TRUE,
           pch=16, col="black",
           cex=.5, quiet=T, 
           labels=as.character(regresion.arboles$SETU_CCDGO),
           xlab="residuos sar", 
           ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 

moran.plot(sem.mod.sqrt.area_copa.wq$residuals, 
           listw=W_queen, 
           zero.policy = TRUE,
           pch=16, col="black",
           cex=.5, quiet=T, 
           labels=as.character(regresion.arboles$SETU_CCDGO),
           xlab="residuos sar", 
           ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 

moran.plot(slx.mod.sqrt.area_copa.wq$residuals, 
           listw=W_queen, 
           zero.policy = TRUE,
           pch=16, col="black",
           cex=.5, quiet=T, 
           labels=as.character(regresion.arboles$SETU_CCDGO),
           xlab="residuos sar", 
           ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 
moran.plot(sd.mod.sqrt.area_copa.wq$residuals, 
           listw=W_queen, 
           zero.policy = TRUE,
           pch=16, col="black",
           cex=.5, quiet=T, 
           labels=as.character(regresion.arboles$SETU_CCDGO),
           xlab="residuos sar", 
           ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 


resmodelos<-regresion.arboles %>% select(SETU_CCDGO) %>% 
  mutate(SETU_CCDGO = as.character(SETU_CCDGO))%>%
  mutate(ols.resid=lm.mod.area_copa$residuals) %>%
  mutate(slx.resid =slx.mod.sqrt.area_copa.wq$residuals)%>%
  mutate(sar.resid =sar.mod.sqrt.area_copa.wq$residuals)%>%
  mutate(sem.resid =sem.mod.sqrt.area_copa.wq$residuals)%>%
  mutate(sd.resid =sd.mod.sqrt.area_copa.wq$residual )




respls<-plots_map_gradient0_df(resmodelos,names(resmodelos)[-1])
grid.arrange(grobs=respls[c(1,5)],top = "Mapa de residuos de los modelos", nrow =1)


resmodelos.long<-resmodelos %>% 
  gather(key = modelo, value = valores, -SETU_CCDGO) 
cuantiles<-stats::quantile(resmodelos.long$valores,probs =seq(0,1,0.2)) %>% round(.,digits = 3)
resmodelos.long %<>%
  mutate(residuos.qn=cut(valores,breaks = cuantiles)) %>% na.omit()


su.f %>% dplyr::select(-area_su)  %>%
  left_join(resmodelos.long,by = c("id"="SETU_CCDGO")) %>%
 filter(!is.na(modelo)) %>%
  ggplot()+
 geom_polygon(data = su.f, aes(x= long, y = lat, group = group), fill = "grey60")+
  geom_polygon(aes(x= long, y = lat, group = group, fill = residuos.qn))+
  coord_equal()+
  scale_fill_brewer(palette = "RdBu",
                    guide = guide_legend(direction = "horizontal",
                                         label.position = "bottom",
                                         title.position = 'top',
                                         nrow = 1))+
  theme_void()+
  facet_wrap(~modelo, nrow = 1)+
  tema_lgnd_abajo()

