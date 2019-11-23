##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Analisis geoestadistico y SAR SLX SE
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
## dependencias: analsis_estadistico.R
##################################################
#library(spdep)

# nb.su<-poly2nb(su, row.names = su$SETU_CCDGO)
# par(mai=c(0,0,0,0))
# #plot(su)
# plot(nb.su, coordinates(su), col='grey50', pch=19, cex=0.1)
# dev.off()
su.EV<-su[su$SETU_CCDGO %in% regresion.EV$SETU_CCDGO ,]
lnb.su<-poly2nb(su.EV)
W_cont.ev<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(lnb.su, coordinates(su.EV), col='orchid1',pch=19, cex=0.1, add=TRUE)
# dev.off()
par(mar=rep(0,4))
plot(W_cont.ev,coords=coordinates(su.EV),pch=19, cex=0.1, col="gray")
# dev.off()

lnb.su<-poly2nb(su.EV, queen = T)
W_queen.ev<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(W_queen.ev, coordinates(su.EV), col='orchid1',pch=19, cex=0.1, add=TRUE)
# dev.off()
par(mar=rep(0,4))
plot(W_queen.ev,coords=coordinates(su.EV),pch=19, cex=0.1, col="gray")
# dev.off()




#otras matrices de pesos W ---- 
coords<-coordinates(su.EV)
# matriz distancias inversas en el rango de 1000 metros desde el centriode 
W_dist1000.ev<-dnearneigh(coords,0,1001,longlat = FALSE)
W_dist1000_mat.ev<-nb2mat(W_dist1000.ev, style="W", zero.policy=T)
W_dist1000.inv_mat.ev<-1000/W_dist1000_mat.ev
W_dist1000.inv_mat.ev[!is.finite(W_dist1000.inv_mat.ev)]<- 0
W_dist1000.inv.ev<-mat2listw(W_dist1000.inv_mat.ev,style = "W")

# W_dist1000_mat.ev<-nb2mat(W_dist1000.ev, style="W", zero.policy=T)
# W_dist1000.inv_mat.ev <- W_dist1000_mat.ev
# for(i in 1:(dim(W_dist1000_mat.ev))[1]) {W_dist1000.inv_mat.ev[i,i] = 0} # renders exactly zero all diagonal elements
# W_dist1000.inv_mat.ev[W_dist1000.inv_mat.ev > 1000] <- 0                   # all distances > 1000 miles are set to zero
# W_dist1000.inv_mat.ev <- ifelse(W_dist1000.inv_mat.ev!=0, 1/W_dist1000.inv_mat.ev, W_dist1000.inv_mat.ev)    # inverting distances
# W_dist1000.inv.ev <- mat2listw(W_dist1000.inv_mat.ev, style="W")    # create a (normalized) listw object
# #mydmi <- listw2mat(mydm.lw)              # change back to 'classic' matrix, if desired

# matriz distancias inversas en el rango de 1000 metros desde el centriode 
#W_dist1000.ev<-dnearneigh(coords,0,1001,longlat = FALSE)
# W_dist1000_mat.ev<-nb2mat(W_dist1000.ev, style="W",zero.policy = T)
# W_dist1000.inv_mat.ev<-1000/W_dist1000_mat.ev
# lnb.su<-poly2nb(su.arboles, queen = T)
par(mai=c(0,0,0,0))
plot(su, border="grey80")
plot(W_dist1000.inv.ev, coordinates(su.EV), col='orchid1',pch=19, cex=0.1, add=TRUE)

  par(mar=rep(5,4))
  moran.plot(lm.area_ep.ptje.sel$residuals, 
             listw=W_queen.ev, 
             pch=16, col="black",
             cex=.5, quiet=F, 
             labels=as.character(regresion.EV$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran")  

  moran.test(regresion.EV$area_ep.porcentaje, listw=W_queen.ev)
  moran.test(regresion.EV$ia.areas.dist, listw=W_queen.ev)
  moran.test(lm.area_ep.ptje.sel$residuals, listw=W_queen.ev)
  moran.test(lm.ia.areas.dist.sel$residuals, listw=W_queen.ev)
  
  
  par(mar=rep(5,4))
  moran.plot(lm.area_ep.ptje.sel$residuals, 
             listw=W_dist1000.inv.ev,
             zero.policy = TRUE,
             pch=16, col="black",
             cex=.5, quiet=F, 
             labels=as.character(regresion.EV$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran")  
  
  moran.test(regresion.EV$area_ep.porcentaje, listw=W_dist1000.inv.ev,zero.policy = TRUE)
  moran.test(regresion.EV$ia.areas.dist, listw=W_dist1000.inv.ev,zero.policy = TRUE)
  moran.test(lm.ia.areas.dist.sel$residuals,listw=W_dist1000.inv.ev,zero.policy = TRUE)
  moran.test(lm.area_ep.ptje.sel$residuals,listw=W_dist1000.inv.ev,zero.policy = TRUE)
  # 
  
  par(mar=rep(5,4))
  moran.plot(lm.ia.areas.dist.sel$residuals, 
             listw=W_dist1000.inv.ev, 
             zero.policy = TRUE,
             pch=16, col="black",
             cex=.5, quiet=F, 
             labels=as.character(regresion.EV$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran")  
  
  moran.plot(lm.ia.areas.dist.sel$residuals, 
             listw=W_queen.ev, 
             pch=16, col="black",
             cex=.5, quiet=F, 
             labels=as.character(regresion.EV$SETU_CCDGO),
             xlab="residuos", 
             ylab="residuos (Spatial Lag)", main="Gráfico de Moran") 
  
 
  pintar_mapa_su_LISA_lmres(regresion.EV,lm.area_ep.ptje.sel,W_queen.ev,  wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_lmres(regresion.EV,lm.area_ep.ptje.sel,W_dist1000.inv.ev, wname = "Wd",nrow =1)
  
  pintar_mapa_su_LISA_lmres(regresion.EV,lm.ia.areas.dist.sel,W_queen.ev,  wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_lmres(regresion.EV,lm.ia.areas.dist.sel,W_dist1000.inv.ev, wname = "Wd",nrow =1)
  
  
  pintar_mapa_su_LISA_var(regresion.EV,"area_ep.porcentaje",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"ia.areas.dist",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"ia.areas.dist",W_dist1000.inv.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"area_ep.porcentaje",W_dist1000.inv.ev, wname = "Wq",nrow =1)
  
  pintar_mapa_su_LISA_var(regresion.EV,"area_media_manzana",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"unidad_economica.porcentaje",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"cuarto.porcentaje",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"ningun_estudio.porcentaje",W_queen.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"densidad_poblacion",W_queen.ev, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"con_alguna_limitacion.porcentaje",W_queen.ev, wname = "Wq",nrow =1)
  
  
  pintar_mapa_su_LISA_var(regresion.EV,"area_media_manzana",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"unidad_economica.porcentaje",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"cuarto.porcentaje",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"ningun_estudio.porcentaje",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"densidad_poblacion",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  pintar_mapa_su_LISA_var(regresion.EV,"con_alguna_limitacion.porcentaje",W_dist1000.inv.ev, wname = "Wd",nrow =1)
  
  # diagPlts<-diagPlot(lm.area_ep.ptje.sel)
  # grid.arrange(grobs=diagPlts, ncol =2)
  
  lm_data<-augment(lm.area_ep.ptje.sel)
  lm_data$SETU_CCDGO<-regresion.EV$SETU_CCDGO
  
  # pl_lm.ac<-plots_map_su_df(lm_data,c("area_ep.porcentaje.mxn",str_c(independientes.EV[c(1,2)],".mxn"),".fitted"))
  # grid.arrange(grobs =pl_lm.ac, nrow =1)
  # 

# test de auto correlacion espacial en los residuos del modelo OLS
moran.lm<-lm.morantest(lm.area_ep.ptje.sel, W_queen.ev, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.area_ep.ptje.sel, W_dist1000.inv.ev, alternative="two.sided",zero.policy = T)
print(moran.lm)
# elegimos para lm.area_ep.ptje.sel, W_queen.ev,

moran.lm<-lm.morantest(lm.ia.areas.dist.sel, W_queen.ev, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.ia.areas.dist.sel, W_dist1000.inv.ev, alternative="two.sided",zero.policy = T)
print(moran.lm)
# elegimos para lm.ia.areas.dist.sel, W_queen.ev,

# modelos con terminos autorregresivos o de retardo 
#SAR

sar.areas.dist<-lagsarlm(formula = as.formula(lm.ia.areas.dist.sel),
         data = lm.ia.areas.dist.sel$model,
         listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30)
sar.sm<-summary(sar.areas.dist,Nagelkerke=T)
moran.test(sar.areas.dist$residuals,W_queen.ev,zero.policy = T)
bptest.sarlm(sar.areas.dist)

sar.areas_ep<-lagsarlm(formula = as.formula(lm.area_ep.ptje.sel),
                                    data = lm.area_ep.ptje.sel$model,
                                    listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30)
sar2.sm<-summary(sar.areas_ep,Nagelkerke=T)
moran.test(sar.areas_ep$residuals,W_queen.ev,zero.policy = T)
bptest.sarlm(sar.areas_ep)


# SEM
sem.areas.dist<-errorsarlm(formula = as.formula(lm.ia.areas.dist.sel),
                                      data = lm.ia.areas.dist.sel$model,
                                      listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30)
summary(sem.areas.dist,Nagelkerke=T)
bptest.sarlm(sem.areas.dist)
moran.test(sem.areas.dist$residuals,W_queen.ev,zero.policy = T)

sem.areas_ep<-errorsarlm(formula = as.formula(lm.area_ep.ptje.sel),
                           data = lm.area_ep.ptje.sel$model,
                           listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30)
summary(sem.areas_ep,Nagelkerke=T)
bptest.sarlm(sem.areas_ep)
moran.test(sem.areas_ep$residuals,W_queen.ev,zero.policy = T)





#SLX
slx.areas.dist<-lmSLX(formula = as.formula(lm.ia.areas.dist.sel),
                           data = lm.ia.areas.dist.sel$model,
                           listw = W_queen.ev, zero.policy = T)

summary(slx.areas.dist)
moran.test(slx.areas.dist$residuals,W_queen.ev)
lmtest::bptest(slx.areas.dist)
AIC(slx.areas.dist)
AIC(lm.ia.areas.dist.sel)

slx.areas_ep<-lmSLX(formula = as.formula(lm.area_ep.ptje.sel),
                      data = lm.area_ep.ptje.sel$model,
                      listw = W_queen.ev, zero.policy = T)

summary(slx.areas_ep)
moran.test(slx.areas_ep$residuals,W_queen.ev)
lmtest::bptest(slx.areas_ep)
AIC(slx.areas_ep)
AIC(lm.area_ep.ptje.sel)


#SD

sd.areas.dist<-lagsarlm(formula = as.formula(lm.ia.areas.dist.sel),
                                    data = lm.ia.areas.dist.sel$model,
                                    listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30,
                                    type = "mixed")
summary(sd.areas.dist,Nagelkerke=T)
moran.test(sd.areas.dist$residuals,W_queen.ev,zero.policy = T)
bptest.sarlm(sd.areas.dist)
hist(sd.areas.dist$residuals)

sd.areas_ep<-lagsarlm(formula = as.formula(lm.area_ep.ptje.sel),
                        data = lm.area_ep.ptje.sel$model,
                        listw = W_queen.ev, zero.policy = T,tol.solve=1.0e-30,
                        type = "mixed")
summary(sd.areas_ep,Nagelkerke=T)
moran.test(sd.areas_ep$residuals,W_queen.ev,zero.policy = T)
bptest.sarlm(sd.areas_ep)
hist(sd.areas_ep$residuals)

#reducimos el modelo

# 
# diagPltsSlX<-diagPlot(slx.areas_ep)
# diagPlts[1:3]
# grid.arrange(grobs=diagPlts, ncol =2)

# diagPltsSAR<-diagPlotlaglm(sar.mod.sqrt.area_copa.wq)
# diagPltsSEM<-diagPlotlaglm(sem.mod.sqrt.area_copa.wq)
# diagPltsSD<-diagPlotlaglm(sd.mod.sqrt.area_copa.wq)
# diagPltsSLX<-diagPlotlaglm(slx.mod.sqrt.area_copa.wq)
# diagPltsOLS<-diagPlotlaglm(lm.mod.area_copa)


# diagPlotsAll<-arrangeGrob(grobs = c(diagPltsOLS,diagPltsSLX,diagPltsSD,diagPltsSEM,diagPltsSAR),ncol = 3, nrow = 5)
# grid.arrange (arrangeGrob(grobs = diagPltsOLS, ncol = 3,left ="OLS"),
#               arrangeGrob(grobs = diagPltsSLX, ncol = 3,left = "SLX"),
#               arrangeGrob(grobs = diagPltsSAR, ncol = 3,left =  "SAR"),
#               arrangeGrob(grobs = diagPltsSEM, ncol = 3,left = "SEM"),
#               arrangeGrob(grobs = diagPltsSD,ncol = 3, left = "SD"),nrow =5)




# moran.plot(sar.mod.sqrt.area_copa.wq$residuals, 
#            listw=W_queen, 
#            zero.policy = TRUE,
#            pch=16, col="black",
#            cex=.5, quiet=T, 
#            labels=as.character(regresion.arboles$SETU_CCDGO),
#            xlab="residuos sar", 
#            ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 
# 
# moran.plot(sem.mod.sqrt.area_copa.wq$residuals, 
#            listw=W_queen, 
#            zero.policy = TRUE,
#            pch=16, col="black",
#            cex=.5, quiet=T, 
#            labels=as.character(regresion.arboles$SETU_CCDGO),
#            xlab="residuos sar", 
#            ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 
# 
# moran.plot(slx.mod.sqrt.area_copa.wq$residuals, 
#            listw=W_queen, 
#            zero.policy = TRUE,
#            pch=16, col="black",
#            cex=.5, quiet=T, 
#            labels=as.character(regresion.arboles$SETU_CCDGO),
#            xlab="residuos sar", 
#            ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 
# moran.plot(sd.mod.sqrt.area_copa.wq$residuals, 
#            listw=W_queen, 
#            zero.policy = TRUE,
#            pch=16, col="black",
#            cex=.5, quiet=T, 
#            labels=as.character(regresion.arboles$SETU_CCDGO),
#            xlab="residuos sar", 
#            ylab="residuos sar (Spatial Lag)", main="Gráfico de Moran") 
# 

# resmodelos<-regresion.arboles %>% select(SETU_CCDGO) %>% 
#   mutate(SETU_CCDGO = as.character(SETU_CCDGO))%>%
#   mutate(ols.resid=lm.mod.area_copa$residuals) %>%
#   mutate(slx.resid =slx.mod.sqrt.area_copa.wq$residuals)%>%
#   mutate(sar.resid =sar.mod.sqrt.area_copa.wq$residuals)%>%
#   mutate(sem.resid =sem.mod.sqrt.area_copa.wq$residuals)%>%
#   mutate(sd.resid =sd.mod.sqrt.area_copa.wq$residual )




# respls<-plots_map_gradient0_df(resmodelos,names(resmodelos)[-1])
# grid.arrange(grobs=respls[c(1,5)],top = "Mapa de residuos de los modelos", nrow =1)
# 
# 
# resmodelos.long<-resmodelos %>% 
#   gather(key = modelo, value = valores, -SETU_CCDGO) 
# cuantiles<-stats::quantile(resmodelos.long$valores,probs =seq(0,1,0.2)) %>% round(.,digits = 3)
# resmodelos.long %<>%
#   mutate(residuos.qn=cut(valores,breaks = cuantiles)) %>% na.omit()
# 
# 
# su.f %>% dplyr::select(-area_su)  %>%
#   left_join(resmodelos.long,by = c("id"="SETU_CCDGO")) %>%
#  filter(!is.na(modelo)) %>%
#   ggplot()+
#  geom_polygon(data = su.f, aes(x= long, y = lat, group = group), fill = "grey60")+
#   geom_polygon(aes(x= long, y = lat, group = group, fill = residuos.qn))+
#   coord_equal()+
#   scale_fill_brewer(palette = "RdBu",
#                     guide = guide_legend(direction = "horizontal",
#                                          label.position = "bottom",
#                                          title.position = 'top',
#                                          nrow = 1))+
#   theme_void()+
#   facet_wrap(~modelo, nrow = 1)+
#   tema_lgnd_abajo()
# 
