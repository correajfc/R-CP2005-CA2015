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
  
  
  pintar_mapa_su_LISA_lmres(regresion.arboles,lm.mod.area_copa,W_queen, nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"sqrt.area_copa",W_queen, wname = "Wq",nrow =1)
  pintar_mapa_su_LISA_var(regresion.arboles,"sqrt.area_copa",W_dist1000.inv, wname = "Wd",nrow =1)
  # calcular the local moran 
  localmoranmatrix<-localmoran(lm.mod.area_copa$residuals, listw=W_dist1000.inv,zero.policy = T)
  lmoran.df<-as_data_frame(localmoranmatrix)
  lmoran.df$Z.Ii <-lmoran.df$Z.Ii %>% as.vector()
  lmoran.df$`Pr(z > 0)` <-lmoran.df$`Pr(z > 0)` %>% as.vector()
  # escalar z valor
  su.arboles$s_resid <- scale(lm.mod.area_copa$residuals)  %>% as.vector()
  
  # varible retardada
  su.arboles$lag_s_resid <- lag.listw(W_dist1000.inv, su.arboles$s_resid, zero.policy = T) %>% as.vector()
  
  dftemp<-bind_cols(su.arboles@data,lmoran.df)
  dftemp$p<-dftemp$`Pr(z > 0)`%>% as.vector()
  dftemp$Z.Ii<-dftemp$Z.Ii%>% as.vector()
  # summary of variables
  summary(dftemp)

# crear etiqueta de observaciones foco de la autocorrelacion
  dftemp<-dftemp %>% rowwise() %>%
  mutate(quad_sig_05=localmoran_quad(s_resid,lag_s_resid, p,0.05)) 
  coloresLisa<-brewer.pal(5, "RdBu")
  labels = c("high-high","high-low" ,"not signif.", "low-high","low-low")
  
# volverlas un factor ordenado para que coincida con los colores
    dftemp$quad_sig_05<-factor(dftemp$quad_sig_05, levels =labels)
  
  su.f %>% dplyr::select(-area_su)  %>%
    left_join(dftemp, by = c("id"="SETU_CCDGO")) %>% 
    ggplot(aes(long, lat, group = group, fill = quad_sig_05)) + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(color = "grey90", size = .05)  + 
    coord_equal() + 
    theme_void() + scale_fill_manual(values = coloresLisa, drop =FALSE)
  
# grafica discreta del p-value  
  signf_levels<-c(Inf,0.05,0.01,0.001,0.0001,0)
  signf_levels_label<-c("0001",".001",".01",".05","not signif.")
  su.f %>% dplyr::select(-area_su)  %>%
    left_join(dftemp, by = c("id"="SETU_CCDGO")) %>% 
    ggplot() + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(aes(long, lat, group = group, fill = cut(p,breaks =signf_levels)),
                 color = "white", size = .05)  + 
    coord_equal() + 
    theme_void()+ 
    scale_fill_brewer(palette = "RdPu", drop=FALSE, direction = -1, labels =signf_levels_label)
  
  # graficas LISA
  pl_moran.ac<-plots_map_LISA_df(dftemp,c("s_resid","lag_s_resid","Z.Ii"))
  grid.arrange(grobs =pl_moran.ac, nrow =1)
  
  lm_data<-augment(lm.mod.area_copa)
  lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
  
  pl_lm.ac<-plots_map_su_df(lm_data,c("sqrt.area_copa.mxn","superior_postgrado.mxn",".fitted"))
  grid.arrange(grobs =pl_lm.ac, nrow =1)
  
  lm_data<-augment(lm.mod.area_copa)
  lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
  lm_data$lmZ.abs <- abs(localmoranmatrix[,"Z.Ii"]) ## Extract z-scores abs
  lm_data$lmZ <- localmoranmatrix[,"Z.Ii"] ## Extract z-scores
  su.f %>% dplyr::select(-area_su)  %>%
    left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
    ggplot()+
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ,n = 5))) +
    coord_equal()+
    theme_void()+
    scale_fill_brewer(palette = "RdBu")

  su.f %>% dplyr::select(-area_su)  %>%
    left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
    ggplot()+
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ.abs,n = 5))) +
    coord_equal()+
    theme_void()+
    scale_fill_brewer(palette = "PuRd")
  
     

# test de auto correlacion espacial en los residuos del modelo OLS
moran.lm<-lm.morantest(lm.mod.area_copa, W_queen, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.mod.area_copa, W_dist1000.inv, alternative="two.sided",zero.policy = T)
print(moran.lm)

# Cual modelo usar ?
LM.area_copa.wd<-lm.LMtests(lm.mod.area_copa, W_queen, test=c("LMerr","RLMerr","LMlag","RLMlag"))
print(LM.area_copa.wd)

LM.copa.ap.wd<-lm.LMtests(lm.mod.cobertura.ap2, W_queen,  test=c("LMerr","RLMerr","LMlag","RLMlag"))
print(LM.copa.ap.wd)


# modelos con terminos autorregresivos o de retardo 
#SAR

sar.mod.sqrt.area_copa.wq<-lagsarlm(formula = as.formula(lm.mod.area_copa),
         data = lm.mod.area_copa$model,
         listw = W_queen, zero.policy = T,tol.solve=1.0e-30)
summary(sar.mod.sqrt.area_copa.wq,Nagelkerke=T)
moran.test(sar.mod.sqrt.area_copa.wq$residuals,W_queen,zero.policy = T)
bptest.sarlm(sar.mod.sqrt.area_copa.wq)

sar.mod.sqrt.area_copa.wd<-lagsarlm(formula = as.formula(lm.mod.area_copa),
                                       data = lm.mod.area_copa$model,
                                       listw = W_dist1000.inv, zero.policy = T,tol.solve=1.0e-30)
summary(sar.mod.sqrt.area_copa.wd,Nagelkerke=T)
moran.test(sar.mod.sqrt.area_copa.wd$residuals,W_dist1000.inv,zero.policy = T)
bptest.sarlm(sar.mod.sqrt.area_copa.wd)

# SE
se.mod.sqrt.area_copa.wq<-errorsarlm(formula = as.formula(lm.mod.area_copa),
                                      data = lm.mod.area_copa$model,
                                      listw = W_queen, zero.policy = T,tol.solve=1.0e-30)
summary(se.mod.sqrt.area_copa.wq,Nagelkerke=T)
bptest.sarlm(se.mod.sqrt.area_copa.wq)

se.mod.sqrt.area_copa.wd<-errorsarlm(formula = as.formula(lm.mod.area_copa),
                                     data = lm.mod.area_copa$model,
                                     listw = W_dist1000.inv, zero.policy = T,tol.solve=1.0e-30)
summary(se.mod.sqrt.area_copa.wd,Nagelkerke=T)
bptest.sarlm(se.mod.sqrt.area_copa.wd)



#SLX
slx.mod.area_copa.wq<-lmSLX(formula = as.formula(lm.mod.area_copa),
                           data = lm.mod.area_copa$model,
                           listw = W_queen, zero.policy = T)

summary(slx.mod.area_copa.wq)
moran.test(slx.mod.area_copa.wq$residuals,W_queen)
lmtest::bptest(slx.mod.area_copa.wq)
AIC(slx.mod.area_copa.wq)


slx.mod.area_copa.wd<-lmSLX(formula = as.formula(lm.mod.area_copa),
      data = lm.mod.area_copa$model,
      listw = W_dist1000.inv, zero.policy = T)

summary(slxmod.area_copa.wd)
AIC(slxmod.area_copa.wq)


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



laglm_data<-augment(lm.mod.area_copa)
laglm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
laglm_data$lagresid<-lm.mod.area_copa$residuals
su.f %>% dplyr::select(-area_su)  %>%
  left_join(laglm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lagresid,n = 7))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(laglm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lagresid)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(laglm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = abs(lagresid))) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")


