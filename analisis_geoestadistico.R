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
dev.off()
su.arboles<-su[su$SETU_CCDGO %in% regresion.arboles$SETU_CCDGO ,]
lnb.su<-poly2nb(su.arboles)
W_cont<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su.arboles, border="grey80")
plot(lnb.su, coordinates(su.arboles), col='orchid1',pch=19, cex=0.1, add=TRUE)
dev.off()
par(mar=rep(0,4))
plot(W_cont,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
dev.off()

lnb.su<-poly2nb(su.arboles, queen = T)
W_queen<-nb2listw(lnb.su, style="W", zero.policy=T)
par(mai=c(0,0,0,0))
plot(su.arboles, border="grey80")
plot(W_queen, coordinates(su.arboles), col='orchid1',pch=19, cex=0.1, add=TRUE)
dev.off()
par(mar=rep(0,4))
plot(W_queen,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
dev.off()

W_cont_mat<-listw2mat(W_cont)
mat.melted <- melt(W_cont_mat)
ggplot(mat.melted, aes(y = Var1, x = Var2, fill = value)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  #scale_fill_viridis(direction = -1, option = "magma")+
  ylab('filas') +
  xlab('columnas') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

su.arboles$residuals<-lm.mxn.sqrt.area_copa.sel$residuals
resnb <- sapply(lnb.su, function(i) mean(su.arboles$residuals[i])) #media de los residuos de los vecinos
cor(su.arboles$residuals, resnb)
data.frame(su.arboles@data,resnb,coordinates(su.arboles)) %>%
ggplot()+
  geom_point( aes(x=residuals,y=resnb))

# probar autocorrelacion de la varible predicha
# Global Autocorrelation Tests: Moran's I

moran.test(regresion.arboles$area_copa, listw=W_cont)
# existe autocorrelacion espacial en la varoble a area_copa
geary.test(regresion.arboles$area_copa, listw=W_cont)
# Moran plot
dev.off()
moran.plot(regresion.arboles$area_copa, 
           listw=W_cont, 
           # xlim=c(0,100),ylim=c(0,100), 
           pch=16, col="black",
           cex=.5, quiet=F, 
           labels=as.character(regresion.arboles$SETU_CCDGO),
           xlab="area copa", 
           ylab="area copa (Spatial Lag)", main="Moran Scatterplot")


#local Moran I
localmoranmatrix<-localmoran(regresion.arboles$area_copa, listw=W_cont)
summary(localmoranmatrix)
lm_data<-augment(lm.area_copa.sel)
lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
lm_data$lmZ.abs <- abs(localmoranmatrix[,"Z.Ii"]) ## Extract z-scores abs
lm_data$lmZ <- localmoranmatrix[,"Z.Ii"] ## Extract z-scores
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ,n = 7))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ.abs)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")
  #scale_fill_brewer(palette = "RdBu", drop = FALSE)

#examinemos la SAC (Spatial Autocorrealation) de las variables del mejor LM
lm_data<-augment(lm.mxn.sqrt.area_copa.sel)
lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
moran.test(lm_data[,1], listw=W_cont)
geary.test(lm_data[,1], listw=W_cont)
moran.plot(lm_data$sqrt.area_copa.mxn, 
           listw=W_cont, 
           # xlim=c(0,100),ylim=c(0,100), 
           pch=16, col="black",
           cex=.5, quiet=F, 
           labels=as.character(lm_data$SETU_CCDGO),
           xlab="sqrt.area_copa.mxn", 
           ylab="sqrt.area_copa.mxn (Spatial Lag)", main="Moran Scatterplot")

localmoranmatrix<-localmoran(lm_data$sqrt.area_copa.mxn, listw=W_cont)
lm_data$lmZ.abs <- abs(localmoranmatrix[,"Z.Ii"]) ## Extract z-scores abs
lm_data$lmZ <- localmoranmatrix[,"Z.Ii"] ## Extract z-scores
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ,n = 7))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ.abs)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")
#scale_fill_brewer(palette = "RdBu", drop = FALSE)

#otras matrices de pesos W ---- 
coords<-coordinates(su.arboles)
centroides.su.arboles<-gCentroid(su.arboles, byid = T)
W_dist_mat<-gDistance(centroides.su.arboles, byid=T)
W_dist.inv_mat<-1000/W_dist_mat
diag(W_dist.inv_mat) <- 0

mat.melted <- melt(W_dist.inv_mat)
ggplot(mat.melted, aes(x = Var1, y = Var2, fill = value)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette = "RdPu", direction = -1) +
  ylab('columnas') +
  xlab('filas') +
  # coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

mat.melted <- melt(W_dist_mat)
ggplot(mat.melted, aes(x = Var1, y = Var2, fill = value)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  ylab('columnas') +
  xlab('filas') +
  # coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# transforma matrices a forma listw
W_dist.inv<-mat2listw(W_dist.inv_mat, style = "W")

par(mar=rep(0,4))
plot(W_dist.inv,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
dev.off()

# matriz distancias inversas en el rango de 1000 metros desde el centriode 
W_dist1000<-dnearneigh(coords,0,1001,longlat = FALSE)
W_dist1000_mat<-nb2mat(W_dist1000, style="W", zero.policy=T)
mat.melted <- melt(W_dist1000_mat)
ggplot(mat.melted, aes(x = Var1, y = Var2, fill = value)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  ylab('columnas') +
  xlab('filas') +
  # coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

par(mar=rep(0,4))
plot(W_dist1000,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
dev.off()


W_dist1000.inv_mat<-1/W_dist1000_mat
W_dist1000.inv_mat[!is.finite(W_dist1000.inv_mat)]<- 0
mat.melted <- melt(W_dist1000.inv_mat)
ggplot(mat.melted, aes(x = Var1, y = Var2, fill = value)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  ylab('columnas') +
  xlab('filas') +
  # coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



W_dist1000.inv<-mat2listw(W_dist1000.inv_mat, style = "W")
par(mar=rep(0,4))
plot(W_dist1000.inv,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
dev.off()

# usando otras matrices
#examinemos la SAC (Spatial Autocorrealation) de las variables del mejor LM
# W_dist1000.inv
lm_data<-augment(lm.mxn.sqrt.area_copa.sel)
lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
moran.test(lm_data[,1], listw=W_dist1000.inv,zero.policy = T,alternative = "two.sided")

geary.test(lm_data[,1], listw=W_dist1000.inv,zero.policy = T,alternative = "two.sided")

moran.plot(lm_data$sqrt.area_copa.mxn, 
           listw=W_dist1000.inv, 
           # xlim=c(0,100),ylim=c(0,100), 
           pch=16, col="black",
           cex=.5, quiet=F, 
           labels=as.character(lm_data$SETU_CCDGO),
           xlab="sqrt.area_copa.mxn", 
           ylab="sqrt.area_copa.mxn (Spatial Lag)", main="Moran Scatterplot",zero.policy = T)

localmoranmatrix<-localmoran(lm_data$sqrt.area_copa.mxn, listw=W_dist1000.inv,zero.policy = T)
lm_data$lmZ.abs <- abs(localmoranmatrix[,"Z.Ii"]) ## Extract z-scores abs
lm_data$lmZ <- localmoranmatrix[,"Z.Ii"] ## Extract z-scores
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ,n = 7))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ.abs)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")
#scale_fill_brewer(palette = "RdBu", drop = FALSE)

# usando otras matrices
#examinemos la SAC (Spatial Autocorrealation) de las variables del mejor LM
# W_dist.inv
lm_data<-augment(lm.mxn.sqrt.area_copa.sel)
lm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
moran.test(lm_data[,1], listw=W_dist.inv,zero.policy = T)
geary.test(lm_data[,1], listw=W_dist.inv,zero.policy = T)
moran.plot(lm_data$sqrt.area_copa.mxn, 
           listw=W_dist.inv, 
           zero.policy = T,
           # xlim=c(0,100),ylim=c(0,100), 
           pch=16, col="black",
           cex=.5, quiet=F, 
           labels=as.character(lm_data$SETU_CCDGO),
           xlab="sqrt.area_copa.mxn", 
           ylab="sqrt.area_copa.mxn (Spatial Lag)", main="Moran Scatterplot")

localmoranmatrix<-localmoran(lm_data$sqrt.area_copa.mxn, listw=W_dist1000.inv,zero.policy = T)
lm_data$lmZ.abs <- abs(localmoranmatrix[,"Z.Ii"]) ## Extract z-scores abs
lm_data$lmZ <- localmoranmatrix[,"Z.Ii"] ## Extract z-scores
su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = cut_number(lmZ,n = 7))) +
  coord_equal()+
  theme_void()+
  scale_fill_brewer(palette = "RdBu")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")

su.f %>% dplyr::select(-area_su)  %>%
  left_join(lm_data,by = c("id"="SETU_CCDGO")) %>%
  ggplot()+
  geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
  geom_polygon(aes(x= long, y = lat, group = group, fill = lmZ.abs)) +
  coord_equal()+
  theme_void()+
  scale_fill_viridis( option = "magma")
#scale_fill_brewer(palette = "RdBu", drop = FALSE)

# test de auto correlacion espacial en los residuos del modelo OLS
moran.lm<-lm.morantest(lm.mxn.sqrt.area_copa.sel, W_cont, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.mxn.sqrt.area_copa.sel, W_dist1000.inv, alternative="two.sided",zero.policy = T)
print(moran.lm)
moran.lm<-lm.morantest(lm.mxn.sqrt.area_copa.sel, W_dist.inv, alternative="two.sided",zero.policy = T)
print(moran.lm)

# Cual modelo usar ?
LM<-lm.LMtests(lm.best.area_copa, W_cont, test="all")
print(LM)


# modelos con terminos autorregresivos o de retardo 
#SAR

laglm.mxn.sqrt.area_copa.sel<-lagsarlm(formula = as.formula(lm.mxn.sqrt.area_copa.sel),
         data = lm.mxn.sqrt.area_copa.sel$model,
         listw = W_cont, zero.policy = T,tol.solve=1.0e-30)
summary(laglm.mxn.sqrt.area_copa.sel,Nagelkerke=T)

laglm_data<-augment(lm.mxn.sqrt.area_copa.sel)
laglm_data$SETU_CCDGO<-regresion.arboles$SETU_CCDGO
laglm_data$lagresid<-laglm.mxn.sqrt.area_copa.sel$residuals
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


