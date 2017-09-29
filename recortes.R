
moran.test(regresion.arboles$area_copa, listw=W_queen)
# existe autocorrelacion espacial en la varoble a area_copa
geary.test(regresion.arboles$area_copa, listw=W_cont)
# Moran plot
# dev.off()
moran.plot(regresion.arboles$area_copa, 
           listw=W_queen, 
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
lm_data<-augment(lm.mod.area_copa)
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



par(mar=rep(0,4))
plot(W_dist1000,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")
# dev.off()



par(mar=rep(0,4))
plot(W_dist1000.inv,coords=coordinates(su.arboles),pch=19, cex=0.1, col="gray")

# dev.off()

# usando otras matrices
#examinemos la SAC (Spatial Autocorrealation) de las variables del mejor LM
# W_dist1000.inv
lm_data<-augment(lm.mod.area_copa)
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
lm_data<-augment(lm.mod.area_copa)
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



```{r tab:comp-lmcopa}

stargazer(lm.mn.area_copa.sel, 
          lm.mxn.log.area_copa.sel, 
          lm.mxn.sqrt.area_copa.sel, 
          title="Modelos área copa preliminares", 
          align=TRUE,
          type = "latex") 

```