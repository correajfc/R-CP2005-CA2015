# funciones 

# escalar varibles en [0,1] para los mapas ----
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}




tema_lgnd_up<-function (...){
  theme_bw() +
    theme(
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          # legend.key.size = unit(2,"mm"),
          legend.position = "top",
          ... 
          )
  
}

tema_lgnd_abajo<-function (...){
  theme_void() +
    theme(
      legend.text=element_text(size=6),
      legend.position = "bottom",
      legend.key.height = unit(2,"mm"),
      legend.direction = "horizontal",
      ... 
    )
  
}

tema_lgnd_derecha<-function (...){
  theme_void() +
    theme(
      legend.text=element_text(size=6),
      legend.position = "right",
      legend.key.height = unit(2,"mm"),
      legend.direction = "horizontal",
      ... 
    )
  
}


expandBbox<-function(bb,perc.x,perc.y){
  dx <- (bb[1,2]-bb[1,1])*perc.x 
  dy<-(bb[2,2]-bb[2,1])*perc.x
  bb[1,]<-c(bb[1,1]-dx, bb[1,2]+dx)
  bb[2,]<-c(bb[2,1]-dy, bb[2,2]+dy)
  bb  
}


# grafica de dispersion bivariada con tendecia del modelo 
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue",alpha=0.2) +
    geom_smooth(method = method, color = "red", ...)
  p
}


# grafica de dispercion con label de correlacion dentror de la grafica
lm_with_cor <- function(data, mapping, ... ,method_cor = "pearson", method_smooth="lm") {
  x <- eval(mapping$x, data)
  y <- eval(mapping$y, data)
  cor <- cor(x, y, method = method_cor, use = "pairwise.complete.obs")
  
  
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue",alpha=0.2) +
    geom_smooth(method = method_smooth, color = "red", ...)+
    geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 2)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 4, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    )
  p
}


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

pintar_corrmatrix<- function(data, variables, ... ,method_cor = "pearson" ){
  
  require(reshape2)
  # Correlacion Cobertura Arborea y estructura SU ----
  cormat <- round(cor(data[,variables],
                                       use = "pairwise.complete.obs",
                                       method = method_cor ),2)
  #head(cormat.ca.strct.pearson)
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  utri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_utri <- melt(utri, na.rm = TRUE)
  #melted_cormat <- melt(cormat)
  head(melted_utri)
  
  
  p<-ggplot(data = melted_utri, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "white")+
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
    #scale_fill_viridis(option = "magma")+
    scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=method_cor) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 8, hjust = 1),axis.title = element_blank())+
    coord_fixed()
  p
}

pintar_corrmatrix_XY<- function(data, x , y , ... ,method_cor = "pearson" ){
  
  require(reshape2)
  # Correlacion Cobertura Arborea y estructura SU ----
  cormat <- round(cor(data[,x],data[,y],
                      use = "pairwise.complete.obs",
                      method = method_cor ),2)
  #head(cormat.ca.strct.pearson)
  # Reorder the correlation matrix
  #cormat <- reorder_cormat(cormat)
  #utri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(cormat, na.rm = TRUE)
  #melted_cormat <- melt(cormat)
  #head(melted_utri)
  
  
  p<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "white")+
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 2.2) +
    #scale_fill_viridis(option = "magma")+
    scale_fill_gradient2(low = "firebrick2", high = "steelblue3", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=method_cor) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 8, hjust = 1),axis.title = element_blank())+
    coord_fixed()
  p
}

pintar_mapa_su_lm<-function(data,lm, ...){
  require(broom)
  #tdy<-tidy(lm)
  aum<-augment(lm)
  #gln<-glance(lm)
  aum$SETU_CCDGO<-data$SETU_CCDGO
  var_dep<-names(lm$model)[1]
  lm.fit_maps<-plots_map_su_df(aum,c(var_dep,".resid",".fitted"))
  grid.arrange(grobs =lm.fit_maps, ... = ...)
  
  
}

plots_map_su_df<-function(df,col_names){ 
  
  l_col_names<-as.list(col_names)
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(df, by = c("id"="SETU_CCDGO")) 
  lapply(l_col_names,function (i){
    ggplot(su_df)+
      geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = i))+
      coord_equal()+
      scale_fill_viridis(
        direction = 1, 
        na.value = "grey50"
      )+
      theme_void()+
      tema_lgnd_abajo()})
  
}


plots_map_su_ntl<-function(df, col_names,num_tiles, ... ){ 
 
  
  df %>% select(one_of(col_names)) %>%
    mutate_all(ntile,num_tiles)->df.ntl
  
  df.ntl<-df %>% 
    select(SETU_CCDGO) %>% 
    bind_cols(df.ntl)
  
  # datos en formato long para graficacion de small multiples
  
  df.ntl.long<-gather_(df.ntl, 
                       key_col = "vars",
                       value_col = "value",
                       gather_cols=col_names)
  
  
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(df.ntl.long, by = c("id"="SETU_CCDGO")) %>%
    filter(vars %in% col_names) %>%
    ggplot()+
    geom_polygon(data = su.f,
                 aes(x = long ,y = lat, group = group),
                 fill = "grey70")+
    geom_polygon(aes(x= long, y = lat, group = group, fill = factor(value)))+
    coord_equal()+
    scale_fill_viridis( name = paste0("q",num_tiles),
                        direction = 1, 
                        discrete = T, 
                        na.value = "grey50",
                        guide = guide_legend(direction = "horizontal",
                                             label.position = "bottom",
                                             title.position = 'top',
                                             nrow = 1))+
    facet_wrap(~vars, nrow = 1)+
    tema_lgnd_abajo()
  
  
  su_df
}

pintar_mapa_su_lm_ntl<-function(data,lm, ..., num_tiles = 5){
  require(broom)
  lm.tdy<-tidy(lm)
  lm.aum<-augment(lm)
  lm.gln<-glance(lm)
  lm.aum$SETU_CCDGO<-data$SETU_CCDGO
  var_dep<-names(lm$model)[1]
  lm.fit_maps<-plots_map_su_ntl(lm.aum,c(var_dep,".resid",".fitted"), num_tiles = num_tiles)
  lm.fit_maps
  
}

max_nomalization<-function(df,var_names){
# Max nomalization model
var_names.mn<-paste0(var_names,".mxn")
df.mn<-df%>%
  select(one_of(var_names))%>%
  mutate_all(funs(./max(.))) %>% # dividir por el maximo
  setNames(nm = var_names.mn)
df.mn$SETU_CCDGO<-df$SETU_CCDGO
df.mn  
  
  
}

crear_lm_from_df<-function(df){
  
  var_names<-names(df)
  dependiente<-var_names[1]
  independientes<-var_names[-c(1,length(var_names))]
  form<-as.formula(paste(dependiente, paste(independientes, collapse=" + "), sep=" ~ "))
  lm_df<-lm(formula = form, data = df )
  lm_df


}
