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
      legend.text=element_text(size=5),
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


tema_lgnd_abajo_s<-function (...){
  theme_void() +
    theme(
      legend.title = element_text(size=7),
      legend.text=element_text(size=5),
      legend.position = "bottom",
      legend.key.height = unit(2,"mm"),
      legend.key.width = unit(5,"mm"),
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
  
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  correlation <- cor(x, y, method = method_cor, use = "pairwise.complete.obs")
  
  
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue",alpha=0.2) +
    geom_smooth(method = method_smooth, color = "red", ...)+
    geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(correlation, digits = 2)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 3, fontface = "bold",
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
                                     size = 6, hjust = 1),
          axis.text.y = element_text(size = 6),
          axis.title = element_blank(),
          plot.title = element_text(size=14))+
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
                                     size = 8, hjust = 1),axis.title = element_blank(),
          legend.text=element_text(size=6),
          legend.position = "bottom",
          legend.key.height = unit(2,"mm"),
          legend.direction = "horizontal",
          plot.title = element_text(size=12))+
   
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

plots_map_su_df<-function(df,col_names,...){ 
  
  l_col_names<-as.list(col_names)
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(df, by = c("id"="SETU_CCDGO")) 
  lapply(l_col_names,function (i){
    ggplot(su_df)+
      geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = i))+
      coord_equal()+
      scale_fill_viridis(
        direction = 1, 
        na.value = "grey50",
        guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(2, units = "mm"),
          barwidth = unit(40, units = "mm"),
          draw.ulim = F,
          title.position = 'top',
          # some shifting around
          title.hjust = 0.5,
          label.hjust = 0.5
        )
      )+
      theme_void()+
      tema_lgnd_abajo(...)})
  
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

localmoran_quad<-function(val,lagval,pval,sgnf){
  quad_sig<-NA
  
  if( is.finite(pval)){
  if(val >= 0 & 
     (lagval >= 0) & 
     (pval <= sgnf))
    quad_sig<-"high-high"
  if(val <= 0 & 
     (lagval <= 0) & 
     (pval <= sgnf))
    quad_sig<-"low-low"
  if(val >= 0 & 
     (lagval <= 0) & 
     (pval <= sgnf))
    quad_sig<-"high-low"
  if(val <= 0 & 
     (lagval >= 0) & 
     (pval <= sgnf))
    quad_sig<-"low-high"
  if(pval > sgnf)
    quad_sig<-"not signif."  
  }
  quad_sig  
}


plots_map_LISA_df<-function(df,col_names){ 

  l_col_names<-as.list(col_names)
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(df, by = c("id"="SETU_CCDGO"))
  lapply(l_col_names,function (i){
    ggplot(su_df)+
      geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = i))+
      coord_equal()+
      scale_fill_gradient2(
        guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(2, units = "mm"),
          barwidth = unit(40, units = "mm"),
          draw.ulim = F,
          title.position = 'top',
          # some shifting around
          title.hjust = 0.5,
          label.hjust = 0.5
        )
      )+
      theme_void()+
      tema_lgnd_abajo()})

}

plots_map_gradient0_df<-function(df,col_names){ 
  
  l_col_names<-as.list(col_names)
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(df, by = c("id"="SETU_CCDGO"))
  lapply(l_col_names,function (i){
    ggplot(su_df)+
      geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = i))+
      coord_equal()+
      scale_fill_gradient2(
        guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(2, units = "mm"),
          barwidth = unit(40, units = "mm"),
          draw.ulim = F,
          title.position = 'top',
          # some shifting around
          title.hjust = 0.5,
          label.hjust = 0.5
        )
      )+
      theme_void()+
      tema_lgnd_abajo()})
  
}

pintar_mapa_su_lm2<-function(data,lm, ...){
  require(broom)
  #tdy<-tidy(lm)
  aum<-augment(lm)
  #gln<-glance(lm)
  aum$SETU_CCDGO<-data$SETU_CCDGO
  var_dep<-names(lm$model)[1]
  
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(aum, by = c("id"="SETU_CCDGO"))
  
  p_vardep<-ggplot(su_df)+
    geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = var_dep))+
    coord_equal()+
    scale_fill_viridis(
      direction = 1, 
      na.value = "grey50",
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(40, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    )+
    theme_void()+
    tema_lgnd_abajo()
  
  p_fit<-ggplot(su_df)+
    geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = ".fitted"))+
    coord_equal()+
    scale_fill_viridis(
      direction = 1, 
      na.value = "grey50",
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(40, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    )+
    theme_void()+
    tema_lgnd_abajo()
  
  p_res<-ggplot(su_df)+
    geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = ".resid"))+
    coord_equal()+
    scale_fill_gradient2(
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(40, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    )+
    theme_void()+
    tema_lgnd_abajo()
  
  title1<-grid::textGrob(format(as.formula(lm)) %>% str_c(collapse = "\n"))
  grid.arrange(p_vardep,p_fit,p_res, top = title1 ,... = ...)
  
  
}


pintar_mapa_su_LISA_lmres<-function(data,lm,W,wname="W" ,...){
  require(spdep)
  
  localmoranmatrix<-localmoran(lm$residuals, listw=W, zero.policy = TRUE)
  lmoran.df<-as_data_frame(localmoranmatrix)
  lmoran.df$SETU_CCDGO<-data$SETU_CCDGO
  #lmoran.df$Z.Ii <-lmoran.df$Z.Ii %>% as.vector()
  # escalar z valor
  lmoran.df$s_resid <- scale(lm$residuals)  %>% as.vector()
  
  # varible retardada
  lmoran.df$lag_s_resid <- lag.listw(W, lmoran.df$s_resid, zero.policy = TRUE)
  
  
  lmoran.df$p<-lmoran.df$`Pr(z > 0)`%>% as.vector()
  lmoran.df$Z.Ii<-lmoran.df$Z.Ii%>% as.vector()
  # summary of variables
  #summary(su.arboles@data)
  
  # crear etiqueta de observaciones foco de la autocorrelacion
  lmoran.df<-lmoran.df %>% rowwise() %>%
    mutate(quad_sig_05=localmoran_quad(s_resid,lag_s_resid, p,0.05)) 
  coloresLisa<-brewer.pal(5, "RdBu")
  quad_moran = c("high-high","high-low" ,"not signif.", "low-high","low-low")
  labels = c("H-H","H-L" ,"not signif.", "L-H","L-L")
  
  # volverlas un factor ordenado para que coincida con los colores
  lmoran.df$quad_sig_05<-factor(lmoran.df$quad_sig_05, levels =quad_moran)
  
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(lmoran.df, by = c("id"="SETU_CCDGO"))
  
  mapa_cluster<-su_df %>% 
    ggplot(aes(long, lat, group = group, fill = quad_sig_05)) + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(color = "grey90", size = .05)  + 
    coord_equal() + 
    theme_void() + 
    scale_fill_manual(values = coloresLisa, drop =FALSE,
                      labels = labels,
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
    tema_lgnd_abajo()
  
  # grafica discreta del p-value  
  signf_levels<-c(Inf,0.05,0.01,0.001,0.0001,0)
  signf_levels_label<-c("0001",".001",".01",".05","not signif.")
  
  mapa_p<-su_df %>% 
    ggplot() + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(aes(long, lat, group = group, fill = cut(p,breaks =signf_levels)),
                 color = "white", size = .05)  + 
    coord_equal() + 
    theme_void()+ 
    scale_fill_brewer(name ="P-val",palette = "Greens", drop=FALSE, direction = -1,
                      labels = signf_levels_label,
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
    tema_lgnd_abajo()
  
  #tdy<-tidy(lm)
  
  mapa_ZI<-ggplot(su_df)+
    geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = "Z.Ii"))+
    coord_equal()+
    scale_fill_gradient2(
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(40, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    )+
    theme_void()+
    tema_lgnd_abajo()
  
  
  title1<-grid::textGrob(paste0("Mapas LISA"," - ",wname,"\nResiduos: \n",format(as.formula(lm)) %>% str_c(collapse = "\n")))
  grid.arrange(mapa_ZI,mapa_p,mapa_cluster, top = title1 ,... = ...)
  
  
}


pintar_mapa_su_LISA_var<-function(data,varname,W, wname= "W", ...){
  require(spdep)
  
  localmoranmatrix<-localmoran(data[,varname], listw=W, zero.policy = TRUE)
  lmoran.df<-as_data_frame(localmoranmatrix)
  lmoran.df$SETU_CCDGO<-data$SETU_CCDGO
  #lmoran.df$Z.Ii <-lmoran.df$Z.Ii %>% as.vector()
  # escalar z valor
  lmoran.df$s_var <- scale(data[,varname])  %>% as.vector()
  
  # varible retardada
  lmoran.df$lag_s_var <- lag.listw(W, lmoran.df$s_var , zero.policy = TRUE)
  
  
  lmoran.df$p<-lmoran.df$`Pr(z > 0)`%>% as.vector()
  lmoran.df$Z.Ii<-lmoran.df$Z.Ii%>% as.vector()
  # summary of variables
  summary(su.arboles@data)
  
  # crear etiqueta de observaciones foco de la autocorrelacion
  lmoran.df<-lmoran.df %>% rowwise() %>%
    mutate(quad_sig_05=localmoran_quad(s_var,lag_s_var, p , 0.05)) 
  coloresLisa<-brewer.pal(5, "RdBu")
  quad_moran = c("high-high","high-low" ,"not signif.", "low-high","low-low")
  labels = c("h-h","h-l" ,"no signif.", "l-h","l-l")
  
  # volverlas un factor ordenado para que coincida con los colores
  lmoran.df$quad_sig_05<-factor(lmoran.df$quad_sig_05, levels =quad_moran)
  
  su_df<-su.f %>% dplyr::select(-area_su)  %>%
    left_join(lmoran.df, by = c("id"="SETU_CCDGO"))
  
  mapa_cluster<-su_df %>% 
    ggplot(aes(long, lat, group = group, fill = quad_sig_05)) + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(color = "grey90", size = .05)  + 
    coord_equal() + 
    theme_void() + 
    scale_fill_manual(name = "Cuadrantes",values = coloresLisa, drop =FALSE,na.value = "grey",
                      labels = labels,
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
    tema_lgnd_abajo_s()
  
  # grafica discreta del p-value  
  signf_levels<-c(Inf,0.05,0.01,0.001,0.0001,0)
  signf_levels_label<-c("0001",".001",".01",".05","not signif.")
  mapa_p<-su_df %>% 
    ggplot() + 
    geom_polygon(data =su.f,aes(x= long, y = lat, group = group), fill ="grey60") +
    geom_polygon(aes(long, lat, group = group, fill = cut(p,breaks =signf_levels)),
                 color = "grey90", size = .05)  + 
    coord_equal() + 
    theme_void()+ 
    scale_fill_brewer(name ="P-valor",palette = "Greens", drop=FALSE, direction = -1,
                      labels = signf_levels_label,
                      na.value = "grey",
                      guide = guide_legend(direction = "horizontal",
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow = 1))+
    tema_lgnd_abajo_s()
  
  #tdy<-tidy(lm)
  
  mapa_ZI<-ggplot(su_df)+
    geom_polygon(aes_string(x= "long", y = "lat", group = "group", fill = "Z.Ii"),
                 color = "grey90", size = .05)+
    coord_equal()+
    scale_fill_gradient2(name = "Moran Local Normalizado",na.value = "grey",
                         low = "firebrick1",mid ="white" ,high = "royalblue",
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(35, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        # some shifting around
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    )+
    theme_void()+
    tema_lgnd_abajo(legend.title = element_text(size=7))
  
  
  # title1<-grid::textGrob(paste0("Mapas LISA - ",varname," - ",wname))
  grid.arrange(mapa_ZI,mapa_p,mapa_cluster, #top = title1,
               ... = ...)
  
  
}


tema_lgnd_derecha2<-function (...){
  theme_void() +
    theme(
      legend.text=element_text(size=6),
      legend.position = "right",
      legend.key.height = unit(2,"mm"),
      legend.direction = "horizontal",
      ... 
    )
  
}



diagPlot<-function(lm){
  require(ggplot2)
  model<-augment(lm)
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
   
  p2<-  ggplot(model)+stat_qq(aes(sample=.resid))
  # p2<-p2+geom_abline(aes(qqline(.std.resid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
  
  p3<-ggplot(model, aes(.fitted, sqrt(abs(.std.resid))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()
  
  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()
  
  p5<-ggplot(model, aes(.hat, .std.resid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")
  
  p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()
  
  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}

myaugment<-function(laglm){
  
  df<-data.frame(.fitted=laglm$fitted.values,
                 .resid=laglm$residuals
                 )
  
  df
}


diagPlotlaglm<-function(laglm,theme.base_size = 8,size_alt = 6){
  require(ggplot2)
  model<-myaugment(laglm)
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point(alpha=0.6)
  p1<-p1+stat_smooth(method="lm")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Valores ajustados")+ylab("Residuos")
  p1<-p1+ggtitle("Residuos vs Valores ajustados")+
    theme_bw(base_size =theme.base_size )+
    theme(plot.title = element_text(size = size_alt))
  
  p2<-  ggplot(model,aes(sample=.resid))+stat_qq()+stat_qq_line(color = "red")
  # geom_abline(intercept = 0, slope = 1, alpha = 0.5) 
  p2<-p2+xlab("Cuantiles teóricos")+ylab("Residuos")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw(base_size =theme.base_size)
  
  p3<-ggplot(model)+geom_histogram(aes(.resid,y=..density..), color ="white")+
    stat_function(fun=dnorm,
                  color="red",
                  args=list(mean=mean(model$.resid), 
                            sd=sd(model$.resid)))
    
 # p3<-p3+geom_density(aes(.resid))
  p3<-p3+ggtitle("Histograma residuos")+
    xlab("Residuos")+ylab("Densidad")+theme_bw(base_size =theme.base_size)
  
  return(list(p1, p2, p3))
}

fitstats_laglm<-function(model,W){
gmt<-moran.test(model$residuals,W,zero.policy = T)

swt<-shapiro.test(model$residuals)
akaike<-AIC(model)

mediares<-mean(summary(model)$residuals)
mse<-mean(summary(model)$residuals^2) 
lglk<-logLik(model)

if(class(model)[1]=="lm"){

adjRsrt<-summary(model)$adj.r.squared
bpt<-lmtest::bptest(model)
fitstats<-c(gmt$estimate[1],gmt$p.value,
       swt$statistic,swt$p.value,
       bpt$statistic,bpt$p.value,
       mediares,
       mse ,
       adjRsrt,
       NA,
       akaike,
       lglk)
}else{
adjRsrt<-summary(model)$NK 
bpt<-bptest.sarlm(model)
fitstats<-c(gmt$estimate[1],gmt$p.value,
       swt$statistic,swt$p.value,
       bpt$statistic,bpt$p.value,
       mediares,
       mse ,
       NA,
       adjRsrt,
       akaike,
       lglk)
}
names(fitstats)<-c("Global Moran'I",
                               "GMI p-valor",
                               "Shapiro-Wilk",
                               "SW p-valor",
                               "Breusch-Pagan",
                               "BP p-valor",
                               "Media Residuos",
                               "MSE (Error cuadratico medio)",
                               "adj-Rsquare",
                               "Nagelkerke pseudo-R-squared",
                               "AIC",
                               "Log likelihood")
fitstats
}


fitstats_laglm_df<-function(model,W){
  gmt<-moran.test(model$residuals,W,zero.policy = T)
  swt<-shapiro.test(model$residuals)
  akaike<-AIC(model)
  mediares<-mean(summary(model)$residuals)
  mse<-mean(summary(model)$residuals^2) 
  lglk<-logLik(model)
  
  if(class(model)[1]=="lm" | class(model)[1]=="SLX"){
    
    adjRsrt<-summary(model)$adj.r.squared
    bpt<-lmtest::bptest(model)
    fitstats<-c(gmt$estimate[1],gmt$p.value,
                swt$statistic,swt$p.value,
                bpt$statistic,bpt$p.value,
                mediares,
                mse ,
                adjRsrt,
                NA,
                akaike,
                lglk)
  }else{
    adjRsrt<-summary(model,Nagelkerke=T)$NK 
    bpt<-bptest.sarlm(model)
    fitstats<-c(gmt$estimate[1],gmt$p.value,
                swt$statistic,swt$p.value,
                bpt$statistic,bpt$p.value,
                mediares,
                mse ,
                NA,
                adjRsrt,
                akaike,
                lglk)
  }
  
  medidasfit<-c("Globla Moran'I",
                     "GMI p-value",
                     "Shapiro-Wilk",
                     "SW p-value",
                     "Breusch-Pagan",
                     "BP p-value",
                     "Media Residuos",
                     "MSE",
                     "adj-Rsquare",
                     "Nagelkerke pseudo-R-squared",
                     "AIC",
                     "Log likelihood")
  
  df<-data_frame(medidasfit, fitstats = as.numeric(fitstats))
  df
}

fitstats_lm<-function(model){

  swt<-shapiro.test(model$residuals)
  akaike<-AIC(model)
  mediares<-mean(summary(model)$residuals)
  mse<-mean(summary(model)$residuals^2) 
  lglk<-logLik(model)
  adjRsrt<-summary(model)$adj.r.squared
  bpt<-lmtest::bptest(model)
  fitstats<-c(swt$statistic,swt$p.value,
                bpt$statistic,bpt$p.value,
                mediares,
                mse ,
                adjRsrt,
                akaike,
                lglk)
  
  names(fitstats)<-c("Shapiro-Wilk",
                     "SW p-value",
                     "Breusch-Pagan",
                     "BP p-value",
                     "Media Residuos",
                     "MSE",
                     "adj-Rsquare",
                     "AIC",
                     "Log likelihood")
  
  fitstats
}


fitstats_lm_df<-function(model){
  swt<-shapiro.test(model$residuals)
  akaike<-AIC(model)
  mediares<-mean(summary(model)$residuals)
  mse<-mean(summary(model)$residuals^2) 
  lglk<-logLik(model)
  adjRsrt<-summary(model)$adj.r.squared
  bpt<-lmtest::bptest(model)
  fitstats<-c(swt$statistic,swt$p.value,
              bpt$statistic,bpt$p.value,
              mediares,
              mse ,
              adjRsrt,
              akaike,
              lglk)
  
  medidasfit<-c("Shapiro-Wilk",
                     "SW p-value",
                     "Breusch-Pagan",
                     "BP p-value",
                     "Media Residuos",
                     "MSE",
                     "adj-Rsquare",
                     "AIC",
                     "Log likelihood")
  df<-data_frame(medidasfit,fitstats = as.numeric(fitstats))
  df
}


reemplzar_nombres_df <- function(df, df_nombres){
  
  nombres_var <- names(df)
  df_nombres_f <- df_nombres %>% 
    filter_at(1, all_vars(. %in% nombres_var))
  
  oldnames <- pull(df_nombres_f,1)
  newnames <- pull(df_nombres_f,2)
  
  df %>% rename_at(vars(oldnames), ~ newnames)
}


reemplzar_nombres_vector <- function(vec, df_nombres){
  
  df_nombres_f <- df_nombres %>% 
    filter_at(1, all_vars(. %in% vec))
  
  oldnames <- pull(df_nombres_f,1)
  newnames <- pull(df_nombres_f,2)
  
  # for (i in 1:length(oldnames)) {
  # 
  #   vec <- str_replace(vec,pattern = fixed(oldnames[i]),replacement = newnames[i])
  # 
  # }
  
  for (i in 1:length(vec)) {
    for (j in 1:length(oldnames)) {
      if(vec[i]==oldnames[j])
        vec[i] <- newnames[j]
    }
  }
  
  vec
}
