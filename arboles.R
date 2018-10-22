##################################################
## Proyecto: Inequidades en el acceso a 
## Objetivo Script: Procesar los datos del censo arboreo 2015
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
##################################################



# cargar datos Censo Arboreo 2015 ----
CAutf8 <- read_delim("./CA2015/CAutf8.csv",
";", escape_double = FALSE, trim_ws = TRUE)


# indagar las categorias ----
summary(CAutf8)
summary(CAutf8$id)
CAutf8$id %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(CAutf8$familia) 
CAutf8$familia %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()

summary(CAutf8$vitalidad) 
CAutf8$vitalidad %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(CAutf8$emplazamiento)
CAutf8$emplazamiento %>% 
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(CAutf8$edad) 
CAutf8$edad %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()
summary(CAutf8$cobertura) 
CAutf8$cobertura %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()

summary(CAutf8$vegetacion) 
CAutf8$vegetacion %>%
  as.character() %>% 
  trimws() %>%
  unique() %>% 
  as.factor()


summary(CAutf8$familia %>% 
          as.factor()) %>% barplot()
summary(CAutf8$vitalidad %>% 
          as.factor()) %>% barplot()
summary(CAutf8$emplazamiento %>% 
          as.factor()) %>% barplot()
summary(CAutf8$edad%>% 
          as.factor()) %>% barplot()
summary(CAutf8$cobertura%>% 
          as.factor()) %>% barplot()
summary(CAutf8$vegetacion%>% 
          as.factor()) %>% barplot()


# Estandarizacion de nombres y conversion a factores ----

#familias
CAutf8$familia<-gsub("Piperáceae" ,"Piperaceae",CAutf8$familia)
CAutf8$familia %>% trimws()%>% as.factor() ->CAutf8$familia

summary(CAutf8$familia) %>% barplot()
summary(CAutf8$familia)%>%sort(decreasing = T) %>% barplot()
summary(CAutf8$familia)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#nombre cientifico
CAutf8$nombre_cienticico<-gsub("Annona cherimolla" ,"Annona cherimola" ,CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-as.factor(CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-gsub("Artocarpus integrifolius" ,"Artocarpus integrifolia" ,CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-as.factor(CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-gsub("Attalea butyraceae" ,"Attalea butyracea" ,CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-as.factor(CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-as.factor(gsub("Calliandra hematosephala" ,"Calliandra haematocephala" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Calliandra tweedii" ,"Calliandra tweediei" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Citrus lemon" ,"Citrus limon" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Erythroxylon coca" ,"Erythroxylum coca" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Eucalyptus globulus" ,"Eucalyptus globulus" ,CAutf8$nombre_cienticico))#caso raro, parece identicos 
CAutf8$nombre_cienticico<-as.factor(gsub("Guaicum officinale" ,"Guaiacum officinale" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Guaiacum officinarum" ,"Guaiacum officinale" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-gsub("Miconia chlorocarpa" ,"Miconia chlorocarpa" ,CAutf8$nombre_cienticico)#caso raro, parece identicos
CAutf8$nombre_cienticico<-as.factor(CAutf8$nombre_cienticico)
CAutf8$nombre_cienticico<-as.factor(gsub("Miconia Spicellata" ,"Miconia spicellata" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Pereskia bledo" ,"Pereskia bleo" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Phoenix robellina" ,"Phoenix roebelenii" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Platysmicium pinnatum","Platymiscium pinnatum" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico<-as.factor(gsub("Terminalia amazonica","Terminalia amazonia" ,CAutf8$nombre_cienticico))
CAutf8$nombre_cienticico %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$nombre_cienticico
summary(CAutf8$nombre_cienticico)%>%sort(decreasing = T) %>% barplot()
summary(CAutf8$nombre_cienticico)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#barrios
CAutf8$Nombrebarrio<-as.factor(gsub("Alfonso Bonilla Aragon", "Alfonso Bonilla Aragón" ,CAutf8$Nombrebarrio))
CAutf8$Nombrebarrio<-as.factor(gsub("Rep·blica de Israel", "República de Israel",CAutf8$Nombrebarrio))
CAutf8$Nombrebarrio<-as.factor(gsub("Sector Alto Jordan", "Sector Alto Jordán",CAutf8$Nombrebarrio))
CAutf8$Nombrebarrio<-as.factor(gsub("Sector Altos de Normandia - Bataclan", "Sector Altos de Normandia - Bataclán",CAutf8$Nombrebarrio))
summary(CAutf8$Nombrebarrio) %>% barplot()
CAutf8$Nombrebarrio %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$Nombrebarrio
summary(CAutf8$Nombrebarrio) %>% barplot()
summary(CAutf8$Nombrebarrio)%>%sort(decreasing = T) %>% barplot()
summary(CAutf8$Nombrebarrio)%>%sort(decreasing = T) %>%  .[1:10] %>%barplot()


#emplazamiento
CAutf8$emplazamiento<-as.factor(gsub("Rondas de rios", "Ronda de rios",CAutf8$emplazamiento))
CAutf8$emplazamiento<-as.factor(gsub("Escenario depor/cult", "Escenario deportivo y/o Cultural",CAutf8$emplazamiento))
CAutf8$emplazamiento<-as.factor(gsub("Bahia de estacionami", "Bahias de estacionamiento",CAutf8$emplazamiento))
CAutf8$emplazamiento %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$emplazamiento
summary(CAutf8$emplazamiento)%>%sort(decreasing = T) %>% barplot()
summary(CAutf8$emplazamiento)%>%sort(decreasing = T) %>%  .[1:4] %>%barplot()

#vitalidad
CAutf8$vitalidad %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$vitalidad
CAutf8$vitalidad<-factor(CAutf8$vitalidad,levels(CAutf8$vitalidad)[c(2,3,4,1)])
summary(CAutf8$vitalidad) %>% barplot()

#edad
CAutf8$edad %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$edad
CAutf8$edad <- factor(CAutf8$edad,levels(CAutf8$edad)[c(1,3,2)])
summary(CAutf8$edad) %>% barplot()

#cobertura
CAutf8$cobertura %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$cobertura
summary(CAutf8$cobertura) %>% barplot()

#vegetacion
CAutf8$vegetacion %>% as.character() %>% trimws()%>% as.factor() ->CAutf8$vegetacion
summary(CAutf8$vegetacion) %>% 
  barplot()

#id
#buscar duliplicados ----
CAutf8 %>%
  group_by(id) %>% 
  filter(n()>1) #%>% View()
#eliminar duliplicados
CAutf8<-CAutf8 %>%
  distinct(id,.keep_all=TRUE)

total_arboles_censo <- CAutf8 %>% nrow()


# guardar copia de las modficaciones al original.
write.csv(x = CAutf8,"./outputData/ca2015.csv",fileEncoding = "UTF-8")
rm(CAutf8)

# columnas a incluir en analsis ----
AU_analsis<-CAutf8 %>% dplyr::select(id,nombre_cienticico,familia,vegetacion,
                                     altura_arbol,diametro_copa,edad,vitalidad,emplazamiento,cobertura,
                                     Norte,Este,Norte0,Este0) %>% na.omit()



## calculamos la cobertura de copa ----
AU_analsis<-AU_analsis %>% rowwise()%>%
  mutate(area_copa= pi*(diametro_copa/2)^2)

summary(AU_analsis)

# inspecionar CA graficamente sin agregaciones ----
AU_analsis %>% ggplot()+
  #  geom_boxplot(aes(x=edad,y=area_copa))+ coord_flip() +
  geom_boxplot(aes(x=edad,y=area_copa,color=cobertura))+ coord_flip() 

AU_analsis %>% ggplot()+
  geom_violin(aes(x=emplazamiento,y=diametro_copa/2,color=emplazamiento))

AU_analsis %>% ggplot()+
  geom_violin(aes(x=edad,y=diametro_copa/2,color=edad))

# puede ser bueno calcular porcentaje de arboles longevos por sector urbano
# mirar altura versus copa por varibles categoricas
# AU_analsis %>% filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
#   ggplot()+
#   geom_point(aes(x=altura_arbol,y=diametro_copa/2,color=vegetacion),alpha=0.1)+
#   scale_color_brewer(palette = "Dark2")+
#   coord_fixed()+
#   coord_flip() +
#   facet_grid(cobertura~emplazamiento  )


AU_analsis %>% filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
  geom_point(aes(y=altura_arbol,x=diametro_copa/2,color=vegetacion),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  facet_wrap(~emplazamiento  )


# agregacion estadistica de variables CA ----
altura_copa_por_vegetacion<-AU_analsis %>% 
  group_by(vegetacion) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_cobertura<-AU_analsis %>% 
  group_by(cobertura) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_emplazamiento<-AU_analsis %>% 
  group_by(emplazamiento) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

altura_copa_por_edad<-AU_analsis %>% 
  group_by(edad) %>% 
  summarise(altura_media_vegetacion=mean(altura_arbol),
            diametro_medio_copa_vegetacion = mean(diametro_copa),cantidad=n()) 

# Graficar agragaciones CA

p_por_vegetacion<-ggplot(altura_copa_por_vegetacion,
                         aes(x=diametro_medio_copa_vegetacion,
                             y=altura_media_vegetacion)) 
p_por_cobertura<-ggplot(altura_copa_por_cobertura,
                        aes(x=diametro_medio_copa_vegetacion,
                            y=altura_media_vegetacion)) 

p_por_emplazamiento<-ggplot(altura_copa_por_emplazamiento,
                            aes(x=diametro_medio_copa_vegetacion,
                                y=altura_media_vegetacion)) 

p_por_edad<-ggplot(altura_copa_por_edad,
                   aes(x=diametro_medio_copa_vegetacion,
                       y=altura_media_vegetacion)) 


#cantidad de arboles por varible nominal
p_por_vegetacion+
  geom_point(aes(size=cantidad,color=vegetacion ) )+ 
  scale_color_brewer(palette = "Dark2")+
  scale_size_area(max_size = 10)

p_por_cobertura+
  geom_point(aes(size=cantidad,color=cobertura ) )+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size_area(max_size = 10)


p_por_emplazamiento+
  geom_point(aes(size=cantidad,color=emplazamiento ) )+ 
  #  scale_color_brewer(palette = "Paired")+
  scale_size_area(max_size = 10)



p_por_edad+
  geom_point(aes(size=cantidad,color=edad ) )+ 
  scale_color_brewer(palette = "Dark2")+
  scale_size_area(max_size = 10)


# exploracion variables continuas CA ----
p_copa<-ggplot(AU_analsis, aes(x = diametro_copa)) 
p_altura<-ggplot(AU_analsis, aes(x = altura_arbol)) 


p_copa + geom_histogram(aes(fill = vegetacion), 
                        color = "white", 
                        alpha = 0.4,
                        binwidth = 1,
                        position="identity")+  
  geom_vline(data = altura_copa_por_vegetacion,
             aes(xintercept = diametro_medio_copa_vegetacion,
                 color=vegetacion),
             linetype="dashed") + scale_fill_brewer(palette = "Dark2")+scale_color_brewer(palette = "Dark2")

p_altura + geom_histogram(aes(fill = vegetacion), 
                          color = "white", 
                          alpha = 0.4,
                          binwidth = 1,
                          position="identity")+  
  geom_vline(data = altura_copa_por_vegetacion, 
             aes(xintercept = altura_media_vegetacion,
                 color=vegetacion),
             linetype="dashed") + scale_fill_brewer(palette = "Dark2")+scale_color_brewer(palette = "Dark2")


# criterios de seleccion individuos CA ----
#independiente de su vitaidad actual, podemos pensar que hace 10 años 
#no estaban ni enfermos ni secos, ni muertos, asi que los usaremos todos.

#Se excluyen las plantas arbustivas y los arbustos, pues interesa individuos de mayor talla
#que 2 metros que provean sombra y sean un beneficio en andes, donde se ubica la mayor cantidad
#de individuos
selvegetacion<-c("Arbol", "Palma" ,"Bambu","Muerto","Seco")
#para reducir la brecha de tiempo entre los datos del CP2005 y los datos del 2015 del CA 
#solo se tomaran en cuanta arboles Maduros y longevos.
seledad<-c("Longevo","Maduro")

#filtrar por criterios
AU_analsis<-filter(AU_analsis, vegetacion %in% selvegetacion) %>%
  filter(edad %in% seledad) %>%
  filter(altura_arbol > 2) %>%
  filter(diametro_copa >1.5)

nrow(AU_analsis)


# Inspeccion individuos CA seleccionados ----

AU_analsis %>% 
  ggplot()+
  geom_point(aes(y=altura_arbol,x=diametro_copa/2,color=vegetacion),alpha=0.1)+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  facet_wrap( ~ emplazamiento,ncol = 5 )

AU_analsis %>% 
  #filter(vegetacion %in% c("Arbol","Bambu","Palma")) %>%
  ggplot()+
  geom_point(aes(x=altura_arbol,y=diametro_copa/2),alpha=0.1, color =brewer.pal(4,"Dark2")[1] )+
  scale_color_brewer(palette = "Dark2")+
  coord_equal()+
  coord_flip() +
  theme_void()+
  facet_grid( ~ vegetacion )

write.csv(AU_analsis,file = "./outputData/arboles_analisis.csv")
save(AU_analsis,
     seledad,
     selvegetacion,
     altura_copa_por_cobertura,
     altura_copa_por_edad,
     altura_copa_por_emplazamiento,
     altura_copa_por_vegetacion,
     file = "arboles.RData")


