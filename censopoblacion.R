##################################################
## Proyecto: Inequidades en el acceso a beneficios ambientales 
## Objetivo Script: Procesar los datos del censo de poblacion 2005
## Fecha: julio 2017
## Autor: Juan Fernando Correa Caicedo
##################################################
# necesita haber corrido geodata.R 


# Carga datos CP2005 ----

# datos personas 
CP2005_t_persona_edad <- read_csv("./CP2005/CP2005 - t_persona_edad.csv",
                                  col_types = cols(edad_promedio = col_number(),
                                                   personas = col_number(), su_id = col_character()))

CP2005_t_nivel_estudios <- read_csv("./CP2005/CP2005 - t_nivel_estudios.csv",
                                    col_types = cols(ningun_estudio = col_number(),
                                                     su_id = col_character(), superior_postgrado = col_number(),
                                                     total_personas = col_number()))

Cp2005_t_limitacion <- read_csv("./CP2005/Cp2005 - t_limitacion.csv",
                                col_types = cols(NO = col_number(), SI = col_number(),
                                                 su_id = col_character(), total_personas = col_number()))

CP2005_t_etnia <- read_csv("./CP2005/CP2005 - t_etnia.csv",
                           col_types = cols(indigena = col_number(),
                                            negro_mulato_afrocolombiano = col_number(),
                                            ninguno_de_los_anteriores = col_number(),
                                            no_informa = col_number(), 
                                            palenquero = col_number(),
                                            raizal_SAI_Providencia = col_number(),
                                            rom = col_number(), 
                                            su_id = col_character(),
                                            total_personas = col_number()))

#datos de vivienda por SU
CP2005_t_tipo_vivienda <- read_csv("./CP2005/CP2005 - t_tipo_vivienda.csv",
                                   col_types = cols(apartamento = col_number(),
                                                    casa = col_number(), 
                                                    casa_indigena = col_number(),
                                                    otro_tipo_de_vivienda = col_number(),
                                                    su_id = col_character(), tipo_cuarto = col_number(),
                                                    total_viviendas = col_number()))

CP2005_t_uso_predios <- read_csv("./CP2005/CP2005 - t_uso_predios.csv",
                                 col_types = cols(su_id = col_character(),
                                                  total_viviendas = col_number(), 
                                                  uso_LEA = col_number(),
                                                  uso_unidad_economica = col_number(),
                                                  uso_vivienda = col_number()))

CP2005_t_ocupacion_viviendas <- read_csv("./CP2005/CP2005 - t_ocupacion_viviendas.csv",
                                         col_types = cols(desocupada_por_uso_temporal = col_number(),
                                                          desocupadas = col_number(), 
                                                          ocupada_con_personas_ausentes = col_number(),
                                                          ocupada_con_personas_presentes = col_number(),
                                                          su_id = col_character(), total_viviendas = col_number()))


# consolidar datos CP2005 en un solo data frame ----
#datos_personas 

cp2005.personas<-CP2005_t_persona_edad %>%
  full_join(CP2005_t_nivel_estudios,by="su_id") %>% dplyr::rename(personas_edad=personas,
                                                                  personas_estudio=total_personas) %>%
  full_join(Cp2005_t_limitacion,by="su_id") %>% dplyr::rename(con_alguna_limitacion=SI,
                                                              sin_limitacion=NO,
                                                              personas_limitacion=total_personas) %>%
  full_join(CP2005_t_etnia,by="su_id") %>%
  dplyr::rename(personas_etnia=total_personas) %>%
  mutate(SETU_CCNCT = trimws(su_id))
#buscar duplicados
cp2005.personas%>%
  group_by(SETU_CCNCT) %>% 
  filter(n()>1) 
#datos viviendas
cp2005.viviendas<-CP2005_t_tipo_vivienda%>%
  full_join(CP2005_t_uso_predios,by="su_id") %>% dplyr::rename(viviendas_tipo=total_viviendas.x,
                                                               viviendas_uso=total_viviendas.y) %>%
  full_join(CP2005_t_ocupacion_viviendas, by = "su_id") %>% 
  dplyr::rename(viviendas_ocupacion=total_viviendas) %>% 
  mutate(SETU_CCNCT = trimws(su_id))

#buscar duplicados
cp2005.viviendas%>%
  group_by(SETU_CCNCT) %>% 
  filter(n()>1) 

#los datos CP2005 DANE con codigos de diferentes longitud ---- 
# remover dos digitos extras que parecen el codigo de comuna, pues coinciden.
#extraemos los SU de los datos del CP2005 del Redatam que coinciden 
#con el codigo de departamento y de ciudad. 76001 es el codigo de Cali
cp2005.personas <- cp2005.personas[grep("76001", cp2005.personas$SETU_CCNCT), ]
cp2005.viviendas <-cp2005.viviendas[grep("76001", cp2005.viviendas$SETU_CCNCT), ]

# Comparemos los codigos de Redatam con la cartografia
cp2005.personas$SETU_CCNCT %in% as.character(su$SETU_CCNCT)
cp2005.viviendas$SETU_CCNCT %in% as.character(su$SETU_CCNCT)
# No coicenden, pues 
# Para el caso de descarga de información con agregaciones sector urbano, 
# sección urbana o manzana censal, es importante 
# suprimir los digitos 7 y 8 del codigo censal antes de cruzar con la información de las
# capas correspondiente de la cartografía censal del MGN 

# caso de datos de personas
cp2005.personas$su_id %>% substr(1,6) -> c1
cp2005.personas$su_id %>% substr(9,20) -> c2

SETU_CCNCT<-paste0(c1,c2) 
nchar(SETU_CCNCT)
class(SETU_CCNCT)

cp2005.personas$SETU_CCNCT<-SETU_CCNCT

#buscar repetidos
cp2005.personas %>% 
  group_by(SETU_CCNCT) %>% 
  filter(n()>1)  


# hay 20 repetidos. ¿que hacer? 
# decisión: eliminar repetidos

setu.dup<-duplicated(cp2005.personas$SETU_CCNCT)
cp2005.personas<-cp2005.personas[!setu.dup,]


# caso de datos de viviendas
cp2005.viviendas$su_id %>% substr(1,6) -> c1
cp2005.viviendas$su_id %>% substr(9,20) -> c2

SETU_CCNCT<-paste0(c1,c2) 
nchar(SETU_CCNCT)
class(SETU_CCNCT)

cp2005.viviendas$SETU_CCNCT<-SETU_CCNCT

#buscar repetidos
cp2005.viviendas %>% 
  group_by(SETU_CCNCT) %>% 
  filter(n()>1)  


# hay 22 repetidos. ¿que hacer? 
# decisión: eliminar repetidos

setu.dup<-duplicated(cp2005.viviendas$SETU_CCNCT)
cp2005.viviendas<-cp2005.viviendas[!setu.dup,]

write.csv(cp2005.personas,file = "./outputData/cp2005.personas.csv")
write.csv(cp2005.viviendas,file = "./outputData/cp2005.viviendas.csv")

save(cp2005.personas,
     cp2005.viviendas,
     file = "cp2005.RData")



