library(ggplot2) #para gráficos
library(readr) #para leer csv
library(dplyr) #para manipular df
library(readxl) #para leer xlsx
library(stringi) #para eliminar tildes y volver minúscula a mayúsculas
library(tidyverse) #para hacer reemplazo de palabras (funcion 'str_replace')
#1.Importamos DFs
#1.1.Vacunados
vacunas_covid <- read_csv("rawdata/vacunas_covid.csv", 
                          col_types = cols(
                            FECHA_CORTE = col_date(format = "%Y%m%d"), 
                            FECHA_VACUNACION = col_date(format = "%Y%m%d")))
#1.2.Población
pob <- read_excel("rawdata/(PROCESADO) Población Total-Distritos INEI 2017.xlsX")
pert <- read_csv("rawdata/Distritos-Provincias-Departamentos.csv")

#2.Para no tener problemas con los nombres, vamos a hacer que
#(1)estén siempre en mayúscula y (2) se eliminen tildes, eñes,
#y diéresis.
vacunas_covid$DISTRITO=toupper(stri_trans_general
                               (vacunas_covid$DISTRITO,
                                 "Latin-ASCII"))
pob$NOMBDIST=toupper(stri_trans_general
                     (pob$NOMBDIST,"Latin-ASCII"))
pert$DISTRITO=toupper(stri_trans_general(
  pert$DISTRITO, "Latin-ASCII"))

#3.Le añadimos el CCDD de cada Departamento al df original
  #3.1.Comprobamos que los nombres sean iguales en ambos df
  nr1=select(vacunas_covid,DEPARTAMENTO)
  nr1=nr1[!duplicated(nr1), ] #Eliminamos duplicados
  nr2=select(pert,DEPARTAMENTO)
  nr2=nr2[!duplicated(nr2), ] 
    #3.1.1.Vemos cuántos son iguales
    igual=semi_join(nr1,nr2) #Los 25 departamentos están
                             #escritos igual.
    rm(nr1,nr2,igual)
  #3.2.Elegimos solo las columnas que nos interesan.
  dep=select(pert,DEPARTAMENTO,CCDD)
  dep=dep[!duplicated(dep),]  
  #3.3.Añadimos el CCDD al df original
  vacunas_covid=merge(vacunas_covid,dep,by="DEPARTAMENTO")

#4.Vamos a ver si los nombres de distritos del df coinciden con 
#los del df que tiene info de población por distrito

  #4.1.Seleccionamos aquellos del df de vacunados
  nd1=select(vacunas_covid,DISTRITO,CCDD) #Elegimos la columna
  nd1=nd1[!duplicated(nd1), ] #Eliminamos duplicados

  #4.2.Seleccionamos aquellos del df de distritos INEI
  nd2=select(pob,NOMBDIST,CCDD)
  names(nd2)[1]="DISTRITO"
    
  dif=anti_join(nd1,nd2)
  igual=semi_join(nd1,nd2) #Solo 11 distritos están incorrectos
  
  #4.3.Adapataremos el texto del df de NOTI-SINADEF al de INEI,
  #que ya tiene info de Población.
    #4.3.1.Encontraremos qué valores deben cambiarse
    prueba<-fuzzyjoin::stringdist_left_join(dif, 
                                        nd2, 
                                        by = 'DISTRITO',
                                        max_dist = 2)

    #4.3.2.Filtramos para que sea más preciso
    prueba2<-prueba %>%
    filter(CCDD.x==CCDD.y)
    #Encontramos 8 de 11 valores a reemplazar (como guía)
    rm(prueba,prueba2,nd1,nd2)

    #4.3.3.Empezamos con el cambio
    vacunas_covid<-vacunas_covid %>% 
    mutate(DISTRITO = str_replace(DISTRITO, 
                                  "ANCO-HUALLO","ANCO_HUALLO"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "HUAYLLO","IHUAYLLO"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "HUAYA","HUALLA"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "NAZCA","NASCA"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "PICHANAKI","PICHANAQUI"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "AYAUCA","ALLAUCA"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "VENTISEIS DE OCTUB","VEINTESEIS DE OCTUBRE"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "QUISQUI","QUISQUI (KICHKI)"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "LURIGANCHO (CHOSICA)","LURIGANCHO"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "MAGDALENA VIEJA (PUEBLO LIBRE)","PUEBLO LIBRE"))%>%
      mutate(DISTRITO = str_replace(DISTRITO, 
                                    "ALEXANDER VON HUMBO","ALEXANDER VON HUMBOLDT"))

#5.Añadimos el ubigeo al df de NOTI-SINADEF
dist=select(pob,NOMBDIST,UBIGEO,Población,IDPROV)
names(dist)[1]="DISTRITO"
  #5.1.Hacemos el merge
  vacunas_covid=merge(vacunas_covid,dist,by="DISTRITO")
  
#6.Exporto para no tener que repetir el proceso constantemente
library(openxl)  
write.csv(vacunas_covid,"data/vacunas_covid_procesado.csv")  
