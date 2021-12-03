##[RESULTADOS] 1.prov5, que nos permite obtener vacunados de cada
##             provincia de Ancash por fecha. (De aquí salen
##             gráficos 1 y 2)
##             2.prov_pr, que nos permite saber el porcentaje de 
##             vacunados por cada provincia de Ancash.

library(dplyr) #para manipular datos
library(zoo) #para crear medias móviles
library(ggplot2) #para gráficos
library(tidyverse) #para hacer reemplazo de palabras (funcion 'str_replace')
                   #y rellenar fechas

#1.Creamos un df que incluya info de vacunas solo para Ancash.
prov <- vacunas_covid %>%
  filter(DEPARTAMENTO=="ANCASH")
  #1.1.Lo ordenamos para que nos brinde info de vacunados
  #por cada provincia
  prov<-prov%>%
  group_by(IDPROV,PROVINCIA,FECHA_VACUNACION) %>%
  summarise(Vacunados=sum(Vacunados))

#2.Calculamos la población de cada provincia con datos 
#INEI.
pobprov<-pob %>%
  group_by(IDPROV)%>%
  summarise(Pob=sum(Población))

#3.Unimos el df que ya tiene info sobre vacunados con el de
#la población de cada provincia de Ancash.
prov<-prov%>%
  merge(pobprov,by="IDPROV")

#4.Calculamos los vacunados por cada 100.000 habitantes
prov$VacunadosPond=prov$Vacunados/prov$Pob*100000

#5.Y ahora la media móvil cada 7 días
prov2 <- prov %>%
  select(PROVINCIA,FECHA_VACUNACION, 
         Vacunados,VacunadosPond) %>%
  group_by(PROVINCIA)%>%
  mutate(VacunadosPond_MM = rollmean(VacunadosPond, k = 7, fill = NA),)

#5.Lo que necesitamos ahora es ordenar el df. Como se puede ver,
#la fecha de inicio y fin de vacunación es distinta para cada
#provincia. Y entre ella hay fechas sin vacunación.
#Entonces, para que se vea más bonito, elijamos unas fechas 
#estándar y rellenémoslas con ceros cuando hayan vacunado esos 
#días.
  #5.1.Elijamos las fechas de inicio y fin estándar
  #para todas las provincias.
  inicio=as.Date("2021-02-01")
  fin=as.Date("2021-07-31")

  #5.1.Hay dos maneras de mejorar el df:
  #5.1.1.Esto si quisiera que respete la fecha de inicio y fin de
  #cada provincia y solo rellene las fechas que hay entre ellas.
  prov4<- prov2 %>% 
  group_by(PROVINCIA) %>%
  complete(FECHA_VACUNACION = full_seq(FECHA_VACUNACION, 1), 
           fill = list(Vacunados = 0))

  #5.2.Esto si quisiera señalar una fecha de inicio y fin 
  #para cada provincia a mi antojo.
  prov5 <- prov2%>%
  group_by(PROVINCIA)%>%
  complete(FECHA_VACUNACION = seq.Date(min(inicio), 
                                       max(fin), 
                                       by = "day")) %>%
  complete(fill=list(Vacunados=0,VacunadosPond=0))

#6.Creamos la variable de media móvil de vacunados por cada
#100.000 habitantes.
prov5 <- prov5 %>%
  select(PROVINCIA,FECHA_VACUNACION, 
         Vacunados,VacunadosPond) %>%
  group_by(PROVINCIA)%>%
  mutate(VacunadosPond_MM = rollmean(VacunadosPond, k = 30, fill = NA),)

#7.Y también la media móvil del total de vacunados.
prov5 <- prov5 %>%
  select(PROVINCIA,FECHA_VACUNACION, 
         Vacunados,VacunadosPond,VacunadosPond_MM) %>%
  group_by(PROVINCIA)%>%
  mutate(Vacunados_MM = rollmean(Vacunados, k = 30, fill = NA),)

  #[BONUS]
  #La provincia Carlos Fermin Fitzcarrald tiene un nombre muy largo
  #para los gráficos. La editaremos
  prov5<-prov5%>%
  mutate(PROVINCIA = str_replace(PROVINCIA, 
                              "CARLOS FERMIN FITZCARRALD",
                              "CARLOS FERMIN F."))

#8.Graficamos
  #8.1.Grafico 1
  ggplot(prov5) +
  aes(x = FECHA_VACUNACION, y = VacunadosPond_MM) +
  geom_line(size = 0.6, colour = "#44b470") +
  labs(
    x = " ",
    y = "",
    title="Provincias de la región Áncash",
    subtitle="Vacunación diaria por cada 100.000 habitantes hasta el 30 de julio de 2021",
    caption="Fuente: MINSA e INEI") +
  theme_minimal() +
  theme(plot.title =element_text(vjust=2,face="bold"),
        plot.subtitle =element_text(vjust=2,face="bold"))+
  facet_wrap(vars(PROVINCIA))
  
  ggsave("imagenes/VacunadosPond_Tiempo.jpg",
    plot = last_plot(),
    width = 20,
    height = 20,
    units="cm",
    dpi = 1200,
  )
  
  #8.2.Grafico 2
  ggplot(prov5) +
  aes(x = FECHA_VACUNACION, y = Vacunados_MM) +
  geom_line(size = 0.5, colour = "#44b470") +
  labs(
    x = " ",
    y = " ",
    title="Provincias de la región Áncash",
    subtitle="Vacunación diaria hasta el 30 de julio de 2021",
    caption="Fuente: MINSA") +
  theme_minimal() +
  theme(plot.title =element_text(vjust=2,face="bold"),
        plot.subtitle =element_text(vjust=2,face="bold"))+
  facet_wrap(vars(PROVINCIA))
  
  ggsave(
    "imagenes/Vacunados_Tiempo.jpg",
    plot = last_plot(),
    width = 20,
    height = 20,
    units="cm",
    dpi = 600,
  )

#9.Ahora calculamos el porcentaje de vacunados en cada provincia
  #9.1.Primero creamos un nuevo df que nos calcule el total de
  #vacunados por provincia
  prov_pr<-prov%>%
  group_by(PROVINCIA)%>%
  summarise(Vacunados=sum(Vacunados))
  
  #9.2.Ahora calculamos la población de cada una.
    #9.2.1.Primero identificamos el nombre de cada 'IDPROV'
    pertp<-pert%>%
    select(IDPROV,PROVINCIA)
    pertp=pertp[!duplicated(pertp),]
    #9.2.2.Ahora la unimos al df que ya habíamos calculado antes
    #con la población de cada provincia.
    #Obtenemos un df que tiene IDPROV,NOMBRE DE PROVINCIA y POBLACIÓN.
    pobprov<-pobprov%>%
    merge(pertp,by="IDPROV")
    #9.2.3.Unimos lo obtenido en los dos pasos anteriores al df
    #que obtuvimos en el paso 9.1.
    prov_pr<-prov_pr%>%
    merge(pobprov,by="PROVINCIA")
  
  #9.3.Calculamos el porcentaje de poblacion vacunada por 
  #cada provincia
  prov_pr$VacunadosPR=prov_pr$Vacunados/prov_pr$Pob*100
  
  
  ggplot(prov_pr, aes(reorder(PROVINCIA,VacunadosPR), VacunadosPR)) +
    geom_bar(stat = "identity", 
             width = 0.75,
             position="identity",
             colour = "#44b470",
             fill = "#44b470") +
    coord_flip() +
    xlab("")+ylab("")+
    labs(title="Provincias de la región Áncash",
         subtitle="Porcentaje de población vacunada hasta el 30 de julio de 2021",
         caption="Fuente: MINSA e INEI")+
    theme_classic()+
    theme(panel.grid.major.x = element_line(color = "#c2c2c2",size=0.3))+
    theme(panel.grid.minor.x = element_line(color = "#c2c2c2",size=0.3))+
    theme(plot.title =element_text(vjust=2,face="bold"),
          plot.subtitle =element_text(vjust=2,face="bold"))
  
    ggsave(
    "imagenes/Porcentaje de vacunados.jpg",
    plot = last_plot(),
    width = 20,
    height = 20,
    units="cm",
    dpi = 600,
      )


##Comentario: en el paso 9.2. hemos dado muchas vueltas básicamente
#por haber diagramado incorrectamente el df original (tenía
#IDPROV pero no nombre de provincias. Y cuando intentaba hacer el 
#'group_by' me daba problemas al considerar el IDPROV). 
#Debemos tener más cuidado para próximas ocasiones.