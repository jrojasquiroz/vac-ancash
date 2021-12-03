library(dplyr) #para manipular datos
library(readr) #para importar archivos
library(tidyverse)
library(zoo)

#1.Creamos un df que incluya info de vacunas solo para Ancash.
chimbote<-vacunas_covid%>%
  filter(DISTRITO == "CHIMBOTE" | DISTRITO =="NUEVO CHIMBOTE")
  #1.1.Lo ordenamos para que nos brinde info por fecha
  chimbote<-chimbote%>%
  group_by(FECHA_VACUNACION) %>%
  summarise(Vacunados=sum(Vacunados))

#2.Ahora calculamos los fallecimientos por Covid-19 para
#Chimbote y Nuevo Chimbote
  #2.1.Importamos
  fallecidos <- read_delim("G:/Mi unidad/Documentos personales/Covid-19 Chimbote/NOTI-SINADEF/fallecidos_covid.csv", 
                           ";", escape_double = FALSE, col_types = cols(FECHA_CORTE = col_date(format = "%Y%m%d"), 
                           FECHA_FALLECIMIENTO = col_date(format = "%Y%m%d")), 
                           trim_ws = TRUE)
  #2.2.Filtramos
  fallecidos<-fallecidos%>%
    filter(DISTRITO == "CHIMBOTE" | DISTRITO =="NUEVO CHIMBOTE")%>%
    filter(FECHA_FALLECIMIENTO <= '2021-07-30')
  
  #2.3.Lo ordenamos para que nos brinde info por fecha
    #2.3.1.Primero añadimos una columna que nos permita hacer la
    #tabla dinámica
    fallecidos$Muertes=rep(1)
    #2.3.2.Hacemos la tabla dinámica
    fallecidos<-fallecidos%>%
    group_by(FECHA_FALLECIMIENTO) %>%
    summarise(Muertes=sum(Muertes))
    head(fallecidos)
    #vemos que la fecha inicia en 2020-03-27 y termina en 2021-07-30

#3.Ordenamos ambos df para luego poder unirlos. Lo que necesitamos
#es que empiecen y terminen en las mismas fechas, y que tengan la misma
#cantidad de días
  #3.1.Indicamos las fechas que nos interesan
  inicio=as.Date('2020-03-27')
  fin=as.Date('2021-07-30')
  #3.2.DF de vacunas
  chimbote <- chimbote%>%
    complete(FECHA_VACUNACION = seq.Date(min(inicio), 
                                        max(fin), 
                                        by = "day")) %>%
    complete(fill=list(Vacunados=0))
  #3.3.DF de fallecimientos
  fallecidos <- fallecidos%>%
    complete(FECHA_FALLECIMIENTO = seq.Date(min(inicio), 
                                         max(fin), 
                                         by = "day"))%>%
    complete(fill=list(Muertes=0))
  head(fallecidos)
  #3.4.Le ponemos el mismo nombre a la columna de fechas para
  #poder hacer el match
  names(chimbote)[1]="FECHA"
  names(fallecidos)[1]="FECHA"    

#4.Obtenemos un solo df con info de fallecidos y vacunados por día
final <- chimbote%>%
  merge(fallecidos,by="FECHA")
  #4.1.Por alguna razón los campos NA no se han cambiado por ceros
  #en los pasos 3.2 y 3.3. Desconozco la razón,
  #pero los llenamos antes de calcular la media móvil.
  final[is.na(final)] <- 0
  #4.1.Calculamos media móvil de fallecidos y ponderados
  final <- final %>%
  mutate(Vacunados_MM = rollmean(Vacunados, k = 15, fill = NA),)%>%
  mutate(Muertes_MM = rollmean(Muertes, k = 15, fill = NA),)
  
#5.Graficamos
  #5.1.Definimos algunas fechas importantes para hacer anotaciones en el
  #gráfico. Se pueden revisar en el archivo rawdata/Calendario de vacunacion - Chimbote y Nvo Chimbote.odt
  xnav=as.Date("2020-12-24")
  xm=as.Date("2020-05-10")
  ss20=as.Date("2020-04-05") #0 fallecidos
  ss21=as.Date("2021-03-28") #10.60 fallecidos
  x80=as.Date("2021-04-28")
  x70=as.Date("2021-05-21")
  x60=as.Date("2021-06-08")
  x55=as.Date("2021-07-11")
  xd=as.Date("2021-05-21")
  #5.2.Definimos el estilo del fondo del gráfico, para que no se vean
  #los ejes
  estilo <-   theme(panel.grid.major.x = element_line(color = "#ffffff",size=0.3))+
    theme(panel.grid.minor.x = element_line(color = "#ffffff",size=0.3))+
    theme(panel.grid.major.y = element_line(color = "#ffffff",size=0.3))+
    theme(panel.grid.minor.y = element_line(color = "#ffffff",size=0.3))
  #5.3.Graficamos
  ggplot(final) +
    aes(x = FECHA, y = Muertes_MM) +
    geom_line(size = .35, colour = "black") +
    theme_minimal()+
    labs(x="",y="",title="Fallecimientos diarios por Covid-19",
         subtitle="Chimbote y Nuevo Chimbote",
         caption="Fuente: Ministerio de Salud y Dirección Regional de Salud de Áncash")+
    annotate(geom = "curve", x = x80-15, y = 19, xend = x80, yend = 17.23, 
             curvature = -.5, colour="#79745C", arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = x80-16, y = 19, 
             colour="#79745C",label = "Empieza vacunación 80+", hjust = "right")+
    annotate(geom = "curve", x = x70+15, y = 13.2, xend = x70, yend = 11.95, 
             curvature = .3, colour="#79745C", arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = x70+18, y = 13.2, 
             colour="#79745C",label = "Empieza vacunación 70+", hjust = "left")+
    annotate(geom = "curve", x = x60+15, y = 7.3, xend = x60, yend = 5.6, 
             curvature = .3, colour="#79745C",arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = x60+18, y = 7.3, label = "Empieza vacunación 60+, 
Síndrome de Down
y enfermedades renales", colour="#79745C", hjust = "left")+
    annotate(geom = "curve", x = x55+15, y = 2.96, xend = x55, yend = 1, 
             curvature = .3, colour="#79745C", arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = x55+18, y = 2.96, 
             colour="#79745C", label = "Empieza 
vacunación 55+", hjust = "left")+
    annotate(geom = "curve", x = xnav-15, y = 5.06, xend = xnav, yend = 4.06, 
             curvature = -.3, colour="#871709",arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = xnav-16, y = 5.06, colour="#871709",
             label = "Navidad", hjust = "right")+
    annotate(geom = "curve", x = xm-15, y = 12.6, xend = xm, yend = 11.8, 
             curvature = -.3, colour="#871709",arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = xm-16, y = 12.6, label = "Día de la madre", 
             colour="#871709",hjust = "right")+
    annotate(geom = "curve", x = ss21-15, y = 11.6, xend = ss21, yend = 10.6, 
             curvature = -.3,colour="#871709", arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = ss21-16, y = 11.6, label = "Semana Santa", 
             colour="#871709",hjust = "right")+
    annotate(geom = "curve", x = ss20-15, y = 1, xend = ss20, yend = 0.35, 
             curvature = -.3, colour="#871709", arrow = arrow(length = unit(1, "mm"))) +
    annotate(geom = "text", x = ss20-16, y = 1, label = "Semana
  Santa",colour="#871709",hjust = "right")+
    scale_x_date(date_labels = "%b %Y",date_breaks = "3 month",
                 limits = as.Date(c('2020-02-15','2021-10-25')))+
    theme(plot.title =element_text(vjust=2,face="bold"),
          plot.subtitle =element_text(vjust=2,face="bold"))+
    estilo
  
    ggsave(
    "imagenes/Fallecidos diarios con hitos - Chimbote - v2.jpg",
    plot = last_plot(),
    width = 21,
    height = 15.54,
    units="cm",
    dpi = 1200,)
