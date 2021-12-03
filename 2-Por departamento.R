names(vacunas_covid)
vacunas_covid$Vacunados=rep(1)
vacunas_covid$Muertes<-NULL

vacunas_covid$Vacunados=rep(1)
names(vacunas_covid)

library(dplyr)
din <- vacunas_covid %>%
  group_by(CCDD,DEPARTAMENTO,FECHA_VACUNACION) %>%
  summarise(Vacunados=sum(Vacunados))


pobdep<-pob %>%
  group_by(CCDD)%>%
  summarise(Pob=sum(Población))

din<-din%>%
  merge(pobdep,by="CCDD")

din$VacunadosPond=din$Vacunados/din$Pob*100000
names(din)

library(zoo)
din2 <- din %>%
  select(CCDD, DEPARTAMENTO, FECHA_VACUNACION, 
         Vacunados,VacunadosPond) %>%
  group_by(DEPARTAMENTO)%>%
  mutate(VacunadosPond_MM = rollmean(VacunadosPond, k = 7, fill = NA),)


library(ggplot2)

ggplot(din2) +
  aes(x = FECHA_VACUNACION, y = VacunadosPond_MM) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = " ",
    y = "Vacunados por cada 100.000 habitantes") +
  theme_minimal() +
  facet_wrap(vars(DEPARTAMENTO))

din_pr<-din%>%
  group_by(CCDD,DEPARTAMENTO,Pob)%>%
  summarise(Vacunados=sum(Vacunados))

din_pr$VacunadosPR=din_pr$Vacunados/din_pr$Pob*100
