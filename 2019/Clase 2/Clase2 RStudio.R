#cargamos librerias
install.packages("ggplot")
library(ggplot2)
library(tidyverse)
data<-read.csv("resultados-elecciones-legisladores-generales-2017.csv",encoding="UTF-8",sep=",")
#encoding es para decirle a R cómo tiene que leer el dataset, porque cada software
#lee y codifica en su propio abecedario, el UTF-8 es un tipo de abecedario

head(data)
#head(data) es para ver las 6 lineas del data
colnames(data)
#colnames(data) es para ver las primeras 10 variables
View(data)
#view(data) es para visualizar el dataset ya ordenado
data$total_votos
#es una forma de indicarle a R que vamos a trabajar sobre una columna (total votos)

mean(data$total_votos)
#para el promedio de votos

sum(data$total_votos)
#suma de votos total

median(data$total_votos)
#mediana
max(data$total_votos)

min(data$total_votos)

votos_por_partido<-data%>%
group_by(partido_descripcion)%>%
  group_by(partido_codigo)
summarise(sum(total_votos))%>%
rename(votos="sum(total_votos)")
View(votos_por_partido)


grafico_por_partido<-votos_por_partido%>%
  ggplot(aes(x=partido_descripcion,y=votos))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90,vjust = 0.6))
grafico_por_partido

data_viz_2<-data%>%
  subset(partido_descripcion=="ALIANZA VAMOS JUNTOS"|partido_descripcion=="ALIANZA UNIDAD PORTEÑA")%>%
  filter(mesa_numero<300)%>%
  ggplot(aes(x=mesa_numero,y=total_votos,col=partido_descripcion))+
  geom_jitter()+
  geom_smooth(aes(col=partido_descripcion),method = "loess", se=F)
labs(title="Grafico de Dispersión",caption = "Elecciones 2017")
data_viz_2

data_viz_3<-data%>%
  subset(partido_descripcion=="A.F.DE I.Y DE LOS TRABAJADORES"|partido_descripcion=="ALIANZA EVOLUCION"|partido_descripcion=="ALIANZA UNIDAD PORTEÑA")%>%
  filter(seccion_codigo==15)%>%
  ggplot(aes(x=mesa_numero,y=total_votos,col=partido_descripcion))+
  geom_jitter()+
  geom_smooth(aes(col=partido_descripcion),method = "loess", se=F)
labs(title="Grafico de Dispersión2",caption = "Elecciones 2017")
data_viz_3

mesas_uc <- data %>%
  filter((partido_descripcion=="ALIANZA UNIDAD PORTEÑA"))
head(mesas_uc)

media_circuitos <- data%>%
  filter(partido_descripcion=="ALIANZA UNIDAD PORTEÑA")%>%
  group_by(circuito_codigo)%>%
  summarize(mediana=mean(total_votos), sd=sd(total_votos), desvio=mean(total_votos)-sd(total_votos)*2)


data_desvios <- inner_join(mesas_uc, media_circuitos,by="circuito_codigo")

uc_desviadas <- data_desvios %>%
  mutate(desviadas = total_votos - desvio) %>%
  filter(desviadas < 0)


grafico_tarea <- data%>%
  subset(partido_descripcion=="A.AVANCEMOS HACIA 1 PAIS MEJOR"|)
  
  
