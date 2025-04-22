library(tidyverse)
library(lubridate)

#1 levantamos la base de datos ####

base <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2019.csv",
                          encoding = "UTF-8")

#exploramos un poco
glimpse(base)

unique(base$franja_horaria)#me fijo los valores únicos de la variable franja porque nosé a qué se refiere
unique(base$tipo_delito)

base<-base%>%
  filter(!franja_horaria=="S/D")%>%
  mutate(tipo_delito=as.factor(tipo_delito),
         fecha=dmy(fecha))%>%
  mutate(horario=as.numeric(franja_horaria))

#2 Estructura de un gráfico ####

#3 Formas de escribir un gráfico ####


#4.1 barplot - Gráfico de barras ####

base_comuna<-base%>%
  group_by(comuna, tipo_delito)%>%
  summarise(cantidad=n())

plot_delito_comuna<-ggplot(base_comuna, aes(x=comuna, y=cantidad, fill=tipo_delito))+
  geom_bar(stat="identity", position = "dodge")
plot_delito_comuna

#mejoremos un poco el gráfico 
base_comuna<-base%>%
  group_by(comuna, tipo_delito)%>%
  summarise(cantidad=n())%>%
  ungroup()%>%
  filter(!is.na(comuna))%>%
  mutate(comuna=as.factor(comuna))

plot_delito_comuna<-ggplot(base_comuna, aes(x=reorder(comuna,cantidad),y=cantidad, fill=tipo_delito))+
  # geom_bar(stat="identity", position = "stack")+
  # geom_bar(stat="identity", position = "dodge")+
  geom_bar(stat="identity", position = "fill")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 20))+
  labs(x="Comuna", y="Cantidad",
       title = "Cantidad de delitos por comuna",
       caption = "Fuente:datos abiertos GCBA")
plot_delito_comuna

#4.2 linebar - Gráfico de líneas ####

base_delitos_fecha<-base%>%
  group_by(fecha, tipo_delito)%>%
  summarise(cantidad=n())

plot_delitos_fecha<-ggplot(base_delitos_fecha, aes(x=fecha, y=cantidad, color=tipo_delito))+
  geom_line(size=1, alpha=0.1)
  # geom_smooth()
plot_delitos_fecha

#4.3 scatterplot - Gráfico de puntos ####

plot_delitos_tipo<-ggplot(base%>%
                            filter(barrio=="Palermo"), aes(x=long, y=lat, color=tipo_delito))+
  geom_jitter()
plot_delitos_tipo


plot_delitos_tipo<-ggplot(base%>%
                            filter(barrio%in%c("Palermo", "Paternal", "Flores", "Liniers")),
                          aes(x=long, y=lat, color=tipo_delito))+
  geom_jitter()
  # facet_grid(~barrio, scales = "free_y")
plot_delitos_tipo

#este plot puede tardar en cargar
plot_delitos_tipo<-ggplot(base,aes(x=long, y=lat, color=tipo_delito))+
  geom_jitter()+
  facet_wrap(~tipo_delito)
plot_delitos_tipo

#4.4 heat map - Mapa de calor ####

base_dias_horarios<-base%>%
  mutate(dia=wday(fecha, label = T))%>%
  group_by(dia, horario)%>%
  summarise(cantidad=n())

plot_dias_horarios<-ggplot(base_dias_horarios,aes(x=dia, y=horario, fill=cantidad))+
  geom_tile()+
  # scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  labs(title="Cantidad de delitos por día y hora")
plot_dias_horarios

#5 combinaciones y chiches ####

#vamos a combinar lineas y puntos
base_delitos_fecha_2<-base%>%
  mutate(mes=round_date(fecha, unit = "month"))%>% #redondeamos por mes
  group_by(mes, tipo_delito)%>% #agrupamos por mes
  summarise(cantidad=n())

plot_delitos_mes<-ggplot(base_delitos_fecha_2, aes(x=mes, y=cantidad, color=tipo_delito))+
  geom_line()+
  geom_point()
plot_delitos_mes

#vamos a manipular el eje x de un plot anterior que tiene formato fecha

plot_delitos_fecha<-plot_delitos_fecha+
  scale_x_date(date_breaks = "1 month",
  date_labels = "%Y %b")
plot_delitos_fecha

#vamos a superponer plots
plot_areas<-ggplot()+
  geom_area(data = base_delitos_fecha_2%>%
              filter(tipo_delito=="Robo (con violencia)"), 
            mapping = aes(x=mes, y=cantidad,fill=tipo_delito),alpha=0.5)+
  geom_area(data = base_delitos_fecha_2%>%
              filter(tipo_delito=="Hurto (sin violencia)"), 
            mapping = aes(x=mes, y=cantidad,fill=tipo_delito),alpha=0.5)+
  geom_area(data = base_delitos_fecha_2%>%
              filter(tipo_delito=="Lesiones"), 
            mapping = aes(x=mes, y=cantidad,fill=tipo_delito),alpha=0.5)+
  geom_area(data = base_delitos_fecha_2%>%
              filter(tipo_delito=="Homicidio"), 
            mapping = aes(x=mes, y=cantidad,fill=tipo_delito),alpha=1)+
  theme_minimal()+
  labs()
plot_areas

#vamos a agregar colores que fijamos manualmente
unique(base$tipo_delito) #primero nos fijamos las categorias que tiene la variable

colores_tipo_delito<-c("Homicidio"="#9FE73B",
                       "Hurto (sin violencia)"="#57BEC3",
                       "Robo (con violencia)"="#E13586",
                       "Lesiones"="#A7DCDE") #definimos los colores a usar

plot_areas_2<-plot_areas+
  scale_fill_manual(values = colores_tipo_delito)
plot_areas_2

#otros tipos de escalas de colores

plot_delito_comuna<-plot_delito_comuna+
  scale_fill_brewer(palette = "Dark2")
plot_delito_comuna

#6 guardar gráficos en png ####

dir.create("plots")
path_plots<-("./plots")

ggsave(filename = "1 Cantidad de delitos por comuna.png",
       plot = plot_delito_comuna,
       device = "png",
       path = path_plots,
       width = 20,
       height = 15,
       units = "cm")

#7.1 YAPA1: plotly #### 
library(plotly)

ggplotly(plot_delito_comuna, tooltip = c("tipo", "cantidad"))

#7.2 YAPA2: gganimate ####
# install.packages("runner")
library(runner)
library(gganimate)

base_comuna_fecha<-base%>%
  group_by(fecha, comuna)%>%
  summarise(cantidad=n())%>%
  ungroup()%>%
  filter(!is.na(comuna))%>%
  arrange(comuna)%>%
  mutate(suma=sum_run(x = cantidad,k=1, idx = comuna),
         comuna=as.factor(comuna))%>%
  select(fecha,comuna,suma)

plot_crecimiento<-ggplot(base_comuna_fecha, aes(x=reorder(comuna,suma), suma, fill=comuna))+
  geom_bar(stat="identity")+
  geom_text(aes(x=1, y=18000, label=fecha, size=20))+
  coord_flip()+
  transition_time(time = fecha)+
  # transition_states(states = fecha)+
  # transition_components(time = fecha)+
  labs(title = "Cantidad de delitos")
plot_crecimiento