library(tidyverse)
library(lubridate)
library(sf)

# 0 seteo ####
options(scipen = 999)

# 1 - levanto data de barrios y comunas de CABA ####
df_barrios<-st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")%>%
  st_transform(crs = 4326) #transformo el sistema de coordenadas
st_crs(df_barrios) #chequeo el sistema de coords

# levanto data de alquileres por airbnb
df_airbnb<-read.csv("./data/listings.csv", sep = ",", encoding = "UTF-8")

df_airbnb2<-st_as_sf(df_airbnb, coords = c("longitude", "latitude"), crs = 4326) #transformo el objeto y el sistema de coords
st_crs(df_airbnb2) #chequeo el sistema de coords

df_airbnb2_barrios<-st_join(df_airbnb2, df_barrios) # joineo 2 objetos espaciales

## 1.a Exploro un poco ####
glimpse(df_airbnb2_barrios)
unique(df_airbnb2_barrios$room_type)

exploro<-skimr::skim(df_airbnb2_barrios) #puede tardar un poquito
# rm(exploro)

## 1.b Termino de limpiar y ordenar el df ####

df_airbnb2_barrios<-df_airbnb2_barrios%>%
  mutate(last_review = ymd(last_review), 
         COMUNA = as.factor(COMUNA))

# 2 Estructura de un gráfico ####

mi_grafico<-ggplot(data, #llamamos a ggplot y definimos el df que vamos a graficar
                   aes(x=variable_independiente, y=variable_dependiente))+ #definimos los datos que van a ir en cada eje
  geom_point()+ #definimos la geometria del grafico
  geom_line()+ #podemos agregar o combinar geometrias
  scale_y_continuous()+ #definimos las escalas de los ejes
  scale_colour_brewer()+#definimos colores para las variables
  theme()+ #definimos elementos esteticos del grafico (fondo, bordes, tamano de letras,etc)
  facet_grid()+#podemos definir facetas (dividir en sub-graficos segun alguna variable)
  coord_flip()+#podemos definir el sistema de coordenadas
  labs() #definimos titulos o nombre de los ejes

# 3 Formas de escribir un gráfico ####

## 3.a Armamos el df por un lado y el plot por el otro ####
deptos_por_comuna<-df_airbnb2_barrios%>%
  group_by(COMUNA, room_type)%>%
  summarise(cantidad = n())

plot1_deptos_por_comuna<-ggplot(deptos_por_comuna,
                                aes(reorder(COMUNA, cantidad), cantidad, fill=room_type))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
plot1_deptos_por_comuna

## 3.b Armamos el df y agregamos el plot ####

deptos_por_comuna<-df_airbnb2_barrios%>%
  group_by(COMUNA, room_type)%>%
  summarise(cantidad = n())%>%
  ggplot(aes(reorder(COMUNA, cantidad),  
             cantidad, 
             fill=room_type))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
deptos_por_comuna

## 3.c Armamos el df dentro del plot ####

plot1_deptos_por_comuna<-ggplot(df_airbnb2_barrios%>%
                                  group_by(COMUNA, room_type)%>%
                                  summarise(cantidad = n()),
                                aes(reorder(COMUNA, cantidad), cantidad, fill=room_type))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
plot1_deptos_por_comuna

## 3.d Armamos el df dentro de la geometria (esta es la opcion mas "rara") ####

plot1_deptos_por_comuna<-ggplot()+
  geom_bar(data = df_airbnb2_barrios, 
           mapping = aes(COMUNA, fill = room_type), 
           stat = "count",
           position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
plot1_deptos_por_comuna

# 4 A graficar! ####
## 4.1 barplot - Gráfico de barras ####

deptos_por_comuna <- df_airbnb2_barrios%>%
  group_by(COMUNA, room_type)%>%
  summarise(cantidad = n())

#creo un lienzo
plot_1_deptos_por_comuna <- ggplot()

#agrego ejes
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(x = COMUNA, y = cantidad))

#agrego geometría
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(COMUNA, cantidad))+
  geom_bar(stat = "identity")

#agrego una variable
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(COMUNA, cantidad, fill = room_type))+
  geom_bar(stat = "identity")

#reordeno 
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(COMUNA, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")

#roto los ejes de coordenadas
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(COMUNA, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")+
  coord_flip()

#defino el tema general del plot
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(COMUNA, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()

#acomodo la referencia de colores y agrego títulos
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(COMUNA, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  theme_minimal()+
  theme(
    legend.position = c(.85,.3)
    # legend.position = "bottom"
    )+
  labs(x="",
       y="",
       title = "",
       legend ="")

## 4.2 scatterplot - Gráfico de puntos (2 variables numéricas) ####
min(df_airbnb2_barrios$price)
max(df_airbnb2_barrios$price)

plot_2_precios_reviews <- ggplot(df_airbnb2_barrios%>%
                                   filter(price<25000 & number_of_reviews > 0)%>%
                                   mutate(COMUNA = as.factor(COMUNA)), 
                                 aes(x = price, y = number_of_reviews, color = COMUNA))+
  geom_point(
    aes(shape = room_type),
    # size = 0.5,
    alpha = 0.6
  )+
  theme_minimal()+
  labs(x= "", 
       y= "", 
       title= "")
plot_2_precios_reviews 

## 4.3 linebar - Gráfico de líneas ####

reviews_por_fecha <- df_airbnb2_barrios%>%
  filter(!is.na(last_review))%>%
  mutate(mes_review = month(last_review, label = T, abbr = F))%>%
  group_by(mes_review)%>%
  summarise(cantidad=n())

plot_3_reviews_por_mes<-ggplot(reviews_por_fecha, 
                               aes(x = mes_review, y = cantidad))+
  geom_line(aes(group = 1),
            stat = "identity",
            size=1.3,
            color="salmon")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  labs()
plot_3_reviews_por_mes

reviews_por_fecha <- df_airbnb2_barrios%>%
  filter(!is.na(last_review))%>%
  mutate(mes_review = month(last_review, label = T, abbr = F))%>%
  group_by(mes_review, room_type)%>%
  summarise(cantidad=n())

plot_3b_reviews_por_mes<-ggplot(reviews_por_fecha, 
                               aes(x = mes_review, y = cantidad, color=room_type))+
  geom_line(aes(group = room_type),
            stat = "identity",
            size=1.3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  labs()
plot_3b_reviews_por_mes

## 4.4 heat map - Mapa de calor ####

precios_por_comuna <- df_airbnb2_barrios%>%
  filter(price<10000)%>%
  group_by(COMUNA, room_type)%>%
  summarise(precio = mean(price))

plot_4_precios_por_comuna <- ggplot(precios_por_comuna, 
                                    aes(COMUNA, room_type, fill = precio))+
  geom_tile(color = "white", 
            lwd = 1.5,
            linetype = 1)+
  geom_text(aes(label = str_c("$",round(precio, digits = 0))), color= "black", size = 3)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)
plot_4_precios_por_comuna

# 5 combinaciones y chiches ####

## scales ####
plot_3b_reviews_por_mes <- plot_3b_reviews_por_mes+
  # scale_y_sqrt()
  # scale_y_log10()
  scale_y_continuous(trans = "log2")
plot_3b_reviews_por_mes

## facets ####
plot_1_deptos_por_comuna <- plot_1_deptos_por_comuna+
  facet_wrap(~room_type, scales = "free")+#la virgulilla se puede escribir con alt+126
  theme(legend.position = "none")
plot_1_deptos_por_comuna

## texto ####

mejores_hospedadores <- df_airbnb2_barrios%>%
  group_by(host_id, host_name)%>%
  summarise(number_of_reviews = sum(number_of_reviews))%>%
  ungroup()%>%
  slice_max(order_by = number_of_reviews, n = 10)

plot_4_mejores_hospedadores <- ggplot(mejores_hospedadores, 
                                      aes(reorder(host_name, number_of_reviews),
                                          number_of_reviews,
                                          fill=number_of_reviews,
                                          label=number_of_reviews))+
  geom_bar(stat = "identity")+
  geom_text(color = "white",
            # size = 11, 
            hjust = 1.3)+
  coord_flip()+
  labs()
plot_4_mejores_hospedadores

## fuente ####
library(sysfonts)
library(showtext)

df_fuentes<-as.data.frame(font.files())

font.add(family = "Gabriola", regular = "Gabriola.ttf")
fuente<-"Gabriola"
showtext.auto()

plot_4_mejores_hospedadores<-plot_4_mejores_hospedadores+
  theme(text = element_text(family = fuente))
plot_4_mejores_hospedadores


font_add_google(name = "Encode Sans")
fuente<-"Encode Sans"
showtext::showtext_auto()

plot_4_mejores_hospedadores<-plot_4_mejores_hospedadores+
  theme(text = element_text(family = fuente))
plot_4_mejores_hospedadores

## colores ####
# 6 Guardemos nuestros plots ####

dir.create("./plots")

periodo<- "Mayo"

dir.create(paste0("./plots/", periodo))

ggsave(plot = plot_1_deptos_por_comuna, 
       file ="plot_1_deptos_por_comuna.png", 
       device = "png", 
       path = paste0("./plots/", periodo), 
       width = 10, height = 15)

# 7 Plotly ####

install.packages("plotly")
library(plotly)

ggplotly(plot_1_deptos_por_comuna)

# 8 Desafíos ####



