#### Librerías ####

library(tidyverse) #Nos permite trabajar con tablas (operaciones tipo excel)
library(lubridate) #Nos permite formatear las fechas
library(sf) #Nos permite trabajar con mapas (lo vamos a ver con más detalle en el futuro)
library(skimr) #Una librería para explorar los datos (ver cuantos faltan en cada columna etc.)
library(janitor) #Cumple varias funciones de limpieza de datos
library(sysfonts) #Para usar fuentes que tenemos en el sistema
library(showtext) #Para aplicar texto
library(plotly) #Otra librería de gráficos (se usa mucho en python)


#### 0 seteo ####
options(scipen = 999) #Seteamos para que los números no nos aparezcan en formato científico

#### 1. Datos ####
##### 1.1. levanto data de barrios y comunas de CABA #####

df_barrios <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")%>%
  st_transform(crs = 4326) #transformo el sistema de coordenadas 

st_crs(df_barrios) #chequeo el sistema de coords

###### 1.2. levanto data de alquileres por airbnb
df_airbnb <- read.csv("./data/listings.csv", 
                      sep = ",", #Defino el separador de las columnas
                      encoding = "UTF-8") #El encoding de las letras

# Transformo la tabla en csv en un objeto gegráfico
df_airbnb2 <- st_as_sf(df_airbnb, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326) #transformo el objeto y el sistema de coords
st_crs(df_airbnb2) #chequeo el sistema de coords

##### 1.2. join de tablas #####
df_airbnb2_barrios <- st_join(df_airbnb2, df_barrios) # joineo 2 objetos espaciales

##### 1.3. Exploración #####

glimpse(df_airbnb2_barrios) #Listamos las columnas y los tipos de datos

unique(df_airbnb2_barrios$room_type) #Unique para ver que valores tiene una columna

explore <- skim(df_airbnb2_barrios) #puede tardar un poquito
explore

rm(explore) #la función rm elimina un objeto de la memoria

##### 1.4. Limpieza de los datos #####
df_airbnb2_barrios <- df_airbnb2_barrios %>%
  clean_names() %>% #de janitor Pone todos los nombres en minuscula, elimina espacios etc.
  mutate(last_review = ymd(last_review)) %>% # con la función "ymd" formateo las fechas y le digo a r como leerla
  mutate(comuna = as.factor(comuna)) #Le decimos a R que el número de columna es una categorización y no un número

#### 2. GRÁFICOS ####
##### 2.1. Estructura de un gráfico #####

mi_grafico <- ggplot(data, #llamamos a ggplot y definimos el df que vamos a graficar
                   aes(x=variable_independiente,
                       y=variable_dependiente))+ #definimos los datos que van a ir en cada eje
  geom_point()+ #definimos la geometría del grafico
  geom_line()+ #podemos agregar o combinar geometrias
  scale_y_continuous()+ #definimos las escalas de los ejes
  scale_colour_brewer()+ #definimos colores para las variables
  theme()+ #definimos elementos esteticos del grafico (fondo, bordes, tamano de letras,etc)
  facet_grid()+#podemos definir facetas (dividir en sub-graficos segun alguna variable)
  coord_flip()+#podemos definir el sistema de coordenadas
  labs() #definimos titulos o nombre de los ejes

##### 2.2. Formas de escribir un gráfico ####

##### 2.2.1. Un gráfico simple: deptos por barrio #####

deptos_x_barrio <- df_airbnb2_barrios %>%
  group_by(barrio) %>% # Agrupamos los datos por barrio
  summarise(cantidad = n())

chart_datos_x_barrio <- deptos_x_barrio %>%
  ggplot(aes( #parámetros "aesthetics" de ggplot
    x = barrio, #Variable independiente
    y = cantidad)) + #Variable dependiente
  geom_col() #Tipo de gráfico
  
chart_datos_x_barrio #Pero no se entiende nada ;)


##### 2.2.2.Un gráfico un poco más tunneado ####
deptos_por_comuna <- df_airbnb2_barrios%>%
  group_by(comuna, room_type)%>%
  summarise(cantidad = n())
deptos_por_comuna

plot1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                aes(x = reorder(comuna, cantidad), #le decimos que reordene una columna según los valores de otra 
                                    y = cantidad,
                                    fill = room_type)) + #relleno de acuerdo a una columna
  geom_bar(stat = "identity", 
           position = "stack") + #stak apiladas
  coord_flip() + #cambiamos coordenadas (la x en vertical por ejemplo)
  labs(title = "Oferta por tipo y comuna") #le ponemos un titul
plot1_deptos_por_comuna

##### 2.2.3. Todo junto procesamiento de los datos + gráfico #####

deptos_por_comuna <- df_airbnb2_barrios%>%
  group_by(comuna, room_type)%>%
  summarise(cantidad = n())%>%
  ggplot(aes(reorder(comuna, cantidad),  
             cantidad, 
             fill=room_type))+
  geom_bar(stat = "identity",
           position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
deptos_por_comuna

##### 2.2.4. Armamos el df directo dentro del plot #####

plot1_deptos_por_comuna <- ggplot(df_airbnb2_barrios%>%
                                  group_by(comuna, room_type)%>%
                                  summarise(cantidad = n()),
                                aes(reorder(comuna, cantidad), 
                                    cantidad, fill = room_type))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
plot1_deptos_por_comuna

## 2.2.5. Armamos el df dentro de la geometria (esta es la opcion mas "rara") ####

plot1_deptos_por_comuna <- ggplot()+
  geom_bar(data = df_airbnb2_barrios, 
           mapping = aes(comuna, 
                         fill = room_type), 
           stat = "count",
           position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
plot1_deptos_por_comuna

#### 3. A graficar! ####

##### 3.1 barplot - Gráfico de barras #####
deptos_por_comuna <- df_airbnb2_barrios%>%
  group_by(comuna, room_type)%>%
  summarise(cantidad = n())

###### 3.1.1. creo un lienzo ######
plot_1_deptos_por_comuna <- ggplot()

###### 3.1.2. agrego ejes ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(x = comuna, y = cantidad))

###### 3.1.3. agrego geometría ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(comuna, cantidad))+
  geom_bar(stat = "identity")

###### 3.1.4. agrego una variable ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(comuna, cantidad, fill = room_type))+
  geom_bar(stat = "identity")

###### 3.1.5. reordeno ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(comuna, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")

###### 3.1.6. roto los ejes de coordenadas ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(comuna, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")+
  coord_flip()

###### 3.1.7. defino el tema general del plot ######
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(comuna, cantidad),
                                       cantidad, 
                                       fill = room_type))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()

###### 3.1.8. acomodo la referencia de colores y agrego títulos
plot_1_deptos_por_comuna <- ggplot(deptos_por_comuna,
                                   aes(reorder(comuna, cantidad),
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

##### 3.2. scatterplot - Gráfico de puntos (2 variables numéricas) #####
min(df_airbnb2_barrios$price)
max(df_airbnb2_barrios$price)

plot_2_precios_reviews <- ggplot(df_airbnb2_barrios%>%
                                   filter(price<25000 & number_of_reviews > 0)%>%
                                   mutate(comuna = as.factor(comuna)), 
                                 aes(x = price, y = number_of_reviews, color = comuna))+
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

##### 3.3. linebar - Gráfico de líneas #####
reviews_por_fecha <- df_airbnb2_barrios%>%
  filter(!is.na(last_review))%>%
  mutate(mes_review = month(last_review, label = T, abbr = F))%>%
  group_by(mes_review)%>%
  summarise(cantidad=n())

###### 3.3.1. Gráfico de lineas por mes ######
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

###### 3.3.2. Gráfico de lineas por fecha ######
reviews_por_fecha <- df_airbnb2_barrios%>%
  filter(!is.na(last_review))%>%
  mutate(mes_review = month(last_review, label = T, abbr = F))%>%
  group_by(mes_review, room_type)%>%
  summarise(cantidad=n())

plot_3b_reviews_por_mes <- ggplot(reviews_por_fecha, 
                               aes(x = mes_review,
                                   y = cantidad, 
                                   color=room_type))+
  geom_line(aes(group = room_type),
            stat = "identity",
            size=1.3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  labs()
plot_3b_reviews_por_mes

###### 3.1.3. Heat Map - Mapa de calor ###### 

precios_por_comuna <- df_airbnb2_barrios%>%
  filter(price<10000)%>%
  group_by(comuna, room_type)%>%
  summarise(precio = mean(price))

plot_4_precios_por_comuna <- ggplot(precios_por_comuna, 
                                    aes(comuna, room_type, fill = precio))+
  geom_tile(color = "white", 
            lwd = 1.5,
            linetype = 1)+
  geom_text(aes(label = str_c("$",round(precio, digits = 0))), color= "black", size = 3)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)
plot_4_precios_por_comuna

##### 3.4. Otras combinaciones y chiches ####

###### 3.4.1. Scales ###### 
plot_3b_reviews_por_mes <- plot_3b_reviews_por_mes+
  # scale_y_sqrt()
  # scale_y_log10()
  scale_y_continuous(trans = "log2")
plot_3b_reviews_por_mes

###### 3.4.2. Facets #####
plot_1_deptos_por_comuna <- plot_1_deptos_por_comuna+
  facet_wrap(~room_type, scales = "free")+#la virgulilla se puede escribir con alt+126
  theme(legend.position = "none")
plot_1_deptos_por_comuna

######  3.4.3. Texto #####
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

###### 3.4.4. Fuente ######
df_fuentes<-as.data.frame(font.files()) #Vemos que fuentes tenemos en el sistema

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

###### 3.4.5. Colores ######
# Dataframe que vamos a usar
data <- df_airbnb2_barrios %>%
  select(price, number_of_reviews_ltm, room_type) %>% #seleccionamos las columnas
  rename(reviews = number_of_reviews_ltm) %>% #Renombramos la columna para hacerla más fácil
  filter(price < 10000) %>% #sacamos los precios muy altos
  filter(price > 1000)

# Colores automáticos
colores_auto <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = room_type, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  theme_minimal()

colores_auto

# Colores manuales
# Creo una paleta de colores
mi_paleta <- c("#F8766D", "#7CAE00", "#00BFC4", "black", "orange")



colores_custom <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = room_type, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  scale_color_manual(values = mi_paleta) +
  theme_minimal()
colores_custom

#Gradientes automáticos
gradiente_auto <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = price, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  scale_color_gradient() +
  theme_minimal()
gradiente_auto

#Gradientes custom
gradiente_custom <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = price, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  scale_color_gradient(low = "red", # Color para el valor mínimo de "price"
                       high = "blue") + # Color para el valor máximo de "price"
  #existe una función "scale_color_gradient2" que permite poner un valor medio
  theme_minimal()
gradiente_custom

#Gradientes de color predefinida de ggplot
gradiente_pred <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = price, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  scale_color_viridis_c() + # "_c" cuando es continuo, "_d" cuando es discreta
  theme_minimal()
gradiente_pred

#Paletas locas
install.packages("wesanderson")
library(wesanderson)


gradiente_wesanderson <- data %>%
  ggplot(aes(x = price,
             y = reviews)) +
  geom_point(aes(color = room_type), #Color definido por una columna
             size = 2) + #Tamaño
  scale_color_manual(values = wes_palette("GrandBudapest1") ) + # Especificar la paleta personalizada
  theme_minimal()
gradiente_wesanderson

#### 4. Guardemos nuestros plots ####

dir.create("./plots") #Creamos una carpeta dentro de nuestro directorio de proyecto

periodo <- "Mayo"

dir.create(paste0("./plots/", periodo))

ggsave(plot = plot_1_deptos_por_comuna, 
       file ="plot_1_deptos_por_comuna.png", 
       device = "png", 
       path = paste0("./plots/", periodo), 
       width = 10, height = 15)

#### 5 Plotly ####
ggplotly(plot_1_deptos_por_comuna)

# 8 Desafíos ####



