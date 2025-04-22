# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("skimr")

library(tidyverse)
library(lubridate)
library(skimr)

# 0 - Seteamos el entorno de trabajo ####
options(scipen = 999) # para eliminar notación científica de números
getwd() # chequeamos el directorio donde estamos trabajando

# 1 - Levantamos la df_delitos de datos ####

## 1.A podemos llamar directamente desde la web (no siempre se puede)

df_delitos  <-  read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2023.csv",
                          encoding = "UTF-8")

## 1.B podemos cargar desde la compu local

df_delitos  <-  read.csv("./data/delitos_2023.csv",
                       sep = ";",
                       encoding = "UTF-8")

# 2 - Exploramos un poco el dataframe ####

head(df_delitos)
tail(df_delitos, n = 10)

glimpse(df_delitos) # para ver variables y tipo de datos de cada una

explore <- skim(df_delitos) # para ver resumidamente datos faltantes, distribuciones, etc de cada variable

unique(df_delitos$franja)# me fijo los valores únicos de la variable 'franja' porque nosé a qué se refiere
unique(df_delitos$tipo)
unique(df_delitos$comuna)

# 3 - Limpiamos ####

df_delitos2  <-  df_delitos %>% # creamos un nuevo objeto por si tenemos que corregir y necesitamos volver a 0
  mutate(tipo_delito = as.factor(tipo),
         fecha = ymd(fecha),
         horario = as.numeric(franja),
         comuna = case_when(
           comuna == "CC-08" ~ "8",
           comuna == "CC-09" ~ "9",
           comuna == "CC-01 NORTE" ~ "1",
           comuna == "CC-01 SUR" ~ "1",
           comuna == "CC-04" ~ "4",
           comuna == "CC-07" ~ "7",
           comuna == "CC-15" ~ "15",
           comuna == "CC-02" ~ "2",
           comuna == "CC-12" ~ "12",
           comuna == "CC-10" ~ "10",
           comuna == "CC-06" ~ "6",
           comuna == "CC-13" ~ "13",
           comuna == "CC-05" ~ "5",
           comuna == "CC-03" ~ "3",
           comuna == "CC-14" ~ "14",
           comuna == "CC-11" ~ "11",
           TRUE ~ comuna)) %>%
  mutate(comuna = as.numeric(comuna)) %>%
  select(-c(tipo, franja)) # seleccionamos todas las columnas excepto las que acabamos de transformar

unique(df_delitos2$comuna)


df_delitos2  <-  df_delitos2 %>% #podemos trabajar un poco más en las variables lat y long
  mutate(
    latitud = str_remove_all(latitud, "\\."),
    latitud = str_squish(latitud),
    latitud = str_replace(latitud, "(^-?\\d{2})", "\\1."),
    latitud = as.numeric(latitud),
    longitud = as.numeric(str_replace_all(str_squish(str_remove_all(longitud, "\\.")),"(^-?\\d{2})", "\\1."))
    )

# 4 - Estructura de un gráfico ####

# Los gráficos en R son como las cebollas: TIENEN CAPAS

mi_grafico  <-  ggplot(df_delitos, # llamamos a ggplot y definimos el df que vamos a graficar
                     aes(x=variable_independiente,
                         y=variable_dependiente))+ # definimos los datos que van a ir en cada eje
  geom_point()+ # definimos la geometría del grafico
  geom_line()+ # podemos agregar o combinar geometrias
  scale_y_continuous()+ # definimos las escalas de los ejes
  scale_colour_brewer()+ # definimos colores para las variables
  theme()+ # definimos elementos esteticos del grafico (fondo, bordes, tamano de letras,etc)
  facet_grid()+# podemos definir facetas (dividir en sub-graficos segun alguna variable)
  coord_flip()+# podemos definir el sistema de coordenadas
  labs() # definimos titulos o nombre de los ejes

## 4.1 barplot - Gráfico de barras ####

# Se usan para representar distribuciones de variables categóricas o comparaciones de cantidades entre categorías. 
# Pueden mostrar frecuencias (conteos) o valores agregados (como sumas o promedios) usando el parámetro `stat`

df1_delitos_comuna <- df_delitos2 %>%
  group_by(comuna, tipo_delito) %>%
  summarise(cantidad=n())%>%
  drop_na()

p1_delito_comuna_a <- ggplot(df1_delitos_comuna, 
                           aes(x = comuna, 
                               y = cantidad, 
                               fill = tipo_delito))+
  geom_bar(stat="identity", 
           position = "dodge")
p1_delito_comuna_a

# mejoremos un poco el gráfico 

p1_delito_comuna_b <- ggplot(df1_delitos_comuna,
                           aes(x = reorder(comuna,cantidad),
                               y = cantidad, 
                               fill = tipo_delito))+
  geom_bar(stat = "identity", position = "stack")+
  # geom_bar(stat="identity", position = "fill")+
  # scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y = element_text(size = 15))+
  labs(x = "Comuna", 
       y = "Cantidad",
       fill = "Tipo de delito",
       title = "Cantidad de delitos por comuna",
       caption = "Fuente:datos abiertos GCBA")
p1_delito_comuna_b

# podemos presentar un poco mejor la información?

install.packages("tidytext")
library(tidytext)

p1_delito_comuna_c<- ggplot(df1_delitos_comuna,
                            aes(x = reorder_within(comuna, cantidad, tipo_delito),
                                y = cantidad, 
                                fill = tipo_delito))+
  geom_bar(stat="identity", position = "stack")+
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~tipo_delito, scales = "free") +  
  theme_minimal()+
  theme(axis.text.y = element_text(size = 15))+
  labs(x = "Comuna", 
       y = "Cantidad",
       fill = "Tipo de delito",
       title = "Cantidad de delitos por comuna",
       caption = "Fuente:datos abiertos GCBA")
p1_delito_comuna_c

# más info: https://juliasilge.com/blog/reorder-within/ 

## 4.2 linebar - Gráfico de líneas ####

# Se usan cuando se quieren mostrar tendencias a lo largo del tiempo o el comportamiento de una variable continua ordenada. 
# Es ideal para series temporales, especialmente cuando los datos están agrupados por fecha.

df2_delitos_delitos_fecha <- df_delitos2 %>% 
  group_by(fecha, tipo_delito) %>% 
  summarise(cantidad=n())

p2_delitos_fecha <- ggplot(df2_delitos_delitos_fecha, 
                             aes(x = fecha, 
                                 y = cantidad, 
                                 color = tipo_delito))+
  geom_line(size = 1, alpha = 0.8)
  # geom_smooth()
p2_delitos_fecha

## 4.3 scatterplot - Gráfico de puntos ####

# Se utilizan para hacer gráficos de dispersión, ideales cuando se quiere ver la relación entre dos variables numéricas. 
# `geom_jitter()` es útil cuando los puntos se superponen mucho, ya que agrega un pequeño ruido para evitar solapamientos.

df3_barrios <- df_delitos2%>%
  filter(longitud < -58 & longitud > -59) %>% # filtramos porque CABA está en este rango de longitud
  drop_na()%>%
  mutate(
    lat = ifelse(latitud < -35 | latitud > -34.0, longitud, latitud),
    lng = ifelse(latitud < -35 | latitud > -34.0, latitud, longitud)
  )

barrio_particular <- "PALERMO"

p3_delitos_un_barrio <- ggplot(df3_barrios%>%
                                    filter(barrio == barrio_particular), 
                            aes(x=lng, 
                                y=lat, 
                                color=tipo_delito))+
  geom_point()+
  coord_sf() # nos da coordenadas geográficas
p3_delitos_un_barrio


p3_delitos_todos_barrios <- ggplot(df3_barrios %>% 
                            filter(barrio %in% c("PALERMO", "FLORES", "PATERNAL", "LINIERS")),
                          aes(x = lng, 
                              y = lat, 
                              color = tipo_delito))+
  geom_jitter() +
  coord_sf()
p3_delitos_todos_barrios


df_comisarias <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv",
                          encoding = "UTF-8")

df_comisarias2 <- df_comisarias%>%
  group_by(comuna)%>%
  summarise(cantidad_comisarias = n())


df3_comisarias<-df1_delitos_comuna%>%
  left_join(df_comisarias2)

p3_delitos_comisarias <- ggplot(df3_comisarias%>%
                                  mutate(comuna = as.factor(comuna)), 
                                aes(x = cantidad_comisarias,
                                    y = cantidad, 
                                    color = comuna))+
  geom_jitter(size = 2)+
  facet_wrap(~tipo_delito, scales = "free_y")+
  theme_minimal()
p3_delitos_comisarias

## 4.4 heat map - Mapa de calor ####

# Son útiles para representar matrices de valores, con el color en cada celda se puede indicar la magnitud de una variable. 
# Se aplica frecuentemente a combinaciones de variables categóricas o discretizadas (por ejemplo, día y hora).

df4_dias_horarios <- df_delitos2 %>% 
  mutate(dia = wday(fecha, label = T)) %>% 
  group_by(dia, horario) %>% 
  summarise(cantidad=n())

p4_dias_horarios <- ggplot(df4_dias_horarios,
                           aes(x = dia, 
                               y = horario, 
                               fill = cantidad))+
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  labs(title="Cantidad de delitos por día y hora")
p4_dias_horarios

# mejoremos el gráfico

p4_dias_horarios <- ggplot(df4_dias_horarios,
                           aes(x = dia, 
                               y = horario, 
                               fill = cantidad))+
  geom_tile(color = "white", 
            lwd = 1.5,
            linetype = 1)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  theme_minimal()+
  labs(title="Cantidad de delitos por día y hora")
p4_dias_horarios

# 5 combinaciones y chiches ####

## 5.1 vamos a combinar geometrias: lineas y puntos ####
df5_delitos_por_fecha <- df_delitos2 %>% 
  mutate(mes = round_date(fecha, unit = "month")) %>%  #redondeamos por mes
  group_by(mes, tipo_delito) %>%  #agrupamos por mes
  summarise(cantidad=n())

p5_delitos_mes <- ggplot(df5_delitos_por_fecha, 
                         aes(x = mes,
                             y = cantidad,
                             color = tipo_delito))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  scale_y_log10()
p5_delitos_mes


## 5.2 vamos a combinar geometrias: heatmap y texto ####
p4_dias_horarios_b <- p4_dias_horarios+
  geom_text(aes(label = cantidad), 
            color = "black", 
            size = 3)+
  theme(legend.position = "none")
p4_dias_horarios_b 

## 5.3 vamos a formatear el eje X de un plot que tiene formato fecha ####
p2_delitos_fecha_b <- p2_delitos_fecha +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Evolución de delitos en el tiempo")
p2_delitos_fecha_b

# mas info: https://www.r-bloggers.com/2018/06/customizing-time-and-date-scales-in-ggplot2/

## 5.4 otros tipos de escalas de colores ####

p2_delitos_fecha_b <- p2_delitos_fecha_b +
  scale_color_brewer(palette = "Dark2") # usamos scale_color_brewer si lo que queremos colorear es el parámetro color
p2_delitos_fecha_b


colores_custom <- c(
  "Amenazas"   = "#1f77b4",  # azul fuerte
  "Homicidios" = "#d62728",  # rojo fuerte
  "Hurto"      = "#2ca02c",  # verde
  "Lesiones"   = "#ff7f0e",  # naranja
  "Robo"       = "purple",  # violeta
  "Vialidad"   = "#8c564b"   # marrón
)

p1_delito_comuna_d <- p1_delito_comuna_c+
  scale_fill_manual(values = colores_custom) # usamos scale_fill_manual si lo que queremos colorear es el parámetro fill
p1_delito_comuna_d


# 6 guardar gráficos en png ####

dir.create("plots")
path_plots <- ("./plots")

ggsave(filename = "1 Cantidad de delitos por comuna.png",
       plot = p1_delito_comuna_d,
       device = "png",
       path = path_plots,
       width = 20,
       height = 15,
       units = "cm")

# 7 plotly #### 
install.packages("plotly")
library(plotly)

ggplotly(p3_delitos_comisarias, tooltip = c("comuna", "cantidad_comisarias"))

