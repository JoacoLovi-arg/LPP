#### Librerías ####

library(tidyverse) #Nos permite trabajar con tablas (operaciones tipo excel)
library(lubridate) #Nos permite formatear las fechas
library(readxl)
library(tidytext)
library(skimr) #Una librería para explorar los datos (ver cuantos faltan en cada columna etc.)
library(janitor) #Cumple varias funciones de limpieza de datos
library(sysfonts) #Para usar fuentes que tenemos en el sistema
library(showtext) #Para aplicar texto
library(plotly) #Otra librería de gráficos (se usa mucho en python)


# 0 seteo ####
options(scipen = 999) #Seteamos para que los números no nos aparezcan en formato científico

# 1. Datos ####
## 1.1. levanto data de establecimientos educativos en CABA #####

# internet
df_establecimientos <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/establecimientos-educativos/establecimientos_educativos_WGS84.csv",
                 encoding = "UTF-8")

# local
# list.files()
# df <- read_xlsx("establecimientos_educativos_WGS84.xlsx", sheet = 1)%>%
#   select(1:29)%>%
#   mutate(across(where(is.character), ~ iconv(., from = "UTF-8", to = "UTF-8")))

df_inscripciones<-read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/inscripcion-escolar/inscripciones-escolares-2017.csv",
                           sep = ";",
                           encoding = "Latin1")

# local
# list.files()
# df_inscripciones<-read.csv("inscripciones-escolares-2017.csv", sep = ";")


## 1.2. Exploración #####

glimpse(df_establecimientos) #Listamos las columnas y los tipos de datos

unique(df_establecimientos$tipest) #Unique para ver que valores tiene una columna
unique(df_establecimientos$depfun)

explore <- skim(df_establecimientos) #puede tardar un poquito
explore

rm(explore) #la función rm elimina un objeto de la memoria

## 1.3. Limpieza de los datos #####
unique(df_establecimientos$nivel)

n<-max(str_count(df_establecimientos$nivel, "-"))+1

df_establecimientos2<-df_establecimientos%>%
  separate(col = nivel, sep = " - ", into = paste0("var", 1:n))%>%
  pivot_longer(cols = paste0("var", 1:n), values_to = "nivel")%>%
  filter(!is.na(nivel))%>%
  mutate(
    nivel_agrupado = case_when(
      str_detect(nivel,"Primari") ~ "Primario",
      str_detect(nivel,"Secundari") ~ "Secundario",
      str_detect(nivel,"Superior") ~ "Terciario",
      str_detect(nivel,"Inicial") ~ "Inicial",
      TRUE ~ "Otros"
    )
  )

unique(df_establecimientos2$nivel_agrupado)


df_inscripciones2 <- df_inscripciones%>%
  group_by(CUE_ANEXO, NIVEL, GRADO)%>%
  summarise(cantidad = n())

# 2. GRÁFICOS ####
## 2.1. Estructura de un gráfico #####

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

## 2.2. Formas de escribir un gráfico ####

### 2.2.1. Un gráfico simple: deptos por barrio #####

establecimientos_por_barrio <- df_establecimientos %>%
  group_by(barrio) %>% # Agrupamos los datos por barrio
  summarise(cantidad = n())

plot_datos_x_barrio <- establecimientos_por_barrio %>%
  ggplot(aes( #parámetros "aesthetics" de ggplot
    x = barrio, #Variable independiente
    y = cantidad)) + #Variable dependiente
  geom_col() #Tipo de gráfico
  
plot_datos_x_barrio #Pero no se entiende nada


### 2.2.2.Un gráfico un poco más tunneado ####
df_1_comunas <- df_establecimientos2%>%
  group_by(comuna, nivel_agrupado)%>%
  summarise(cantidad = n())
df_1_comunas

p1_est_por_comuna_a <- ggplot(df_1_comunas,
                                aes(x = reorder(comuna, cantidad), #le decimos que reordene una columna según los valores de otra 
                                    y = cantidad,
                                    fill = nivel_agrupado)) + #relleno de acuerdo a una columna
  geom_bar(stat = "identity", 
           position = "stack") + #stak apiladas
  coord_flip() + #cambiamos coordenadas (la x en vertical por ejemplo)
  labs(title = "Oferta por tipo y comuna") #le ponemos un titul
p1_est_por_comuna_a

### 2.2.3. Todo junto procesamiento de los datos + gráfico #####

p1_est_por_comuna_b <- df_establecimientos2%>%
  group_by(comuna, nivel_agrupado)%>%
  summarise(cantidad = n())%>%
  ggplot(aes(reorder(comuna, cantidad),  
             cantidad, 
             fill=nivel_agrupado))+
  geom_bar(stat = "identity",
           position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
p1_est_por_comuna_b

### 2.2.4. Armamos el df directo dentro del plot #####

p1_est_por_comuna_c <- ggplot(df_establecimientos2%>%
                                  group_by(comuna, nivel_agrupado)%>%
                                  summarise(cantidad = n()),
                                aes(reorder(comuna, cantidad), 
                                    cantidad, fill = nivel_agrupado))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
p1_est_por_comuna_c

### 2.2.5. Armamos el df dentro de la geometria (esta es la opcion mas "rara") ####

p1_est_por_comuna_d <- ggplot()+
  geom_bar(data = df_establecimientos2, 
           mapping = aes(comuna, 
                         fill = nivel_agrupado), 
           stat = "count",
           position = "stack")+
  coord_flip()+
  labs(title = "Oferta por tipo y comuna")
p1_est_por_comuna_d

# 3. A graficar! ####

## 3.1 barplot - Gráfico de barras #####
df_1_comunas

### 3.1.1. creo un lienzo ######
p1_est_por_comuna <- ggplot()

### 3.1.2. agrego ejes ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(x = comuna, y = cantidad))

### 3.1.3. agrego geometría ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(comuna, cantidad))+
  geom_bar(stat = "identity")

### 3.1.4. agrego una variable ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(comuna, cantidad, fill = nivel_agrupado))+
  geom_bar(stat = "identity")

### 3.1.5. reordeno ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(reorder(comuna, cantidad),
                                cantidad, 
                                fill = nivel_agrupado))+
  geom_bar(stat = "identity")

### 3.1.6. roto los ejes de coordenadas ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(reorder(comuna, cantidad),
                                cantidad, 
                                fill = nivel_agrupado))+
  geom_bar(stat = "identity")+
  coord_flip()

### 3.1.7. defino el tema general del plot ######
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(reorder(comuna, cantidad),
                                cantidad, 
                                fill = nivel_agrupado))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()

### 3.1.8. acomodo la referencia de colores y agrego títulos ####
p1_est_por_comuna <- ggplot(df_1_comunas,
                            aes(reorder(comuna, cantidad),
                                cantidad, 
                                fill = nivel_agrupado))+
  geom_bar(stat = "identity")+
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
p1_est_por_comuna

## 3.2. scatterplot - Gráfico de puntos (2 variables numéricas) #####
df_establecimientos3<-df_establecimientos%>%
  # distinct(cueanexo, comuna, barrio)%>%
  inner_join(df_inscripciones2, by = c("cueanexo" = "CUE_ANEXO")) #hago inner para evitar valores perdidos

df_2_inscripciones<-df_establecimientos3%>%
  # mutate(publico = ifelse())
  group_by(barrio, NIVEL)%>%
  summarise(
    establecimientos_publicos = n(),
    n = sum(cantidad, na.rm = T))

p2_insc_est <- ggplot(df_2_inscripciones,
                        aes(x = establecimientos_publicos, 
                            y = n, 
                            color = NIVEL))+
  geom_point(
    # size = 0.5,
    alpha = 0.6
  )+
  theme_minimal()+
  labs(x= "", 
       y= "", 
       title= "")
p2_insc_est 

## 3.3. linebar - Gráfico de líneas #####
niveles_ordenados <- c("LACTARIO", "2 AÑOS", "3 AÑOS", "4 AÑOS", "5 AÑOS",
                       "1 GRADO", "2 GRADO", "3 GRADO", "4 GRADO", "5 GRADO", "6 GRADO", "7 GRADO",
                       "1 AÑO", "2 AÑO", "3 AÑO", "4 AÑO", "5 AÑO","6 AÑO")

df_inscripciones<-df_inscripciones%>%
  mutate(edad = factor(GRADO, levels=niveles_ordenados, ordered = TRUE))

df_3_insc_edad<-df_inscripciones%>%
  group_by(edad, COMUNA)%>%
  summarise(n = n())

### 3.4 Heat Map - Mapa de calor ###### 

p4_insc_comuna <- ggplot(df_3_insc_edad,
                         aes(COMUNA, edad, fill = n))+
  geom_tile(color = "white", 
            lwd = 1.5,
            linetype = 1)+
  # geom_text(aes(label = str_c("$",round(precio, digits = 0))), color= "black", size = 3)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)
p4_insc_comuna

## 3.4. Otras combinaciones y chiches ####

### 3.4.1. Scales ###### 
p2_insc_estb <- p2_insc_est+
  # scale_y_sqrt()
  # scale_y_log10()
  scale_y_continuous(trans = "log2")
p2_insc_estb

### 3.4.2. Facets #####
p3_insc_edad_b <- p3_insc_edad+
  facet_wrap(~COMUNA, scales = "free")+#la virgulilla se puede escribir con alt+126
  theme(legend.position = "none")
p3_insc_edad_b

### 3.4.3. Texto #####
df_1_comunas <- df_1_comunas%>%
  group_by(comuna)%>%
  mutate(
    total = sum(cantidad),
    texto = paste("Total:\n", sum(cantidad)))%>%
  ungroup()

p1_est_por_comuna_totales<-ggplot(df_1_comunas,
                                  aes(reorder(comuna, cantidad),
                                      cantidad, 
                                      fill = nivel_agrupado))+
  geom_bar(stat = "identity")+
  geom_text(aes(reorder(comuna, total), 
                total, 
                label = texto,
                col = ifelse(total>200, "red", "black")),
            vjust = 0.5, 
            hjust = -0.3,
            size = 3,
            # col = ifelse(total>200, "red", "black"),
            fontface = "bold")+
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
p1_est_por_comuna_totales

### 3.4.4. Fuente ######
df_fuentes<-as.data.frame(font.files()) #Vemos que fuentes tenemos en el sistema

font.add(family = "Freestyle Script", regular = "FREESCPT.TTF")
fuente<-"Freestyle Script"
showtext.auto()

p4_insc_comuna_b<-p4_insc_comuna+
  theme(text = element_text(family = fuente))+
  labs(title = "un titulo")
p4_insc_comuna_b


font_add_google(name = "Encode Sans")
fuente_google<-"Encode Sans"
showtext::showtext_auto()

p4_insc_comuna_c<-p4_insc_comuna+
  theme(text = element_text(family = fuente_google))+
  labs(title = "un titulo")
p4_insc_comuna_c

### 3.4.5. Colores ######
# Dataframe que vamos a usar
# df_5_inscripciones<-df_establecimientos3%>%
#   # mutate(publico = ifelse())
#   group_by(barrio, MODALIDAD)%>%
#   summarise(
#     establecimientos_publicos = n(),
#     n = sum(cantidad, na.rm = T))

# Colores automáticos
colores_auto <- df_2_inscripciones %>%
  ggplot(aes(x = establecimientos_publicos,
             y = n)) +
  geom_point(aes(color = NIVEL, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  theme_minimal()
colores_auto

# Colores manuales
# Creo una paleta de colores
mi_paleta <- c("#00BFC4", "black", "orange")

colores_custom <- df_2_inscripciones %>%
  ggplot(aes(x = establecimientos_publicos,
             y = n)) +
  geom_point(aes(color = NIVEL, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 1) + #Tamaño
  scale_color_manual(values = mi_paleta) +
  theme_minimal()
colores_custom

#Gradientes automáticos
gradiente_auto <- df_2_inscripciones %>%
  ggplot(aes(x = establecimientos_publicos,
             y = n)) +
  geom_point(aes(color = n, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 2) + #Tamaño
  scale_color_gradient() +
  theme_minimal()
gradiente_auto

#Gradientes custom
gradiente_custom <- df_2_inscripciones %>%
  ggplot(aes(x = establecimientos_publicos,
             y = n)) +
  geom_point(aes(color = n, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 2) + #Tamaño
  scale_color_gradient(low = "red", # Color para el valor mínimo de "price"
                       high = "blue") + # Color para el valor máximo de "price"
  #existe una función "scale_color_gradient2" que permite poner un valor medio
  theme_minimal()
gradiente_custom

#Gradientes de color predefinida de ggplot
gradiente_pred <- df_2_inscripciones %>%
  ggplot(aes(x = establecimientos_publicos,
             y = n)) +
  geom_point(aes(color = n, #Color definido por una columna
                 alpha = 0.1), #Transparencia
             size = 2) + #Tamaño
  scale_color_viridis_c() + # "_c" cuando es continuo, "_d" cuando es discreta
  theme_minimal()
gradiente_pred

#Paletas locas
install.packages("wesanderson")
library(wesanderson)
# https://github.com/karthik/wesanderson

gradiente_wesanderson <- df_1_comunas%>% 
  ggplot(aes(x = comuna,
             y = cantidad,
             fill = nivel_agrupado)) +
  geom_bar(stat = "identity", position = "fill")+
  scale_fill_manual(values = wes_palette("Cavalcanti1") ) + # Especificar la paleta personalizada
  theme_minimal()
gradiente_wesanderson

# 4. Guardemos nuestros plots ####

dir.create("./plots") #Creamos una carpeta dentro de nuestro directorio de proyecto

periodo <- "Septiembre"

dir.create(paste0("./plots/", periodo))

ggsave(plot = p1_est_por_comuna, 
       file ="p1_est_por_comuna.png", 
       device = "png", 
       path = paste0("./plots/", periodo), 
       width = 10, height = 15)

# 5 Plotly ####
ggplotly(p1_est_por_comuna)


