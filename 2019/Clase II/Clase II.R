#Clase II. Repaso de tidyverse con datos electorales de GCBA y Nación.

#Instalamos los paquetes necesarios para trabajar, quizas ya los tenemos no es grave.
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

#Una vez que los tenemos instalados tenemos que "activarlos" para nuestro proyecto
library(tidyverse)
library(ggplot2)
library(dplyr)

#Ahora cargamos los datos que bajamos de data.buenosaires.gob.ar y pusimos en la carpeta del proyecto
data <- read.csv("resultados-elecciones-legisladores-generales-2017.csv", encoding = "UTF-8", sep=",")
head(data)
colnames(data)
View()
#En segundo lugar vamos a hacer algunas estadísticas básicas
mean(data$total_votos)
sum(data$total_votos)
median(data$total_votos)
max(data$total_votos)
min(data$total_votos)

#Podemos hacer lo mismo usando summarice de tidyverse
data_stats <- data %>%
  summarise(sum(data$total_votos))

data_stats

#Tambien podemos usar la función grup_by para poder ver cuantos votos saco cada partido
data_groups <- data %>%
  group_by(partido_descripcion) %>%
  summarise(sum(total_votos)) %>%
  rename(votos = "sum(total_votos)") 

View(data_groups)

#Lo mismo que antes pero sin el rename
data_groups <- data %>%
  group_by(partido_descripcion) %>%
  summarise(votos = sum(total_votos))

View(data_groups)

#... o por sección electoral
data_groups2 <- data %>%
  group_by(seccion_codigo) %>%
  summarise(sum(total_votos)) 
colnames(data_groups2)

#Ahora vamos a intentar hacer una visualización básica de estos números
data_viz_1 <- data_groups %>%
  ggplot(aes(x=partido_descripcion,y=votos)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
data_viz_1

#un gráfico de dispersion de las mesas con sus resultados
data_viz_2 <- data %>%
  subset( partido_descripcion=="ALIANZA VAMOS JUNTOS" | partido_descripcion=="ALIANZA UNIDAD PORTEÑA")%>%
  filter(mesa_numero<300)%>%
  ggplot(aes(x=mesa_numero,y=total_votos, col=partido_descripcion)) + 
  geom_jitter() + 
  geom_smooth(aes(col=partido_descripcion), method="loess", se=F)

data_viz_2

#otra opción muy útil es analizar cuales son las mesas más desviadas
#1. Vemos cual es la media, desviadas por circuito electoral.
media_circuitos <- data %>%
  filter(partido_descripcion=="ALIANZA UNIDAD PORTEÑA") %>%
  group_by(circuito_codigo) %>%
  summarize(mediana=mean(total_votos), sd=sd(total_votos), desvio=mean(total_votos)-sd(total_votos)*2)
View(media_circuitos)

#2. ahora vamos a hacer una tabla solo con las mesas y resultados de UC
mesas_uc <- data %>%
  filter((partido_descripcion=="ALIANZA UNIDAD PORTEÑA"))
head(mesas_uc)
#3. ahora juntamos eambas tablas
data_desvios <- inner_join(mesas_uc, media_circuitos,by="circuito_codigo")
View(data_desvios)
#4. ahora vemos cuales son las que están muy desviadas
uc_desviadas <- data_desvios %>%
  mutate(desviadas = total_votos - desvio) %>%
  filter(desviadas < 0)
View(uc_desviadas)


#FUNCIONES
#read.csv: como su nombre indica nos sirve para leer csv. Podemos definir separadores y tambien el enconding
#head: nos muestra las primeras 6 filas del dataframe
#colnames: nos muestra los nombres de las columnas en un dataframe.
#mean(): es la media o el promedio.
#dataframe$columna: significa que vamos a operar solo sobre una columna