#### Laboratorio de Políticas Públicas: Clase 3 "Introducción a R" ####

##### Operaciones básicas #####
# R base puede ser usado como una simple calculadora.

2+3

17 + (5*5)

##### Variables #####
#Tambien podemos nombrar diversos objetos para trabajar con Variables

dos <- 2 #(con el símbolo "<-" asignamos un valor a la variable)
tres <- 3
resultado <- dos + tres

resultado #"Corriendo" la variable obtenemos el resultado en la consola

#*Atención: las variables pueden tener cualquier nombre. No pueden tener espacios

#Las variables pueden ser de texto
nombres <- "Karl"
apellido <- "Marx"

#*Atención: los textos deben estar encomillados "", si no R los considerará una nueva variable


##### Funciones #####

#Una función es un conjunto de instrucciones que convierten las entradas (inputs) en resultados (outputs).
#Funciones básicas incluidas en R base

###### Listas ######
#Con la letra c() podemos hacer una lista de palabras o números o cualquier otro objeto
lista_de_numeros <- c(1,4,7,8)

lista_de_palabras <- c("hola", "como", "estas")

###### Paste ######
#Similar a concatenar en excel y otros lenguajes. Genera un string de texto a partir de dos o más strings

nombre_apellido <- paste(nombres, apellido)
nombre_apellido

#Atención: usando la funcion "paste0", no nos dejará un espacio entre las palabras.

###### Matemáticas ######

mean(lista_de_numeros) #Media
sum(lista_de_numeros) #suma
median(lista_de_numeros) #Mediana

##### Descargar y leer Tablas de datos #####

# Podemos crear una variable que sea un link a los datos si estos están online #
link <- "https://datos.hcdn.gob.ar/dataset/2e08ab84-09f4-4aac-86b3-9573ca9810db/resource/59c05ba8-ad0a-4d55-803d-20e3fe464d0b/download/actas-cabecera-137-2.0.csv"

# Con la funcion básica "read.csv" podemos leer un archivo csv (comma separated values)
df <- read.csv(link)

#creamos una variable con todos los datos del csv en una tabla

###### Explorar los datos ######
#Existen diversas formas de visualizar los datos en el csv que cargamos

#Pare ver en una tabla tipo "excel" usando el comando "View()" 
View(df)

# head() an tail() para ver los primeros o últimos 6 registros de la tabla
head(df)
tail(df)

# Print() para ver toda la tabla en la consola (rstudio limita la cantidad por cuestiones de performance)
print(df)

# colnames() para enumerar todas los nombres de las columnas
colnames(df)

# summary() para obtener una descripción del dataset
summary(df)

##### Paquetes (o librerías) #####

#Un paquete es una colección de funciones, datos y documentación que extienden 
#la funcionalidad básica de R.

# Primero debemos instalar un paquete para luego utiizarlo. Se instala una sola vez

install.packages("dplyr") #Install packages es la función para instalar un paquete

# Luego debemos activar el paquete para utilizarlo. Esto debemos hacerlo cada vez que querramos utilizarlo

library(dplyr)

##### Operaciones sobre la base de datos #####

df2 <- df %>% # A partir de este momento vamos a trabajar siempre sobre el objeto DF
  #el operador "%>%" nos permite encadenar varias operaciones seguidas
  ###### Función select() ######
  #seleccionar columnas con las que queremos trabajar
  select(votos_afirmativos,
         votos_negativos, 
         abstenciones, 
         resultado, 
         base_mayoria,
         tipo_periodo) %>%
  #También podemos de-seleccionar columnas
  select(-abstenciones) %>%
  ###### Función filter() ######
  #seleccionar filas (filtrar)
  filter(votos_afirmativos > 100) %>% #filtro una columna numérica
  filter(tipo_periodo == "Ordinario") %>% #filtro una columna de texto
  ###### Funcion mutate() ######
  mutate(id_periodo = paste(tipo_periodo, base_mayoria)) %>% #Operaciones sobre los datos de una fila
  ###### Función group_by() y summarise()
  #Permite agrupar por una variable y hacer calculos
  group_by(id_periodo) %>% #agrupamos por resultado
  summarise(suma_de_votos = sum(votos_afirmativos)) #Sumo los votos positivos obtenidos por resultado

View(df2)
  
  