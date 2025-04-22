# ANÁLISIS DE TEXTO #

## Instalamos los paquetes que seguramente no tengan instalados

install.packages("syuzhet")
install.packages("quanteda")
install.packages("rwhatsapp")
install.packages("tidytext")

install.packages("quanteda.textstats")
install.packages("quanteda.textplots")

## y ahora los cargamos. Todos al principio, despues verán

library(tidyverse) #Para manejar tablas y otras operaciones
library(quanteda) #Para crear corpus, bag of words y text mininig
library(rwhatsapp) #Para parsear los textos exportados de whatsapp
library(tidytext) #Para limpiar y formatear textos
library(lubridate) #Para formatear fechas
library(syuzhet) #Paquete que entre otras cosas permite hacer sentiment análisis en español
library(quanteda.textstats) #Para hacer algunos análisis y gráficos con R
library(quanteda.textplots) #Para graficar con R (por ejemplo nubes de palabras)

## Ahora van a buscar un grupo de whatsapp. Preferiblemente un grupo picante
## controvertido, con mucho dialogo desde hace varios años.
## lo van a descargar siguiendo este tutorial horrible: https://youtu.be/5pTLn0E_CRA
## si tienen android van a descargar un txt, con IoS un zip. 
## Envíenlo a su mail y descarguenló en la carpeta de R

## Carga de los datos
### Los datos de whatsapp vienen bien feos. Vamos a usar un paquete para parsearlos

chat <- rwa_read("chat.txt") %>% #Cargamos los datos de whatsapp con la función que parsea los mismos
  drop_na(author) #removemos los mensajes sin autor (los enviados automáticamente por la app)
colnames(chat)

## Preparación de los datos.
### aun parseandolos nos conviene editarlos un poco más
chat_clean <- chat %>%
  mutate(fecha = as.Date(time)) %>% #creamos una columna fecha (sin horas)
  mutate(hora = hour(time)) %>% #Creamos una columna hora
  mutate(mes = months(time)) %>% #creamos una columna mes
  mutate(dia = weekdays(time)) %>% #creamos una columna
  mutate(anio = year(time)) %>% #creamos na columna año
  mutate(text = str_to_lower(text)) %>% #ponemos toda la columna de texto en minusculas
  mutate(text = str_squish(text)) %>% #Limpiamos el texto, espacios duplicados y similares
  select(-source) %>%#eliminamos la columna "source" porque vamos a usar una sola fuente de datos
  drop_na(anio) %>% #eliminamos fechas sin datos
  mutate(text = gsub("@\\w+", "", text)) %>%
  filter(!text == str_detect(text, "imagen omitida")) %>% #sacamos mensaje automativo de wa
  mutate(hora = as.factor(hora)) #Pasamos las horas a factor para graficarlas

View(chat_clean)

## ANÁLISIS EXPLOTATORIO ##
### Ahora vamos a hacer un análisis por arriba. 
### ¿Como se comporta el grupo?

## Cuantos chats por día  hacemos en promedio
### ¿Quien tiene el grupo más intenso?
chat_mean_daily <- chat_clean %>%
  group_by(fecha) %>%
  summarise(n = n())

mensajes_por_dia <- mean(chat_mean_daily$n)
mensajes_por_dia
mensajes_por_hora <- mensajes_por_dia/24
mensajes_por_hora

### ¿Quien es el que chatéa más del grupo?
el_que_habla_mas <- chat_clean %>%
  group_by(author) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

el_que_habla_mas # AKA "el intense"

viz_nivel_loro<-ggplot(el_que_habla_mas, aes(reorder(author,n),n, fill=author))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=0.5, size=3)+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title="Nivel de loro")
viz_nivel_loro

## Veamos como evolucionó en el tiempo

viz_chat_day <- chat_clean %>%
  group_by(fecha) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = fecha, y = n)) +
  geom_line() +
  geom_smooth(method = "lm") + #Regresión lineal
  labs(title = "Evolución por día")+
  theme(title = element_text("Evolución diaria"))
viz_chat_day

## veamos que hora es la más activa del grupo

viz_chat_hour <- chat_clean %>%
  group_by(hora) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = hora, y = n)) +
  geom_col() +
  theme_minimal()+
  labs(title = "Conversación por hora")
viz_chat_hour

## veamos que integrante es un intenso que escribe a cualquier hora
### si no son muchos integrantes se pueden hacer lineas de colores sin facetado
viz_chat_hour_autor <- chat_clean %>%
  filter(!author == "LES TONTES (Kukas)") %>%
  group_by(hora, author) %>%
  summarise(n = n()) %>%
  mutate(hora = as.factor(hora)) %>%
  ggplot(aes(x = hora, y = n)) +
  facet_wrap(~author, scales = "free_y", ncol = 4) +
  geom_col(fill = "#f05454") +
  theme_minimal() +
  theme(axis.text.y = element_blank())+
  labs(title = "Horario de conversación por autor")
viz_chat_hour_autor

## Siempre hay un amigo que se cae. ¿Quien se cayó en la cuarentena?
viz_chat_date_autor <- chat_clean %>%
  group_by(fecha, author) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = fecha, y = n)) +
  facet_wrap(~author, scales = "free_y", ncol = 4) + #dejo libre la y
  geom_smooth(method = "lm", color = "#f05454", alpha = 0) +
  theme_minimal() +
  theme(axis.text.y = element_blank())+
  labs(title = "Tendencia de la conversación")
viz_chat_date_autor

## ¿Cambiaron los horarios de escribir con la cuarentena?
viz_chat_hour_year <- chat_clean %>%
  group_by(anio, hora) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = hora, y = n)) +
  facet_wrap(~anio, scales = "free_y", ncol = 1) +
  geom_col(fill = "#f05454") +
  theme_minimal()+
  labs(title = "Horario de covnersación por año")
viz_chat_hour_year

str(chat_clean$hora)

#Top emojis
top_emoji <- chat_clean %>%
  drop_na(emoji) %>% #Muchos chats no tienen emojis, los saco
  unnest(emoji) %>% #Muchos chats tienen varios emojis, los desagrupo
  group_by(emoji) %>%
  summarise(n = n()) %>%
  top_n(10) %>%
  arrange(desc(n))%>%
  ggplot(aes(x = reorder(emoji,n), y = n)) +
  geom_col()+
  labs(title = "Emojis más usado")+
  coord_flip()
top_emoji

## Como apuntamos
apuntando <- chat_clean %>%
  drop_na(emoji_name) %>%
  unnest(emoji) %>%
  filter(emoji_name %in% c("backhand index pointing up", 
                           "index pointing up")) %>%
  group_by(fecha,emoji) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = fecha, y = n, fill = emoji)) +
  geom_line() +
  theme_minimal()

apuntando


# ANALISIS DE TEXTO #
##Hasta ahora estuvimos viendo el comportamiento del grupo.
##¿Que pasa con el contenido?

##Construimos un corpus para procesarlo como texto
### Un corpus es una colección de textos preparada para ser analizada

corpus_chat <- corpus(chat_clean)


## DFM. Matriz de documentos
### es una matriz simple donde cada id es un documento (un chat) y 
### cada columna es una palabra. Las celdas indican cuanto aparecen

custom_stopwords <- c("imagen", "omitido", "omitida", "sticker",
                      "q", "si", "va", "hace", "gif", "ser", "acá", 
                      "creo" ,"|", "voy", "ahi", "ahi", "de", "dale",
                      "hoy", "hacer", "cada", "vos", "dia", "años",
                      "n°", "és", "lá", "pq", "multimedia", "ja", "jaja", "jajaja", "jajajaja")
### Con las stopwords eliminamos palabras comunes que dicen poco

### Tokenizamos el texto (en este caso nuestros tokens serán palabras)
tkn_chat <- tokens(corpus_chat, #tokenizo en palabras (hay otras formas)
                   remove_numbers = TRUE, #elimino números
                   remove_symbols = TRUE, #elimino símbolos
                   remove_punct = TRUE) #elimino signos de puntuación

tkn_chat_clean <-  tkn_chat %>%
  tokens_remove(pattern  = c(stopwords("spa"),custom_stopwords)) #remuevo las stopwords

View(tkn_chat_clean) #pueden ver que nos dió  una lista de palabras para cada texto

### Ahora creo la matriz de docuementos

dfm_chat <- dfm(tkn_chat_clean)

### y agrupo la matriz por author
dfm_chat_by_author <- dfm_group(dfm_chat, groups = author)

head(dfm_chat_by_author)

### y por anio
dfm_chat_by_year <- dfm_group(dfm_chat, groups = anio)

head(dfm_chat_by_year)

## cuales son las palabras más comunes en el grupo (sacando las stop words)

top_words <- textstat_frequency (dfm_chat, n = 20) %>%
  ggplot(aes(x = reorder(feature, docfreq), y = docfreq)) +
  geom_col()+
  coord_flip()+
  labs(title = "Palabras más usadas")
top_words


## Y la típica nube de palabras (WordCloud)
### Para ponerlo más lindo:https://quanteda.io/reference/textplot_wordcloud.html
### lamentablemente vamos a tener que hacer un nuevo dfm con otro grupo

nube <- dfm_chat %>%
  dfm_trim(min_termfreq = 200, verbose = FALSE) %>%
  textplot_wordcloud()

#Y si comparamos en una nube 2020 vs 2019?
nube_comparativa <- dfm_chat %>%
  dfm_group(groups = anio) %>% 
  dfm_trim(min_termfreq = 10) %>% #recorto y me quedo con las palabras más frecuentes
  textplot_wordcloud(comparison = TRUE, 
                     rotation = 0, 
                     labelsize = 1, 
                     fixed_aspect = TRUE)
nube_comparativa

##Distancia Keyness (las palabras más representativas de un discurso frente al resto)
### Que palabras representan mejor al 2020 
chat_keyness  <- textstat_keyness(dfm_chat_by_author,
                                  docvars(dfm_chat_by_author, 
                                          "author") == "Thomsis") #elegir el nombre de uno de los participantes del grupo

textplot_keyness(chat_keyness, color = c("red", "blue")) 

### Similitudes entre los autores del chat. Interesante para analizar similitud de los discursos
chat_sim <- textstat_simil(dfm_chat_by_author, 
                           method = "cosine", 
                           margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame() #Lo convertimos en un dataframe

###y lo visualizamos en un mapa de calor
heat_map_chat_author <- chat_sim %>%
  ggplot(aes(document1, document2, fill= cosine)) + 
  geom_tile()+
  labs(title = "Similitud de discursos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heat_map_chat_author

#Palabras en el discurso
###Hacemos un corpus agrupado por author (cada documento es todo lo que dijo un author)
corpus_chat_by_author <- corpus_group(corpus_chat, groups = author)

### Volvemos a tokenizar (ahora no es necesario sacar nada
corpus_chat_by_author_tkn <- tokens(corpus_chat_by_author)
###Analizamos las palabras en contexto (kwic key words in context)

textplot_xray(
  kwic(corpus_chat_by_author_tkn, pattern = "blanco"),
  kwic(corpus_chat_by_author_tkn, pattern = "negro")
)

# diversidad del lexico
chat_author_diversidad <- textstat_lexdiv(dfm_chat_by_author)

chat_author_diversidad


##Análisis de sentimientos utilizando "syuzhet"

### Sampleamos la muestra por año
### Sampleamos porque si no es mucho para procesar. Con tiempo y algunos trucos igual se puede
chat_sample_anio<- chat_clean %>% 
  group_by(anio) %>% 
  sample_n(1000)
View(chat_sample_anio)
colnames(chat_sample_anio)


#Calculamos los sentimientos para todos los chats sampleados
chat_sentiment_anio<-get_nrc_sentiment(chat_sample_anio$text,
                                            language = "spanish")

#y despues tenemos que juntar los resultados de sentiment con el datase original
chat_sentiment_anio_all <- as.data.frame(cbind(chat_sample_anio, chat_sentiment_anio))


#Visualizamos humor por año
humor_anio <- chat_sentiment_anio_all %>%
  select(anio, negative, positive) %>%
  group_by(anio ) %>%
  summarise(neg = mean(negative), pos = mean(positive)) %>% 
  mutate(humor = pos-neg) %>%
  ggplot(aes(y=humor, x = anio)) +
  geom_col()
humor_anio

#Sampleamos la muestra por usuario
chat_sample_author <- chat_clean %>%
  filter(anio > 2018) %>%
  group_by(author) %>%
  sample_n(43)
View(chat_sample_author)


#Calculamos los sentimientos por author
chat_sentiment_usuario <-get_nrc_sentiment(chat_sample_author$text,
                                           language = "spanish")

chat_sentiment_usuario_all <- as.data.frame(cbind(chat_sample_author,
                                                  chat_sentiment_usuario))

#vemos quien es el más tóxico
toxico <- chat_sentiment_usuario_all %>%
  group_by(author) %>%
  summarise(neg = mean(negative), pos = mean(positive))%>%
  ggplot(aes(y = author)) +
  geom_col(aes(x=neg*-1), fill = "blue") +
  geom_col(aes(x=pos), fill = "red")+
  labs(title = "Negatividad vs Positividad")
toxico

toxico <- chat_sentiment_usuario_all %>%
  group_by(author) %>%
  summarise(neg = mean(negative), pos = mean(positive)) %>%
  mutate(positividad = pos - neg) %>%
  arrange(desc(positividad)) %>%
  ggplot(aes(y = reorder(author,-positividad), x=positividad, fill = positividad)) +
  geom_bar(stat = "identity")+
  labs(title = "Nivel de toxicidad")
toxico

# BUENO AHORA A INVENTAR Y AVERIGUAR COSAS #

#cantidad de palabras por autor
chat_palabras<- chat_clean%>%
  unnest_tokens(input = "text", output = "word")%>%
  select(word, author)%>%
  filter(!word%in%c(stopwords(language = "es"),custom_stopwords))%>%
  count(author, word, sort = TRUE)

chat_cantidad_palabras<-chat_palabras%>%
  group_by(author)%>%
  summarise(cantidad=sum(n))
glimpse(chat_cantidad_palabras)

viz_palabras<-ggplot(chat_cantidad_palabras, aes(reorder(author, cantidad), cantidad,
                                                 fill=author))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Cantidad total de palabras")+
  theme(legend.position = "none")+
  geom_text(aes(label=cantidad), vjust=0.5, size=3)
viz_palabras


#nube de palabras
nube_grupo<-textplot_wordcloud(dfm_chat, 
                               min_count = 50, 
                               random_order = FALSE,
                               rotation = .25, 
                               color = RColorBrewer::brewer.pal(8,"Dark2"))


#diversidad de lexico
chat_diversidad <- chat_clean %>%
  unnest_tokens(input = "text", output = "word") %>%
  filter(!word %in% c(stopwords(language = "es"), custom_stopwords)) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))

ggplot(chat_diversidad, aes(x=reorder(author, lex_diversity), y= lex_diversity, fill=author)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Cantidad Palabras") +
  xlab("Miembros") +
  ggtitle("Lexical Diversity")+
  coord_flip()

#pitolover
chat_pitolover<-chat_clean%>%
  filter(str_detect(text,"pitolover"))

#palabras por usuario
palabras_mas_usadas<-chat_palabras%>%
  group_by(author)%>%
  filter(n>3)%>%
  top_n(n=5)

ggplot(palabras_mas_usadas, aes(x=reorder(word,n),y=n, fill=author))+
  geom_bar(stat="identity", show.legend = FALSE)+
  ylab("Cantidad")+
  xlab("Palabra")+
  coord_flip()+
  facet_wrap(~author, ncol = 2, scales="free_y")
