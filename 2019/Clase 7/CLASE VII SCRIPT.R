library(tidyverse)
library(quanteda)
library(readtext)
library(ggplot2)

#cargo el archivo csv del debate presidencial 2015
debate <- readtext("debate2015.csv", text_field = "text")
View(debate)
debate

#creo un corpus (colección de documentos de texto) para poder procesarlo con quanteda
corpus_debate <-corpus(debate)

#Rápidamente vemos quien dijo más palabras
grafico1 <- summary(corpus_debate) %>%
  ggplot(aes(x = persona, y = Tokens)) +
  geom_bar(stat="identity")
grafico1  

#Hacemos lo mismo pero con subset del corpus (como un filtro)

grafico2 <- summary(corpus_subset(corpus_debate, persona == c("Macri", "Scioli"))) %>%
  ggplot(aes(x = persona, y = Tokens)) +
  geom_bar(stat="identity")
grafico2  

#vamos a sacar a los periodistas para quedarnos con Macri y Scioli
corpus_debate <- corpus_subset(corpus_debate, persona == c("Macri", "Scioli"))

#KWIC
#Función interesante para explorar el corpus. kwic(Keywords in context)
kwic(corpus_debate, pattern = "miedo")

#TOKENIZE
#separar todo el documento por sus palabras
tokens <- tokens(corpus_debate, remove_numbers = TRUE,  remove_punct = TRUE)

#ngrams
toks_ngram <- tokens_ngrams(tokens, n = 2:4)
head(toks_ngram[[1]], 50)

#seleccionando ngrams
toks_ngrams_select <- tokens_select(toks_ngram, pattern = phrase('trabajo'))
head(toks_ngrams_select[[1]], 50)

#DFM
#document-feature matrix una tabla con los documentos como filas y las palabras como columnas. 
dfm_debate <- dfm(corpus_debate)
View(dfm_debate)

#Ahora vamos a hacer lo mismo pero usando dos funciones muy interesantes "stem" y "stopwords"
dfm_debate <- dfm(corpus_debate, remove = stopwords("spanish"), 
                            stem = FALSE, remove_punct = TRUE)
View(dfm_debate)

#ahora vamos a filtrar las palabras más usadas en el debate
topfeatures(dfm_debate, 20)

#y terminamos la exploración con una nube de palabras linda
nubedepalabras <- textplot_wordcloud(dfm_debate, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#OTRAS OPERACIONES
#Agrupando los documentos
dfm_agrupado <- dfm(corpus_debate, groups = "persona", 
                 remove = c(stopwords("spanish"),stopwords_debate), remove_punct = TRUE)
dfm_sort(dfm_agrupado)[, 1:10]

#Podemos crear tambien una lista custom de stopwords
stopwords_debate <- c("va", "sino", "va", "trata", "hace", "día", "hoy", "años",
                      "argentina", "aires", "buenos", "país", "llevar","hacerlo", 
                      "gran", "hace", "millones", "tema","hecho", "ahora", "vos",
                      "ciudad", "realmente", "si")

#Similaridades entre los textos (es más interesante con más gente en el debate)
tstat_debate <- textstat_simil(dfm_agrupado,margin = "documents", method = "cosine")
tstat_debate

#similitudes en los terminos
tstat_sim <- textstat_simil(dfm_agrupado, selection = c("educación", "trabajo", "ciencia"), 
                            method = "cosine", margin = "features")
lapply(as.list(tstat_sim), head, 10)
tstat_sim

#NUEVAS VISUALIZACIONES
#otra nube de palabras más experimental que compara discursos
grafico3 <- dfm_agrupado %>%
  dfm_trim(min_termfreq = 1, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)

#Frequency plots
frecuencia_debate <- textstat_frequency(dfm_agrupado, n = 20)

frecuencia_debate$feature <- with(frecuencia_debate, reorder(feature, -frequency))

grafico5 <- ggplot(frecuencia_debate, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafico5

#frecuencia por grupo
dfm_weight_pres <- data_corpus_inaugural %>%
  corpus_subset(Year > 2000) %>%
  dfm(remove = stopwords("english"), remove_punct = TRUE) %>%
  dfm_weight(scheme = "prop")

# Calculate relative frequency by president
frecuencia_grupo <- textstat_frequency(dfm_debate, n = 10, groups = "persona")
head(frecuencia_grupo)

grafico6 <- ggplot(data = frecuencia_grupo, aes(x = nrow(frecuencia_grupo):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(frecuencia_grupo):1,
                     labels = frecuencia_grupo$feature) +
  labs(x = NULL, y = "Relative frequency")
grafico6

#KEYNESS

# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(dfm_agrupado, target = "Macri")

# Plot estimated word keyness
textplot_keyness(result_keyness) 

#FCM
#co-ocurrencia de palabras
fcm_debate <- fcm(dfm_agrupado)
head(fcm_debate)
#Visualizando co-ocurencia de palabras
grafico7 <- textplot_network(fcm_debate, min_freq = 20,  vertex_size = size / max(size) * 3)
