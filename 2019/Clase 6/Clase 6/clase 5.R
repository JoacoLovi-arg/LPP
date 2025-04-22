install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("lubridate")
install.packages("quanteda")
install.packages("dplyr")
install.packages("stopwords")
install.packages("devtools")
install.packages("remote")
library(remote)
install.packages("remotes")
library(remotes)
library(devtools)
devtools::install_github("JBGruber/rwhatsapp")
force = TRUE

install_version("backports", version = "1.1.0")


library("dplyr")
library("rwhatsapp")
library("ggplot2")
library("lubridate")
library("tidyr")
library("tidytext")
library("quanteda")
library("stopwords")

chat <- rwa_read("LOPIBE.txt")

chat <- chat %>%
  mutate(day=date(time)) %>%
  filter(author!="NA") %>%
  mutate(hour=hour(time))

chat_filtrado<-chat %>%
  mutate(day=date(time)) %>%
  filter(author!="NA") %>%
  filter(text!="<Multimedia omitido>")
  mutate(hour=hour(time))
  
  
chatpordia <-chat %>%
  count(day) %>%
  
grafico1<-chatpordia%>%
  ggplot(aes(x=day, y=n))+
  geom_bar(stat = "identity")
grafico1


chatporhora<-chat%>%
  count(hour)%>%
  ggplot(chatporhora, aes(x=hour, y=n))+
  geom_bar(stat="identity")
  
chatoporusuario<-chat_filtrado%>%
  count(author)%>%
  ggplot(chatoporusuario, aes(x=reorder(author,n), y=n))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("NIVEL DE LORO")

filtro_de_palabras<-c("ja","jaj","jaja","jajaj","jajaja","www.instagram.com",
                      "ww.facebook.com","es","ah?","va" ,"van","re","sos",
                      "voy" , "ahora" , "paso" , "ver" , "mismo" , "nunca",
                      "hoy" , "mas" , "3" , "a?os" , "mejor" , "2" , 
                      "asi" , "hace" , "hizo" , "omitido", "vamos" ,
                      "p", "ma?ana", "cara", "grupo", "aca", "4",
                      "haces", "casa", "dale", "quiero" ,
                      "ahi","multimedia" ,"bueno","si","vos","dia","ah?","ah","bien",
                      "porq","feliz","cumple","despu?s","imagen","q","omitida","https"
                      ,"c","x", "tb", "tmb","pq","xq", "dsps", "desp", "jajajaj",
                      "jajjaja","nose","tambien", "etc")

chat_palabras<- chat_filtrado%>%
  unnest_tokens(input = "text", output = "word")%>%
  select(word, author)%>%
  filter(!word%in%c(stopwords(language = "es"),filtro_de_palabras))%>%
  count(author, word, sort = TRUE)%>%
  bind_tf_idf(term = word, document = author, n=n)%>%
  filter(n>10)%>%
  group_by(author)%>%
  top_n(n=3)

ggplot(chat_palabras, aes(x=reorder(word,n),y=n, fill=author))+
  geom_bar(stat="identity", show.legend = FALSE)+
  ylab("Cantidad")+
  xlab("Palabra")+
  coord_flip()+
  facet_wrap(~author, ncol = 2, scales="free_y")

chat_diversidad <- chat_filtrado %>%
  unnest_tokens(input = "text", output = "word") %>%
  filter(!word %in% c(stopwords(language = "es"), filtro_de_palabras)) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))

ggplot(chat_diversidad, aes(x=reorder(author, lex_diversity), y= lex_diversity, fill=author)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Cantidad Palabras") +
  xlab("Miembros") +
  ggtitle("Lexical Diversity") +
  theme_bw()+
  coord_flip()

library(readtext)

corpus_chat<-corpus(chat_filtrado)
head(corpus_chat)

corpus_joaco<-corpus_subset(corpus_chat,author=="joaquin lovizio" )
tokens_joaco<-tokens(corpus_joaco, remove_numbers = TRUE,  remove_punct = TRUE)

dfm_joaco<-dfm(corpus_joaco, 
               remove = c(stopwords("spanish"),filtro_de_palabras), 
               remove_punct = TRUE)

nube_joaco<-textplot_wordcloud(dfm_joaco, 
                               min_count = 6, 
                               random_order = FALSE,
                               rotation = .25, 
                               color = RColorBrewer::brewer.pal(8,"Dark2"))

corpus_chicho<-corpus_subset(corpus_chat, author=="Manu Chicho")
dfm_chicho<-dfm(corpus_chicho, 
                remove = c(stopwords("spanish"), filtro_de_palabras),
                remove_punct=TRUE)
nube_chicho<-textplot_wordcloud(dfm_chicho, 
                               min_count = 6, 
                               random_order = FALSE,
                               rotation = .25, 
                               color = RColorBrewer::brewer.pal(8,"Dark2"))

corpus_corcho<-corpus_subset(corpus_chat, author=="Russo Cel")
dfm_corcho<-dfm(corpus_corcho, 
                remove = c(stopwords("spanish"), filtro_de_palabras),
                remove_punct=TRUE)
nube_chicho<-textplot_wordcloud(dfm_corcho, 
                                min_count = 6, 
                                random_order = FALSE,
                                rotation = .25, 
                                color = RColorBrewer::brewer.pal(8,"Dark2"))