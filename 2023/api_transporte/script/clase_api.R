library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
# install.packages("ggmap")
library(ggmap)
install.packages("osmdata")
library(osmdata)

#Agrgo mis datos de id y secret entre comillas

client_id <- "63d38f6134db4dc7b13b6afb40de6b91"
client_secret <- "C7D086347B5f43288c10a63a99f6a777"

#Direccion completa.
url <- paste0("https://apitransporte.buenosaires.gob.ar/colectivos/vehiclePositionsSimple?&client_id=", client_id, "&client_secret=", client_secret)

#Request GET
respuesta <- GET(url = url)

respuesta

#Parseamos el contenido de hexadecimal a raw
raw_content <- rawToChar(respuesta$content)

#veamos los primeros 500 caracteres
substr(raw_content, 1, 500)

#Le damos formato
content <- fromJSON(raw_content)
glimpse(content)
#Guardamos el contenido
write_csv(content, "content_transporte.csv")
content <- read_csv("content_transporte.csv")
#####

min(content$speed)
max(content$speed)

cantidad_bondis <- content %>% 
  count(route_short_name) %>% 
  arrange(desc(n)) %>% 
  # mutate(route_short_name = str_remove_all(route_short_name, pattern = "\\D")) %>% 
  # filter(route_short_name == "86") %>% 
  glimpse()
  
filtro <- content %>% 
  filter(route_short_name %in% cantidad_bondis$route_short_name[1:10]) %>% 
  print()

mapa1 <- content %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()

mapa1

bondis <- filtro %>% 
  mutate(longitude = case_when(longitude > 0 ~ longitude*-1,
                               TRUE ~ longitude)) %>% 
  glimpse()


mapa2 <- bondis %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()

mapa2

amba_bb <- getbb("Buenos Aires") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf()

mapa3 <- 
  ggplot(data = bondis, aes(x = longitude, y = latitude)) +
  geom_point() +
  geom_sf(data = amba_bb$osm_lines)

mapa3
