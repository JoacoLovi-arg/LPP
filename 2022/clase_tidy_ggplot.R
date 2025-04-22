#Clase tidyverse y ggplot2####

##Cargo/instalo paquetes----
library(dplyr)
library(ggplot2)

##Genero objetos leyendo los csv----

superficie <- 
  read_csv("data/superficie_incendiada_provincias_tipo_vegetacion.csv", 
           locale = locale(encoding = "UTF-8")) %>% 
  print()

cantidad <- 
  read_csv2("data/incendios-cantidad-causas-provincia.csv",
            locale = locale(encoding = "latin1")) %>% 
  print()

protegidas <- 
  read_csv2("data/areas_protegidas_provincia.csv",
            locale = locale(encoding = "latin1")) %>% 
  print()

##select y rename----

superficie_selec <-
  superficie %>% 
  dplyr::select(1:3) %>% 
  rename(anio = 1,
         provincia = 2,
         total_hectareas = 3) %>% 
  print()

cantidad_selec <-
  cantidad %>% 
  select(anio = incendio_anio,
         provincia = incendio_provincia,
         total_incendios = incendio_total_numero) %>% 
  print()

protegidas_selec <- 
  protegidas %>% 
  select(1:5) %>%
  print()


##geom_col----

ggplot() + 
  geom_col(data = protegidas_selec, 
           mapping = aes(x = provincia_desc, y = areas_naturales_protegidas_porcentaje)) +
  coord_flip()

##geom_bar----

ggplot() +
  geom_bar(data = cantidad_selec,
           mapping = aes(x = provincia, y = total_incendios),
           stat = "identity") +
  coord_flip()

##geom_line----

ggplot()+
  geom_line(data = cantidad_selec, 
            mapping = aes(x = anio, y = total_incendios, 
                          group = provincia, colour = provincia))

##unique----
unique(cantidad_selec$provincia)
unique(cantidad_selec$provincia)
unique(superficie_selec$provincia)
unique(protegidas_selec$provincia_desc)

##mutate####
library(stringr)
cantidad_mutate <- 
  cantidad_selec %>% 
  mutate(provincia = str_to_lower(provincia),
         provincia = chartr("áéíóú", "aeiou", provincia)) %>% 
  print()

# library(stringi)
# mutate(provincia = stri_trans_general(provincia,"Latin-ASCII"))
unique(cantidad_mutate$provincia)

superficie_mutate <-
  superficie_selec %>%
  mutate(provincia = chartr("áéíóú", "aeiou", str_to_lower(provincia)),
         provincia = case_when(provincia == "ciudad autonoma de bs as" ~ "ciudad autonoma de buenos aires",
                               provincia == "ciudad autonoma de" ~ "ciudad autonoma de buenos aires",
                               TRUE ~ provincia)) %>% 
  print()

unique(superficie_mutate$provincia)

###edit----

protegidas_edit <- edit(protegidas_selec)
unique(protegidas_edit$provincia_desc)

protegidas_mutate <- 
  protegidas_edit %>% 
  mutate(provincia_desc = str_to_lower(provincia_desc),
         provincia_desc = chartr("áéíóú", "aeiou", provincia_desc)) %>% 
  print()


##join####

incendios <- inner_join(cantidad_mutate,superficie_mutate) %>%
  print()

incendios <- left_join(incendios, protegidas_mutate, by = c("provincia" = "provincia_desc")) %>% 
  print()

unique(incendios$provincia)
###opcion 2####

incendios <- inner_join(cantidad_mutate,superficie_mutate) %>% 
  left_join(protegidas_mutate, by = c("provincia" = "provincia_desc")) %>%
  print()

incendios <-  incendios %>% 
  select(provincia_id, everything()) %>% 
  print()

##filter####

incendios_filtro <-
  incendios %>% 
  filter(provincia == "buenos aires" | provincia == "catamarca",
         anio >= 2010) %>% 
  mutate(anio = as.factor(anio),
         total_km2_incendiados = round(total_hectareas/100,1)) %>% #Paso hectareas a km
  glimpse()


##Plot 2019 ####

ggplot(incendios_filtro)+
  geom_col(mapping = aes(x = anio, y = total_incendios,
                         group = provincia, fill = provincia), 
           alpha = 0.5) +
  geom_line(mapping = aes(x = anio, y = total_km2_incendiados,
                          group = provincia, colour = provincia),
            size = 3) +
  geom_label(mapping = aes(x = anio, y = total_incendios, 
                           label = total_incendios),
             position = position_dodge2(0.9)) +
  geom_text(mapping = aes(x = anio, y = total_km2_incendiados,
                          label = total_km2_incendiados)) +
  labs(title = "Incendios Forestales en Catamarca y Buenos Aires",
       subtitle = "2010 - 2019",
       caption = "Las columnas indican cantidad de focos de incendio y las líneas indican cantidad de hectáreas incendiadas")
theme()



#Si llegamos hacemos bonus track de bind cols, pero innecesario tanto castigo.
##Plot 2018-2019 binded.