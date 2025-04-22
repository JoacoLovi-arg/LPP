# Usando el dataframe visto en clase sobre delitos en CABA en 2023

# 1 - Mejorar visualmente el siguiente gráfico (pueden usar colores, acomodar labels de los ejes, agregar titulos, etc)

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

# 2 - Cuál fue el mes con mayor cantidad de delitos?

# 3 - En cuál comuna, los robos son el tipo de delito menos usual?

# 4 - Generar una preguna analítica y responderla con un gráfico