library(httr)

# Define variables for each query parameter
anio_eleccion <- "2019"
tipo_recuento <- "1"
tipo_eleccion <- "1"
categoria_id <- "2"
distrito_id <- "1"
seccion_provincial_id <- "0"
seccion_id <- "3"
circuito_id <- "000039"
mesa_id <- "1244"

# Construct the endpoint URL string with interpolated parameter values
endpoint <- paste0("https://resultados.mininterior.gob.ar/api/resultados/getResultados?anioEleccion=", anio_eleccion,
                   "&tipoRecuento=", tipo_recuento,
                   "&tipoEleccion=", tipo_eleccion,
                   "&categoriaId=", categoria_id,
                   "&distritoId=", distrito_id,
                   "&seccionProvincialId=", seccion_provincial_id,
                   "&seccionId=", seccion_id,
                   "&circuitoId=", circuito_id
                   # "&mesaId=", mesa_id
                   )

# Make the API request
response <- GET(endpoint)

# Extract the response content as a character string
response_text <- content(response, as = "text")

