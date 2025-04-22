library(httr)

res <- GET("https://resultados.mininterior.gob.ar/api/resultados/getResultados?
           anioEleccion=2019&
           tipoRecuento=1&
           tipoEleccion=1&
           categoriaId=2&
           distritoId=1&
           seccionProvincialId=0&
           seccionId=3&
           circuitoId=000039&
           mesaId=1244")
res
rm(res)


base_url <- "https://resultados.mininterior.gob.ar/api/resultados/getResultados"

anioEleccion <- 2019
tipoEleccion <- 1 #PASO, 2 #Generales, 3 #Balotaje
categoriaId <- 2
url <- paste0()

curl::curl_


# First, install and load the "rapiclient" package:
install.packages("rapiclient")
library(rapiclient)
?rapiclient
# Set up the RapidAPI endpoint URL and API key:
url <- "https://resultados.mininterior.gob.ar"
endpoint <- "api/resultados/getResultados"

# api_key <- "your_api_key_here"

# Create a new RapidAPI client object:
# api <- get_api(paste(url,endpoint, sep = "/"))
ins <- get_api("https://resultados.mininterior.gob.ar/docs/Insomnia_PublicacionResultadosElectorales-2023-03-22.yaml")
# tec <- get_api("https://resultados.mininterior.gob.ar/docs/api-publicacion-resultados-electorales.yaml")
ins

operations <- get_operations(ins)
schemas <- get_schemas(ins)

# Set up the API endpoint and any necessary parameters:
params <- list(
  "parameter1" = "value1",
  "parameter2" = "value2"
)

# Make a request to the API and store the result in a variable:
result <- call_rapidapi(client, endpoint, params)

# Process the resulting data as needed:
print(result)
?call_rapidapi





url <- "https://resultados.mininterior.gob.ar"
endpoint <- "api/resultados/getResultados"

raw_result <- GET(url = url, path = endpoint)
names(raw_result)

raw_result$status_code


url <- "https://apitransporte.buenosaires.gob.ar"
endpoint <- "colectivos/vehiclePositions"
id <- "63d38f6134db4dc7b13b6afb40de6b91"
secret <- "C7D086347B5f43288c10a63a99f6a777"

raw_result <- GET(url = url, path = endpoint, 
                  add_headers("json" = "1",
                              "client_id" = id, 
                              "client_secret" = secret))

GET(url = url, hand)

GET("https://apitransporte.buenosaires.gob.ar/colectivos/vehiclePositions?client_id=63d38f6134db4dc7b13b6afb40de6b91&client_secret=C7D086347B5f43288c10a63a99f6a777")

raw_result$request
raw_result$status_code
####

response <- GET(url = url, path = endpoint, 
                add_headers("client_id" = id, 
                            "client_secret" = secret))
response$request

if (status_code(response) == 200) {
  # Parse the response content into a list or data frame as appropriate for your use case
  results <- content(response, as = "parsed")
  # Do something with the results...
} else {
  # Print an error message if the API request was unsuccessful
  message("Error: ", status_code(response), " - ", response)
}
