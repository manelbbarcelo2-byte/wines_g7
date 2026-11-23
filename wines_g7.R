#----------------------------------------------------------------
# Sistema de Recomendación de Vinos // Grupo 7
#----------------------------------------------------------------
# 1. Análisis Exploratorio
#----------------------------------------------------------------
library(dplyr)
#----------------------------------------------------------------
datos <- read.csv("wines_SPA.csv", stringsAsFactors = FALSE)
#----------------------------------------------------------------
dim(datos)
#----------------------------------------------------------------
head(datos)
#----------------------------------------------------------------
str(datos)
#----------------------------------------------------------------
summary(datos)
#----------------------------------------------------------------
# nº valores valores únicos por variable
sapply(datos, function(x) length(unique(x)))
#----------------------------------------------------------------
# valores missing por variable
colSums(is.na(datos))
#----------------------------------------------------------------
# % valores missing
colMeans(is.na(datos)) * 100
#----------------------------------------------------------------
# frecuencia body y acidity
table(datos$body, useNA = "ifany")
table(datos$acidity, useNA = "ifany")
#----------------------------------------------------------------
# frecuencia añada
table(datos$year, useNA = "ifany")
#----------------------------------------------------------------
# nº filas duplicadas
sum(duplicated(datos))
#----------------------------------------------------------------
# = combinación bodega + vino + añada
sum(duplicated(datos[c("winery", "wine", "year")]))
#----------------------------------------------------------------
# Cuál es el propósito de nuestro análisis?
# Si eliminamos duplicados, cómo nos afecta?
#----------------------------------------------------------------
#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
#----------------------------------------------------------------
# eliminar duplicados exactos
datos_unicos <- distinct(datos)
#----------------------------------------------------------------
# función para recuperar el primer valor no ausente
primer_valor <- function(x) {
  valores <- x[!is.na(x)]
  
  if (length(valores) == 0) {
    return(x[NA_integer_])
  }
  
  return(valores[1])
}
#----------------------------------------------------------------
# agrupar por vino y recuperar los valores no ausentes
datos_limpios <- datos_unicos %>%
  group_by(winery, wine, year) %>%
  summarise(
    rating = primer_valor(rating),
    num_reviews = primer_valor(num_reviews),
    country = primer_valor(country),
    region = primer_valor(region),
    price = primer_valor(price),
    type = primer_valor(type),
    body = primer_valor(body),
    acidity = primer_valor(acidity),
    .groups = "drop"
  )
#----------------------------------------------------------------
# dimensiones dataset final
dim(datos_limpios)