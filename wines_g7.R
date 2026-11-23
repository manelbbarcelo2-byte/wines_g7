#----------------------------------------------------------------
# Sistema de Recomendación de Vinos // Grupo 7
#----------------------------------------------------------------
# 1. Análisis Exploratorio
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