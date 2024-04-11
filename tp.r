install.packages("readxl")
library(readxl)

# Leer tabla, saltando las dos primeras lineas
tabla2 <- read_xlsx("Datos_LP.xlsx",
                    
                    skip = 2)

tabla_presion <- table(tabla2$`¿Cómo es la presión del agua?`)


    