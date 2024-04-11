install.packages("readxl")
library(readxl)

# Leer tabla, saltando las dos primeras lineas
tabla <- read_xlsx("Datos_LP.xlsx",
                    skip = 2)


# Sub-Tablas
tabla_tiempo_de_residencia <- table(tabla$`Tiempo de residencia en la vivienda actual (en años)`)
tabla_presion_de_agua <- table(tabla$`¿Cómo es la presión del agua?`)

#

cantidad_entrevistados <- sum(tabla_presion_de_agua)
frec_rel_presion_de_agua <- round(tabla_presion_de_agua / cantidad_entrevistados, 2)


# Boxplot para ver claramente la distribución
boxplot(tabla_tiempo_de_residencia,
        horizontal = TRUE,
        axes = FALSE,
        xlab = "Tiempo (en años)",
        main = "Tiempo de residencia en la vivienda actual.",
        col = "lightpink",
        sub = "Fuente: El Observatorio Villero, La Poderosa. Relevamiento de Condiciones habitacionales 2022",
        cex.sub = 0.7)

axis(side = 1,
     at = seq(0, 100, by = 10))


# Dado que las valores de las variables son excluyentes entre si, 
# ademas de ser pocos, el gráfico de torta permite ver de manera
# sencilla las condiciones de los entrevistados

labels_presion_de_agua <- paste(c("Buena", "Débil", "Muy débil"), "\n ", frec_rel_presion_de_agua * 100, "%")
pie(tabla_presion_de_agua,
    labels = labels_presion_de_agua,
    col = c("green3", "yellow", "red"),
    main = "¿Cómo es la presión del agua?",
    clockwise = TRUE)

mtext("Fuente: El Observatorio Villero, La Poderosa. \n Relevamiento de Condiciones habitacionales 2022",
      side = 1, cex = 0.9, line = 2, font = 2)

