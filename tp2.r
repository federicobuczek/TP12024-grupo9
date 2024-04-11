install.packages("readxl")
library(readxl)

# Leer tabla, saltando las dos primeras lineas
tabla <- read_xlsx("Datos_LP.xlsx",
                    skip = 2)
fuente <- "Fuente: El Observatorio Villero, La Poderosa. Relevamiento de Condiciones habitacionales 2022"
colores1 <- c("#79155B", "#C23373", "#F6635C", "#FFBA86")
colores2 <- c("#884A39", "#C38154", "#FFC26F", "#F9E0BB")
cantidad_entrevistados <- 1222

# Sub-Tablas
tabla_tiempo_de_residencia <- table(tabla$`Tiempo de residencia en la vivienda actual (en años)`)
tabla_presion_de_agua <- table(tabla$`¿Cómo es la presión del agua?`)
frec_rel_presion_de_agua <- round(tabla_presion_de_agua / cantidad_entrevistados, 2)


# Boxplot para ver claramente la distribución
boxplot(tabla_tiempo_de_residencia,
        horizontal = TRUE,
        axes = FALSE,
        xlab = "Tiempo (en años)",
        main = "Tiempo de residencia en la vivienda actual.",
        col = "lightpink",
        sub = fuente)

axis(side = 1,
     at = seq(0, 100, by = 10))


# Dado que las valores de las variables son excluyentes entre si, 
# ademas de ser pocos, el gráfico de torta permite ver de manera
# sencilla las condiciones de los entrevistados

labels_presion_de_agua <- paste(c("Buena", "Débil", "Muy débil"), "\n ", frec_rel_presion_de_agua * 100, "%")
pie(tabla_presion_de_agua,
    labels = labels_presion_de_agua,
    col = colores,
    main = "¿Cómo es la presión del agua?",
    clockwise = TRUE)

tabla_plaga <- table(tabla$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`)
porcentaje_plaga = paste(round(tabla_plaga / sum(tabla_plaga) * 100, 2), c('%'), sep = " ")
categorias_plaga = c("Sí", "No")
etiquetas_plaga = paste(categorias_plaga, porcentaje_plaga, sep = "\n")
pie(tabla_plaga,
    main = "¿Hay plagas en tu vivienda?",
    labels = etiquetas_plagas,
    cex = 0.8,
    sub = fuente,
    col = colores1,
    cex.sub = 0.8
)

cucarachas = table(factor(tabla$`¿Cuáles plagas?`))
mosquitos = table(factor(tabla$...94))
ratas = table(factor(tabla$...95))
tabla_plagas = data.frame(FrecuenciaAbsoluta = c(cucarachas, mosquitos, ratas))
frec_rel_plagas = round(tabla_plagas$FrecuenciaAbsoluta / cantidad_entrevistados, 2)
names(frec_rel_plagas) = c("cucarachas", "mosquitos", "ratas")
frec_rel_plagas = frec_rel_plagas[order(frec_rel_plagas, decreasing = TRUE)]
par(mar = c(5, 6, 4, 2) + 0.1)
barplot(frec_rel_plagas,
        horiz = TRUE,
        xlim = c(0, 1),
        xlab = "Frecuencia relativa",
        main = "¿Cuales plagas?",
        sub = fuente,
        col = colores1,
        las = 1,
        cex.sub = 0.8)

