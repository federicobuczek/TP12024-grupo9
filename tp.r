# Paquetes y lectura de datos -----
install.packages("readxl")
library(readxl)

# Leer tabla, saltando las dos primeras lineas
tabla <- read_xlsx("Datos_LP.xlsx",
                    skip = 2)

# Globales ----------------

fuente <- "Fuente: El Observatorio Villero, La Poderosa. Relevamiento de Condiciones habitacionales 2022"
colores1 <- c("#79155B", "#C23373", "#F6635C", "#FFBA86")
colores2 <- c("#884A39", "#C38154", "#FFC26F", "#F9E0BB")
colores3 <- c("#00b4eb", "#d9d9d9")
cantidad_entrevistados <- 1222



# Sección 1: Características generales de la composición del hogar -----------

# Tiempo de residencia (Boxplot)

tabla_tiempo_de_residencia <- table(tabla$`Tiempo de residencia en la vivienda actual (en años)`)

boxplot(tabla_tiempo_de_residencia,
        horizontal = TRUE,
        axes = FALSE,
        xlab = "Tiempo (en años)",
        main = "Tiempo de residencia en la vivienda actual.",
        col = "lightpink",
        sub = fuente,
        cex.sub = 0.7)

axis(side = 1,
     at = seq(0, 100, by = 10))

# Integrantes por vivienda (Histograma)

integrantes_mediana <- paste("Mediana: ", median(tabla$`¿Cuántos integrantes hay en su vivienda?`))
cuartiles_integrantes <- quantile(tabla$`¿Cuántos integrantes hay en su vivienda?`)
rango_intercuartilico_integrantes <- paste("Rango intercuartílico: ", IQR(tabla$`¿Cuántos integrantes hay en su vivienda?`))
cuartiles_integrantes <- paste("Cuartiles: (", cuartiles_integrantes[2], ",",
                               cuartiles_integrantes[3], ",",
                               cuartiles_integrantes[4], ")")

plot(table(tabla$`¿Cuántos integrantes hay en su vivienda?`),
     type = 'h',
     xlim = c(1,10),
     xlab = "Integrantes por vivienda",
     ylab = "Frecuencia absoluta",
     main = "¿Cuántos integrantes hay en su vivienda?",
     sub = fuente,
     cex.sub = 0.7,
     col = "black",
     font.lab = 2,
     frame.plot = FALSE)

mtext(paste(integrantes_mediana, "\n", cuartiles_integrantes, "\n", rango_intercuartilico_integrantes),
      side = 3,
      cex = 0.7,
      adj = 1,
      line = -3,
      font = 2)


# Sección 5: Agua y saneamiento --------------

# Presión de agua (Grafico de torta)

tabla_presion_de_agua <- table(tabla$`¿Cómo es la presión del agua?`)
frec_rel_presion_de_agua <- round(tabla_presion_de_agua / cantidad_entrevistados, 4)

labels_presion_de_agua <- paste(c("Buena", "Débil", "Muy débil"), "\n ", frec_rel_presion_de_agua * 100, "%")
pie(tabla_presion_de_agua,
    labels = labels_presion_de_agua,
    col = colores2,
    main = "¿Cómo es la presión del agua?",
    sub = fuente,
    cex.sub = 0.9,
    clockwise = TRUE)

# Relación entre dos variables categóricas
# Grafico de barras subdivididas
tabla_bivariada_agua = table(tabla$`¿Tiene capacidad de almacenamiento de agua en altura?`,
                             tabla$`¿Cómo es la presión del agua?`)

barplot(tabla_bivariada_agua,
        main = "Presión de agua según si se tiene un tanque en altura",
        beside = FALSE,
        horiz = FALSE,
        ylim = c(0, 600),
        ylab = "Cantidad de viviendas",
        xlab = "Presión agua",
        las = 1,
        col = colores3,
        sub = fuente,
        border = NA,
        cex.sub = 0.8)

legend("topright",
       legend = rev(rownames(tabla_bivariada_agua)),
       fill = rev(colores3))


# Sección 10: Servicios barriales --------

# Presencia de plagas (total de la población) (Grafico de torta)

tabla_plaga <- table(tabla$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`)
porcentaje_plaga = paste(round(tabla_plaga / sum(tabla_plaga) * 100, 2), c('%'), sep = " ")
categorias_plaga = c("Sí", "No")
etiquetas_plaga = paste(categorias_plaga, porcentaje_plaga, sep = "\n")
pie(tabla_plaga,
    main = "¿Hay plagas en tu vivienda?",
    labels = etiquetas_plaga,
    cex = 0.8,
    sub = fuente,
    col = colores2,
    cex.sub = 0.8
)

# Tipo de plaga sobre población total (Grafico de torta)

cucarachas = table(factor(tabla$`¿Cuáles plagas?`))
mosquitos = table(factor(tabla$...94))
ratas = table(factor(tabla$...95))
tabla_plagas = data.frame(FrecuenciaAbsoluta = c(cucarachas, mosquitos, ratas))
frec_rel_plagas = round(tabla_plagas$FrecuenciaAbsoluta / cantidad_entrevistados, 2)
names(frec_rel_plagas) = c("Cucarachas", "Mosquitos", "Ratas")
frec_rel_plagas = frec_rel_plagas[order(frec_rel_plagas, decreasing = TRUE)]
par(mar = c(5, 6, 4, 2) + 0.1)
barplot(frec_rel_plagas,
        horiz = TRUE,
        xlim = c(0, 1),
        xlab = "Frecuencia relativa",
        main = "¿Cuales plagas?",
        sub = fuente,
        col = colores2,
        las = 1,
        cex.sub = 0.8)

