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
tiempo_de_residencia_mediana <- paste("Mediana: ", median(tabla_tiempo_de_residencia))
cuartiles_tiempo_de_residencia <- quantile(tabla_tiempo_de_residencia)
IQR_tiempo_de_residencia <- paste("Rango intercuartílico: ", IQR(tabla_tiempo_de_residencia))
cuartiles_tiempo_de_residencia <- paste("Cuartiles: (", cuartiles_tiempo_de_residencia[2], ",",
                                        cuartiles_tiempo_de_residencia[3], ",",
                                        cuartiles_tiempo_de_residencia[4], ")")

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

mtext(paste(tiempo_de_residencia_mediana, "\n", cuartiles_tiempo_de_residencia, "\n", IQR_tiempo_de_residencia),
      side = 3,
      cex = 0.7,
      adj = 1,
      line = -3,
      font = 2)

# El uso de la mediana viene de la forma asimétrica a derecha de la distribución.
# Al no ser simétrica, el promedio puede resultar no representativo de los datos
# en relación a la mediana.
# Los cuartiles permiten entender la distribución, haciendo observar en que 
# dirección se presentan los casos extremos
# El IQR permite ver que tan dispersos están los datos de la mediana, y no es
# influenciada por valores atípicos.

# Integrantes por vivienda (Histograma)

integrantes_mediana <- paste("Mediana: ", median(tabla$`¿Cuántos integrantes hay en su vivienda?`))
cuartiles_integrantes <- quantile(tabla$`¿Cuántos integrantes hay en su vivienda?`)
IQR_integrantes <- paste("Rango intercuartílico: ", IQR(tabla$`¿Cuántos integrantes hay en su vivienda?`))
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

mtext(paste(integrantes_mediana, "\n", cuartiles_integrantes, "\n", IQR_integrantes),
      side = 3,
      cex = 0.7,
      adj = 1,
      line = -3,
      font = 2)

# El uso de la mediana viene de la forma asimétrica a derecha de la distribución.
# Al no ser simétrica, el promedio puede resultar no representativo de los datos
# en relación a la mediana.
# Los cuartiles permiten entender la distribución, haciendo observar en que 
# dirección se presentan los casos extremos
# El IQR permite ver que tan dispersos están los datos de la mediana, y no es
# influenciada por valores atípicos.


# Sección 3: Hacinamiento----

# Máximo de personas en dormitorio según integrantes (Diagrama de dispersión)
# Correlación lineal positiva

cY1 = tabla$`¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?`
cX1 = tabla$`¿Cuántos integrantes hay en su vivienda?`
plot(cX1, 
     cY1,
     main = "Máximo de personas en dormitorio segun integrantes",
     ylab = "Máximo personas en un solo dormitorio",
     xlab = "Integrantes por vivienda",
     xaxt = 'n',
     yaxt = 'n',
     sub = fuente,
     cex.sub = 0.6,
     col = "#00000020",
     pch=16)

fit1 <- lm(cY1 ~ cX1)
abline(fit1)

axis(2, at = cY1)
axis(1, at = cX1)

# Sección 4: Propiedad ----

# Certificados de RENABAP (Grafico de torta)

tabla_RENABAP <- table(tabla$`¿Posee el Certificado de Vivienda (RENABAP)`)
frec_rel_RENABAP <- round(tabla_RENABAP / cantidad_entrevistados, 4)
labels_RENABAP <- paste(c("No", "No corresponde", "Si"), "\n ", frec_rel_RENABAP * 100, "%")

pie(tabla_RENABAP,
    labels = labels_RENABAP,
    col = colores1,
    main = "¿Posee el Certificado de Vivienda (RENABAP)?",
    sub = fuente,
    cex.sub = 0.9,
    clockwise = TRUE)

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
        main = "Presión de agua según disponibilidad de tanque en altura",
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


# Sección 7: Electricidad ----

# Tipo de conexión eléctrica (Barplot)

tabla_conexion_electrica <- table(tabla$`¿Qué tipo de conexión posee a la red eléctrica?`)
tabla_conexion_electrica <- tabla_conexion_electrica[order(tabla_conexion_electrica, decreasing = TRUE)]

barplot(tabla_conexion_electrica, names.arg = c("Sin medidor", "Medidor particular",
                                        "Medidor comunitario", "Sin conexión"),
        ylim = c(0, 600),
        col = "lightpink",
        main = "¿Qué tipo de conexión posee a la red eléctrica?",
        sub = fuente)
  
# Sección 8: Conectividad ----

tabla_internet <- table(tabla$`¿Posee servicio de internet de banda ancha en la vivienda?`)
tabla_internet <- tabla_internet[order(tabla_internet, decreasing = TRUE)]

si_internet <- sum(tabla_internet[2:5])
no_internet <- tabla_internet[1]
frec_rel_internet <- round(rbind(si_internet, no_internet) / cantidad_entrevistados, 4)

print(frec_rel_internet)

labels_internet <- paste(c("Si", "No"), "\n ", frec_rel_internet * 100, "%")

pie(rbind(si_internet, no_internet),
    labels = labels_internet,
    clockwise = TRUE,
    main = "¿Posee servicio de internet de banda ancha en la vivienda?",
    sub = fuente,
    col = colores2)

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

# Tipo de plaga sobre población total
# Variable categórica de múltiples opciones

# Calculamos las frecuencias relativas de cada plaga
# y las ordenamos 
cucarachas = table(factor(tabla$`¿Cuáles plagas?`))
mosquitos = table(factor(tabla$...94))
ratas = table(factor(tabla$...95))
tabla_plagas = data.frame(FrecuenciaAbsoluta = c(cucarachas, mosquitos, ratas))
frec_rel_plagas = round(tabla_plagas$FrecuenciaAbsoluta / cantidad_entrevistados, 2)
names(frec_rel_plagas) = c("Cucarachas", "Mosquitos", "Ratas")
frec_rel_plagas = frec_rel_plagas[order(frec_rel_plagas, decreasing = TRUE)]

# Lugar de las marcas
xmin_p = 0
xmax_p = 0.6
sep_p = 0.1

par(mar = c(5, 6, 4, 2) + 0.1)

barplot(frec_rel_plagas,
        horiz = TRUE,
        xlim = c(xmin_p, xmax_p),
        xlab = "Frecuencia relativa",
        main = "¿Cuales plagas?",
        sub = fuente,
        las = 1,
        cex.sub = 0.8,
        axes = FALSE)

# Añadimos etiquetas en el eje x
etiquetas_plagas = paste(seq(xmin_p * 100, xmax_p * 100, sep_p * 100), '%', sep="")
axis(side=1, 
     at = seq(xmin_p, xmax_p, sep_p), 
     labels = etiquetas_plagas)
abline(v = axTicks(1), lty = "dashed")

barplot(frec_rel_plagas,
        horiz = TRUE,
        xlim = c(xmin_p, xmax_p),
        col = colores3[1],
        las = 1,
        cex.sub = 0.8,
        axes = FALSE,
        add = TRUE)

