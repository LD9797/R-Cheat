
# 1. Carga de datos
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Lectura de datos
# Ejecutar formatter_tarea_2.py previo a la lectura de datos, si no se han formateado los datos Datos_tarea_2.txt !
Data <- read.csv2("Datos_fixed_tarea_2.csv", sep = ";", header = TRUE)
Data$Tiempo <- as.numeric(Data$Tiempo)
Data$Objetos <- as.character(Data$Objetos)
Data$Objetos <- factor(Data$Objetos, levels = unique(Data$Objetos))

# 2. Verificación de la lectura de datos.
library(psych)
headTail(Data)
str(Data)
summary(Data)

# 3. Gráfico simple de interacción
# Variable dependiente: Tiempo
# Variables independientes: Arquitectura y Objetos
par(mfrow = c(3, 1))
interaction.plot(x.factor = Data$Objetos,
 trace.factor = Data$Arquitectura,
 response = Data$Tiempo,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")

# Variable dependiente: Tiempo
# Variables independientes: Arquitectura y Efectos
interaction.plot(x.factor = Data$Efectos,
 trace.factor = Data$Arquitectura,
 response = Data$Tiempo,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")

# Variable dependiente: Tiempo
# Variables independientes: Arquitectura y Resolucion
interaction.plot(x.factor = Data$Resolucion,
 trace.factor = Data$Arquitectura,
 response = Data$Tiempo,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")

# Todo parece indicar que el algoritmo híbrido siendo ejecutado en el APU es más rápido para sintetizar imágenes
# comparado con lo que se pueden considerar otros algoritmos tradicionales que hacen uso de solo el CPU o el GPU.

# 4. Evaluación de los supuestos.

# Función para graficos de los supuestos.
graficos_supuestos <- function(model) {
  par(mfrow = c(3, 1))
  x <- residuals(model)
  library(rcompanion)
  plotNormalHistogram(x)
  plot(fitted(model), residuals(model))
  qqnorm(resid(model), main = "Normal Q-Q",
       xlab = "Theoretical Quantiles", ylab = "Standarized residuals")
  qqline(resid(model), col = "red", lwd = 2)
}

# Datos iniciales originales
model <- lm(Tiempo ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
graficos_supuestos(model)
leveneTest(Tiempo ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)

# No parece haber ni normalidad ni homocedasticidad.

# 5. Transformación de datos.
# Se procede a hacer transformación iniciando desde la forma menos agresiva a la mas agresiva hasta cumplir los supuestos.

# Tranformación por raíz cuadrada.
library(rcompanion)
T_sqrt <- sqrt(Data$Tiempo)
model <- lm(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
graficos_supuestos(model)
leveneTest(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)

# Con la transformacion por raíz cuadrada:
# 1. Histograma de la normalidad se asemeja al de los datos originales, sin embargo el gráfico QQ sugiere la posibilidad
# de normalidad.
# 2. Los gráficos de los residuos muestran una mejor homocedasticidad.
# 3. La prueba Levene regresa un P-Value de 0.3718, por lo que refuerza lo sugerido por los gráficos de los residuos de que
# los datos son homocedásticos.

# Se considera utilizar raíz cúbica para evaluar si la normalidad y homocedasticidad puede mejorar.

# Transformacion por raíz cúbica

library(rcompanion)
T_cub <- sign(Data$Tiempo) * abs(Data$Tiempo)^(1/3)
model_cub <- lm(T_cub ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
graficos_supuestos(model_cub)
leveneTest(T_cub ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)

# Con la transformacion de raíz cúbica:
# 1. Histograma de normalidad muy similar al de raíz cuadrada y gráfico QQ más alejado a la línea de la normal.
# 2. Los gráficos de los residuos muestran una homocedasticidad similar a la de raíz cuadrada.
# 3. La prueba Levene regresa un P-Value de 0.2744, menor comparado a la prueba con raíz cuadrada.

# Los datos no mejoraron con esta transformación, se decide continuar con los datos transformados por raíz cuadrada.

# 6. Anova
library(car)
Anova(model, type = "II")

# Comparaciones relevantes:
# 1. Arquitectura:
# Este factor es esencial ya que compara directamente el rendimiento de las arquitecturas APU, CPU y GPU.
# Se quiere saber si existe una diferencia significativa en el tiempo de ejecución entre estas tres arquitecturas.
# 2. Arquitectura:Efectos
# Esta interacción ayuda a comprender si las diferencias de rendimiento entre arquitecturas dependen de los factores
# aplicados.
# 3. Arquitectura:Objetos
# Esta interacción ayuda a comprender si las diferencias de rendimiento entre las arquitecturas dependen de la
# cantidad de objetos en la imagen.
# 4. Arquitectura:Resolucion: Esta interacción ahora es relevante porque te ayudará a entender si las diferencias de
# rendimiento entre las arquitecturas dependen de la resolución.


# 7. Gráficos finales

par(mfrow = c(1, 1))

graficos_box <- function(Sum, factor_1, factor_2) {
    library(ggplot2)
    pd <- position_dodge(.2)
    ggplot(Sum, aes(x = factor_1,
                y = mean, color = factor_2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, size = 0.7, position = pd) +
    geom_point(shape = 15, size = 4, position = pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    scale_colour_manual(values = c("black", "red", "green")) +
    ylab("Raiz cuadrada de tiempo")
}

# Únicamente arquitectura
Sum <- Summarize(T_sqrt ~ Arquitectura, data = Data, digits = 3)
# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
graficos_box(Sum, Sum$Arquitectura, Sum$Arquitectura)

# Efectos + Arquitectura
Sum <- Summarize(T_sqrt ~ Efectos + Arquitectura, data = Data, digits = 3)
# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
# Ordenamos
Sum$Efectos <- factor(Sum$Efectos, levels = unique(Sum$Efectos))
graficos_box(Sum, Sum$Efectos, Sum$Arquitectura)


# Objetos + Arquitectura
Sum <- Summarize(T_sqrt ~ Objetos + Arquitectura, data = Data, digits = 3)
# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum$Objetos <- factor(Sum$Objetos, levels = unique(Sum$Objetos))
graficos_box(Sum, Sum$Objetos, Sum$Arquitectura)


# Resolucion + Arquitectura
Sum <- Summarize(T_sqrt ~ Resolucion + Arquitectura, data = Data, digits = 3)
# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
# Ordenamos
Sum$Resolucion <- factor(Sum$Resolucion, levels = unique(Sum$Resolucion))
graficos_box(Sum, Sum$Resolucion, Sum$Arquitectura)

# 8. Pairwise t-tests

# Pairwise T-test Arquitectura
# Análisis de normalidad
plotNormalHistogram(Data$Tiempo)
# No se cumple normalidad, se transformand datos.
# Tranformación por raíz cuadrada.
library(rcompanion)
T_sqrt_ar <- sqrt(Data$Tiempo)
plotNormalHistogram(T_sqrt_ar) # No es normal
# Tranformación por raíz cúbica.
T_cub_ar <- sign(Data$Tiempo) * abs(Data$Tiempo)^(1/3)
plotNormalHistogram(T_cub_ar) # No es normal
# Transformación por logaritmo.
T_log_ar <- log(Data$Tiempo) # No es normal
plotNormalHistogram(T_log_ar)
# Transformación logaritmica acerca más los datos a una distribución normal
# pairwise T-test
pairwise.t.test(T_log_ar, Data$Arquitectura)

# Pairwise T-test objetos = 4000
data_obj_1000 <- subset(Data, Objetos == 4000)
data_obj_1000_log <- log(data_obj_1000$Tiempo)
plotNormalHistogram(data_obj_1000_log)
pairwise.t.test(data_obj_1000_log, data_obj_1000$Arquitectura)

# 12. Mejora en porcentajes

df <- data.frame(Data$Arquitectura, Data$Tiempo)
percent_diff <- function(first_value, second_value) {
  ((first_value - second_value) / first_value) * 100
}
promedio_tiempos <- aggregate(df$Data.Tiempo ~ df$Data.Arquitectura, df, mean)
percent_diff(promedio_tiempos[promedio_tiempos$`df$Data.Arquitectura` == "CPU", "df$Data.Tiempo"],
             promedio_tiempos[promedio_tiempos$`df$Data.Arquitectura` == "APU", "df$Data.Tiempo"])
percent_diff(promedio_tiempos[promedio_tiempos$`df$Data.Arquitectura` == "GPU", "df$Data.Tiempo"],
             promedio_tiempos[promedio_tiempos$`df$Data.Arquitectura` == "APU", "df$Data.Tiempo"])

