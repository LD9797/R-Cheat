# ANOVA Monofactorial con bloques.

# 1. Carga inicial de datos.

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}


ln <- ("
Algoritmo       Computadora       Tiempo
'Algoritmo A'  'Computadora 1'    12976
'Algoritmo A'  'Computadora 1'    14854
'Algoritmo A'  'Computadora 1'    13627
'Algoritmo A'  'Computadora 1'     9850
'Algoritmo A'  'Computadora 1'    14466
'Algoritmo A'  'Computadora 1'    11598
'Algoritmo A'  'Computadora 1'    13184
'Algoritmo A'  'Computadora 1'    13096
'Algoritmo A'  'Computadora 1'    14895
'Algoritmo A'  'Computadora 1'    15986
'Algoritmo A'  'Computadora 1'    12327
'Algoritmo A'  'Computadora 1'    11168
'Algoritmo A'  'Computadora 1'     9913
'Algoritmo A'  'Computadora 1'    11698
'Algoritmo A'  'Computadora 1'    16033
'Algoritmo A'  'Computadora 1'    13763
'Algoritmo A'  'Computadora 1'    10237
'Algoritmo A'  'Computadora 1'    13208
'Algoritmo A'  'Computadora 1'    15407
'Algoritmo A'  'Computadora 1'    13587
'Algoritmo A'  'Computadora 2'     9033
'Algoritmo A'  'Computadora 2'    11253
'Algoritmo A'  'Computadora 2'    11842
'Algoritmo A'  'Computadora 2'     9018
'Algoritmo A'  'Computadora 2'    11091
'Algoritmo A'  'Computadora 2'    11143
'Algoritmo A'  'Computadora 2'    12429
'Algoritmo A'  'Computadora 2'    12456
'Algoritmo A'  'Computadora 2'    12250
'Algoritmo A'  'Computadora 2'    13449
'Algoritmo A'  'Computadora 2'    11872
'Algoritmo A'  'Computadora 2'    10463
'Algoritmo A'  'Computadora 2'     9311
'Algoritmo A'  'Computadora 2'     9677
'Algoritmo A'  'Computadora 2'    12941
'Algoritmo A'  'Computadora 2'    11260
'Algoritmo A'  'Computadora 2'     9269
'Algoritmo A'  'Computadora 2'    13926
'Algoritmo A'  'Computadora 2'    14670
'Algoritmo A'  'Computadora 2'    11988
'Algoritmo B'  'Computadora 1'    11080
'Algoritmo B'  'Computadora 1'    12089
'Algoritmo B'  'Computadora 1'    12538
'Algoritmo B'  'Computadora 1'    10571
'Algoritmo B'  'Computadora 1'    12010
'Algoritmo B'  'Computadora 1'    12598
'Algoritmo B'  'Computadora 1'    13543
'Algoritmo B'  'Computadora 1'    13547
'Algoritmo B'  'Computadora 1'    13217
'Algoritmo B'  'Computadora 1'    15297
'Algoritmo B'  'Computadora 1'    12210
'Algoritmo B'  'Computadora 1'    11299
'Algoritmo B'  'Computadora 1'    10067
'Algoritmo B'  'Computadora 1'    11279
'Algoritmo B'  'Computadora 1'    14006
'Algoritmo B'  'Computadora 1'    12099
'Algoritmo B'  'Computadora 1'    11581
'Algoritmo B'  'Computadora 1'    14012
'Algoritmo B'  'Computadora 1'    15069
'Algoritmo B'  'Computadora 1'    12000
'Algoritmo B'  'Computadora 2'    12000
'Algoritmo B'  'Computadora 2'    14011
'Algoritmo B'  'Computadora 2'    13508
'Algoritmo B'  'Computadora 2'     9506
'Algoritmo B'  'Computadora 2'    14005
'Algoritmo B'  'Computadora 2'    11514
'Algoritmo B'  'Computadora 2'    13001
'Algoritmo B'  'Computadora 2'    13220
'Algoritmo B'  'Computadora 2'    14211
'Algoritmo B'  'Computadora 2'    15016
'Algoritmo B'  'Computadora 2'    12504
'Algoritmo B'  'Computadora 2'    11501
'Algoritmo B'  'Computadora 2'     9506
'Algoritmo B'  'Computadora 2'    11514
'Algoritmo B'  'Computadora 2'    16005
'Algoritmo B'  'Computadora 2'    13018
'Algoritmo B'  'Computadora 2'    10503
'Algoritmo B'  'Computadora 2'    13015
'Algoritmo B'  'Computadora 2'    17000
'Algoritmo B'  'Computadora 2'    13020
'Algoritmo C'  'Computadora 1'     9148
'Algoritmo C'  'Computadora 1'    11247
'Algoritmo C'  'Computadora 1'    11571
'Algoritmo C'  'Computadora 1'     9212
'Algoritmo C'  'Computadora 1'    11355
'Algoritmo C'  'Computadora 1'    11848
'Algoritmo C'  'Computadora 1'    12171
'Algoritmo C'  'Computadora 1'    12360
'Algoritmo C'  'Computadora 1'    12053
'Algoritmo C'  'Computadora 1'    13219
'Algoritmo C'  'Computadora 1'    11642
'Algoritmo C'  'Computadora 1'    10918
'Algoritmo C'  'Computadora 1'     9223
'Algoritmo C'  'Computadora 1'     9574
'Algoritmo C'  'Computadora 1'    12245
'Algoritmo C'  'Computadora 1'    11781
'Algoritmo C'  'Computadora 1'     9588
'Algoritmo C'  'Computadora 1'    13093
'Algoritmo C'  'Computadora 1'    14155
'Algoritmo C'  'Computadora 1'    11309
'Algoritmo C'  'Computadora 2'    12511
'Algoritmo C'  'Computadora 2'    14375
'Algoritmo C'  'Computadora 2'    13546
'Algoritmo C'  'Computadora 2'     9962
'Algoritmo C'  'Computadora 2'    14273
'Algoritmo C'  'Computadora 2'    11515
'Algoritmo C'  'Computadora 2'    13556
'Algoritmo C'  'Computadora 2'    13121
'Algoritmo C'  'Computadora 2'    14205
'Algoritmo C'  'Computadora 2'    15424
'Algoritmo C'  'Computadora 2'    12778
'Algoritmo C'  'Computadora 2'    11096
'Algoritmo C'  'Computadora 2'     9364
'Algoritmo C'  'Computadora 2'    11521
'Algoritmo C'  'Computadora 2'    16367
'Algoritmo C'  'Computadora 2'    13060
'Algoritmo C'  'Computadora 2'    10991
'Algoritmo C'  'Computadora 2'    13048
'Algoritmo C'  'Computadora 2'    15078
'Algoritmo C'  'Computadora 2'    13443"
)

# Se introduce la tabla.
Data <- read.table(textConnection(ln), header=TRUE)

# Se ordenan los datos según los ingresamos. (Evitar orden alfabético por R).
Data$Algoritmo <- factor(Data$Algoritmo, levels = unique(Data$Algoritmo))
Data$Computadora <- factor(Data$Computadora, levels = unique(Data$Computadora))

# 2. Verificación de la lectura de datos.

library(psych)
headTail(Data)
str(Data)
summary(Data)
rm(ln)

# 3. Resumen organizado.

# Se agrega ": Computadora" para que la tabla aparezca como en clase.
Summarize(Tiempo ~ Algoritmo : Computadora, data = Data, digits = 3)

# 4. Diagrama de cajas

M <- tapply(Data$Tiempo, INDEX = Data$Algoritmo, FUN = mean)
boxplot(Tiempo ~ Algoritmo, data = Data)
points(M, col = "red", pch = "+", cex = 2)

boxplot(Tiempo ~ Algoritmo : Computadora, data = Data)

# 5. Información de promedios e intervalos de confianza.

Sum <- groupwiseMean(Tiempo ~ Algoritmo, data = Data, conf = 0.95, digits = 3, traditional = FALSE, percentile = TRUE)
Sum

# 6. Gráficos de promedios e intervalos de confianza.

library(ggplot2)
ggplot(Sum,
       aes(x = Algoritmo, y = Mean)) +
       geom_errorbar(aes(ymin = Percentile.lower,
                         ymax = Percentile.upper),
                         width = 0.05, size = 0.5) +
                    geom_point(shape = 15,
                               size = 4) +
                    theme_bw() +
                    theme(axis.title = element_text(face = "bold")) +
                    ylab("Tiempo promedio, s")

# 6.1 Información de promedios e intervalos de confianza, cambio para considerar la computadora.

Sum <- groupwiseMean(Tiempo ~ Algoritmo : Computadora,
                     data = Data, conf = 0.95,
                     digits = 3, traditional = FALSE,
                     percentile = TRUE)
Sum

ggplot(Sum,
       aes(x = Algoritmo, y = Mean)) +
       geom_errorbar(aes(ymin = Percentile.lower,
                         ymax = Percentile.upper),
                         width = 0.05, size = 0.5) +
                    geom_point(shape = 15,
                               size = 4) +
                    theme_bw() +
                    theme(axis.title = element_text(face = "bold")) +
                    ylab("Tiempo promedio, s")

# 7. Modelo lineal.

model <- lm(Tiempo ~ Algoritmo : Computadora, data = Data)
summary(model)

# El p-value en este caso es bajo 0.002001, lo que sugiere los factores impactan la variable de respuesta.

# 8. ANOVA.

library(car)
Anova(model, type = "II")

# La prueba ANOVA pasa con un nivel de significancia de 0.01. Se procede a realizar la prueba post-hoc.

# 9. Histograma de residuos.

x <- residuals(model)
library(rcompanion)
plotNormalHistogram(x)
plot(fitted(model), residuals(model))
plot(model)

# Se cumple la normalidad, pero hay un ligero patrón en el gráfico donde se evalua la homocedasticidad.

# 10. Análisis post-hoc

library(multcompView)
library(lsmeans)
marginal <- lsmeans(model, ~ Algoritmo : Computadora)
pairs(marginal, adjust="tukey")

# Funcion cld

library(multcomp)
CLD <- cld(marginal, alpha = 0.05, Letters = letters, adjust = "tukey")
CLD

# Gráfico promedios, intervalos de confianza y letras de separación
CLD$Algoritmo <- factor(CLD$Algoritmo, levels = c("Algoritmo A", "Algoritmo B", "Algoritmo C"))
CLD$.group <- gsub(" ", "", CLD$.group)

library(ggplot2)
svg("final-ic-3.svg")
ggplot(CLD,
       aes( x = Algoritmo,
            y = lsmean,
            label = .group)) +
       geom_point(shape = 15, size = 4) +
       geom_errorbar(aes(ymin = lower.CL,
                         ymax = upper.CL),
                         width = 0.2,
                         size = 0.7) +
       theme_bw() +
       theme(axis.title = element_text(face = "bold"),
             axis.text = element_text(face = "bold"),
             plot.caption = element_text(hjust = 0)) +

       ylab("Promedio del minimo cuadrado \n
             Tiempo de ejecucion") +

       geom_text(nudge_x = c(0,0,0),
                 nudge_y = c(1100, 1100, 1100),
                 color = "black")
dev.off()

# La prueba de mínimos cuadrados al ser más estricta y en donde se
# reduce la distancia en comparaciones, indica que los grupos
# no son significativamente distintos, esto contrasta con la respuesta
# del ANOVA.

# Sin embargo si se agrega en ": Computadora" en la función lsmeans, este sería el resultado:
#
