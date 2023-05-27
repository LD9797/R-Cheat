# ANOVA Monofactorial

# 1. Carga inicial de datos:

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(multcompView)){install.packages("multcomp")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}


Datos <- ("
Algoritmo       Ejecucion  Tiempo
'Algoritmo A'      '1'    12060
'Algoritmo A'      '2'    14089
'Algoritmo A'      '3'    13502
'Algoritmo A'      '4'     9574
'Algoritmo A'      '5'    14056
'Algoritmo A'      '6'    11569
'Algoritmo A'      '7'    13047
'Algoritmo A'      '8'    13275
'Algoritmo A'      '9'    14257
'Algoritmo A'      '10'   15075
'Algoritmo A'      '11'   12506
'Algoritmo A'      '12'   11557
'Algoritmo A'      '13'    9548
'Algoritmo A'      '14'   11514
'Algoritmo A'      '15'   16015
'Algoritmo A'      '16'   13004
'Algoritmo A'      '17'   10510
'Algoritmo A'      '18'   13040
'Algoritmo A'      '19'   17098
'Algoritmo A'      '20'   13080
'Algoritmo B'      '1'    11080
'Algoritmo B'      '2'    12089
'Algoritmo B'      '3'    12538
'Algoritmo B'      '4'    10571
'Algoritmo B'      '5'    12010
'Algoritmo B'      '6'    12598
'Algoritmo B'      '7'    13543
'Algoritmo B'      '8'    13547
'Algoritmo B'      '9'    13217
'Algoritmo B'      '10'   15297
'Algoritmo B'      '11'   12210
'Algoritmo B'      '12'   11299
'Algoritmo B'      '13'   10067
'Algoritmo B'      '14'   11279
'Algoritmo B'      '15'   14006
'Algoritmo B'      '16'   12099
'Algoritmo B'      '17'   11581
'Algoritmo B'      '18'   14012
'Algoritmo B'      '19'   15069
'Algoritmo B'      '20'   12000
'Algoritmo C'      '1'     9081
'Algoritmo C'      '2'    11012
'Algoritmo C'      '3'    11529
'Algoritmo C'      '4'     9569
'Algoritmo C'      '5'    11092
'Algoritmo C'      '6'    11524
'Algoritmo C'      '7'    12522
'Algoritmo C'      '8'    12588
'Algoritmo C'      '9'    12241
'Algoritmo C'      '10'   13257
'Algoritmo C'      '11'   11294
'Algoritmo C'      '12'   10226
'Algoritmo C'      '13'    9591
'Algoritmo C'      '14'    9224
'Algoritmo C'      '15'   12033
'Algoritmo C'      '16'   11063
'Algoritmo C'      '17'    9537
'Algoritmo C'      '18'   13014
'Algoritmo C'      '19'   14033
'Algoritmo C'      '20'   11093
")

# Lectura de los datos
Data <- read.table(textConnection(Datos), header=TRUE)
# Ordenar los datos segun los ingresamos
Data$Algoritmo <- factor(Data$Algoritmo, levels = unique(Data$Algoritmo))


# 2. Lectura de datos / Verificación de lectura

library(psych)
headTail(Data)
str(Data)
summary(Data)
rm(Datos)

# 3. Resumen organizado

Summarize(Tiempo ~ Algoritmo, data = Data, digits = 4)

# 4. Diagrama de cajas

M <- tapply(Data$Tiempo, INDEX = Data$Algoritmo, FUN = mean)
boxplot(Tiempo ~ Algoritmo, data = Data)
points(M, col = "red", pch = "+", cex = 2)

# 5. Información de promedios e intervalos de confianza

Sum <- groupwiseMean(Tiempo ~ Algoritmo, data = Data, conf = 0.95, digits = 3, traditional = FALSE, percentile = TRUE)
Sum

# 6. Gráficos de promedios e intervalos de confianza

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

# 7. Modelo Lineal

model <- lm(Tiempo ~ Algoritmo, data = Data)
summary(model)

# 8. ANOVA

library(car)
Anova(model, type = "II")

# 9. Histograma de residuos

X <- residuals(model)
library(rcompanion)
plotNormalHistogram(X)

# 10. Dispersión de residuos

plot(fitted(model),residuals(model))

# 11. Gráficos del modelo lineal

plot(model)

# ------

# Ajuste de promedios | Mínimos cuadrados | Post-Hoc

# 1. Separación de promedios

library(multcompView)
library(lsmeans)
marginal <- lsmeans(model, ~ Algoritmo)
pairs(marginal, adjust="tukey")

# 2. Visión compacta

library(multcomp)
CLD <- cld(marginal, alpha=0.05, Letters = letters, adjust = "tukey")
CLD

# 3. Gráfico promedios, intervalos de confianza y letras de separación

# Ordenamos los niveles para imprimirlos
CLD$Algoritmo <- factor(CLD$Algoritmo, levels = c("Algoritmo A", "Algoritmo B", "Algoritmo C"))
# Removemos espacios en blanco
CLD$.group <- gsub(" ", "", CLD$.group)

library(ggplot2)
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

# Salvar gráficos

#svg("cajas1.svg")
#def.off()