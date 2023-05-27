# Analisis Anova multifactorial

# 1. Carga inicial de datos.

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}


ln <- ("Algoritmo       Entrenamiento       Rendimiento
'Algoritmo A'        MT500           12000
'Algoritmo A'        MT500           14005
'Algoritmo A'        MT500           13508
'Algoritmo A'        MT500            9503
'Algoritmo A'        MT500           14004
'Algoritmo A'        MT1000           11502
'Algoritmo A'        MT1000           13006
'Algoritmo A'        MT1000           13252
'Algoritmo A'        MT1000           14253
'Algoritmo A'        MT1000           15003
'Algoritmo A'        MT5000          12504
'Algoritmo A'        MT5000          11504
'Algoritmo A'        MT5000           9500
'Algoritmo A'        MT5000          11506
'Algoritmo A'        MT5000          16000
'Algoritmo A'        MT50000          13008
'Algoritmo A'        MT50000          10506
'Algoritmo A'        MT50000          13005
'Algoritmo A'        MT50000          17002
'Algoritmo A'        MT50000          13008
'Algoritmo B'        MT500            11005
'Algoritmo B'        MT500            12007
'Algoritmo B'        MT500            12509
'Algoritmo B'        MT500            10504
'Algoritmo B'        MT500            12002
'Algoritmo B'        MT1000           12504
'Algoritmo B'        MT1000           13501
'Algoritmo B'        MT1000           13501
'Algoritmo B'        MT1000           13252
'Algoritmo B'        MT1000           15256
'Algoritmo B'        MT5000           12253
'Algoritmo B'        MT5000           11255
'Algoritmo B'        MT5000           10006
'Algoritmo B'        MT5000           11252
'Algoritmo B'        MT5000           14004
'Algoritmo B'        MT50000          12007
'Algoritmo B'        MT50000          11505
'Algoritmo B'        MT50000          14009
'Algoritmo B'        MT50000          15000
'Algoritmo B'        MT50000          12009
'Algoritmo C'        MT500             9000
'Algoritmo C'        MT500            11003
'Algoritmo C'        MT500            11505
'Algoritmo C'        MT500             9509
'Algoritmo C'        MT500            11003
'Algoritmo C'        MT1000           11508
'Algoritmo C'        MT1000           12508
'Algoritmo C'        MT1000           12506
'Algoritmo C'        MT1000           12254
'Algoritmo C'        MT1000           13253
'Algoritmo C'        MT5000           11255
'Algoritmo C'        MT5000           10257
'Algoritmo C'        MT5000            9500
'Algoritmo C'        MT5000            9255
'Algoritmo C'        MT5000           12009
'Algoritmo C'        MT50000          11000
'Algoritmo C'        MT50000           9509
'Algoritmo C'        MT50000          13009
'Algoritmo C'        MT50000          14005
'Algoritmo C'        MT50000          11001
")

# Se introduce la tabla.
Data <- read.table(textConnection(ln), header = TRUE)

# Se ordenan los datos según los ingresamos. (Evitar orden alfabético por R).
Data$Entrenamiento <- factor(Data$Entrenamiento, levels=unique(Data$Entrenamiento))

# 2. Verificación de la lectura de datos

library(psych)
headTail(Data)
str(Data)
summary(Data)
rm(ln)


# 3. Gráfico simple de interacción.

interaction.plot(x.factor = Data$Entrenamiento,
 trace.factor = Data$Algoritmo,
 response = Data$Rendimiento,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")

# 4. Modelo lineal y ANOVA

model <- lm(Rendimiento ~ Entrenamiento + Algoritmo + Entrenamiento : Algoritmo, data = Data)

library(car)
Anova(model, type = "II")

# 5. Evaluación de supuestos

# Normalidad
x <- residuals(model)
library(rcompanion)
plotNormalHistogram(x)

# Disperción de los residuos
plot(fitted(model), residuals(model))

# Graficos del modelo lineal
plot(model)

# 6. Análisis post-hoc

library(lsmeans)
marginal <- lsmeans(model, pairwise ~ Algoritmo, adjust = "tukey")
marginal

# Funcion cld
library(multcomp)
CLD <- cld(marginal, alpha=0.05, Letters= letters, adjust="tukey")
CLD

# Análisis post-hoc entrenamiento
marginal <- lsmeans(model, pairwise ~ Entrenamiento, adjust = "tukey")
marginal

# Funcion cld
library(multcomp)
CLD <- cld(marginal, alpha=0.05, Letters= letters, adjust="tukey")
CLD


# 7. Gráfico final
library(FSA)

Sum <- Summarize(Rendimiento ~ Entrenamiento + Algoritmo, data = Data, digits = 3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum
Sum$Entrenamiento <- factor(Sum$Entrenamiento,
 levels = unique(Sum$Entrenamiento))

# 8. Boxplot error estándar

library(FSA)
library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum, aes(x=Entrenamiento,
 y = mean,
 color = Algoritmo)) +
 geom_errorbar(aes(ymin=mean-se,
 ymax=mean + se),
 width=.2, size=0.7, position=pd) +
 geom_point(shape=15, size=4, position = pd) +
 theme_bw() +
 theme(axis.title = element_text(face="bold")) +
 scale_colour_manual(values = c("black", "red", "green")) +
 ylab("Rendimiento")