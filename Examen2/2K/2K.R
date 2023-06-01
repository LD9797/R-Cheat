# Anova diseño multifactorial 2^k

# 1. Carga inicial de datos.

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}


ln <- ("
Algoritmo       Entrenamiento       Rendimiento   Acelerador
'Algoritmo A'        MT500           12000           NA-NA
'Algoritmo A'        MT500           14005           NA-NA
'Algoritmo A'        MT500           13508           NA-NA
'Algoritmo A'        MT500            9503           NA-NA
'Algoritmo A'        MT500           14004           NA-NA
'Algoritmo A'        MT1000          11502           NA-NA
'Algoritmo A'        MT1000          13006           NA-NA
'Algoritmo A'        MT1000          13252           NA-NA
'Algoritmo A'        MT1000          14253           NA-NA
'Algoritmo A'        MT1000          15003           NA-NA
'Algoritmo A'        MT5000          12504           NA-NA
'Algoritmo A'        MT5000          11504           NA-NA
'Algoritmo A'        MT5000           9500           NA-NA
'Algoritmo A'        MT5000          11506           NA-NA
'Algoritmo A'        MT5000          16000           NA-NA
'Algoritmo A'        MT50000         13008           NA-NA
'Algoritmo A'        MT50000         10506           NA-NA
'Algoritmo A'        MT50000         13005           NA-NA
'Algoritmo A'        MT50000         17002           NA-NA
'Algoritmo A'        MT50000         13008           NA-NA
'Algoritmo B'        MT500           11005           NA-NA
'Algoritmo B'        MT500           12007           NA-NA
'Algoritmo B'        MT500           12509           NA-NA
'Algoritmo B'        MT500           10504           NA-NA
'Algoritmo B'        MT500           12002           NA-NA
'Algoritmo B'        MT1000          12504           NA-NA
'Algoritmo B'        MT1000          13501           NA-NA
'Algoritmo B'        MT1000          13501           NA-NA
'Algoritmo B'        MT1000          13252           NA-NA
'Algoritmo B'        MT1000          15256           NA-NA
'Algoritmo B'        MT5000          12253           NA-NA
'Algoritmo B'        MT5000          11255           NA-NA
'Algoritmo B'        MT5000          10006           NA-NA
'Algoritmo B'        MT5000          11252           NA-NA
'Algoritmo B'        MT5000          14004           NA-NA
'Algoritmo B'        MT50000         12007           NA-NA
'Algoritmo B'        MT50000         11505           NA-NA
'Algoritmo B'        MT50000         14009           NA-NA
'Algoritmo B'        MT50000         15000           NA-NA
'Algoritmo B'        MT50000         12009           NA-NA
'Algoritmo C'        MT500            9000           NA-NA
'Algoritmo C'        MT500           11003           NA-NA
'Algoritmo C'        MT500           11505           NA-NA
'Algoritmo C'        MT500            9509           NA-NA
'Algoritmo C'        MT500           11003           NA-NA
'Algoritmo C'        MT1000          11508           NA-NA
'Algoritmo C'        MT1000          12508           NA-NA
'Algoritmo C'        MT1000          12506           NA-NA
'Algoritmo C'        MT1000          12254           NA-NA
'Algoritmo C'        MT1000          13253           NA-NA
'Algoritmo C'        MT5000          11255           NA-NA
'Algoritmo C'        MT5000          10257           NA-NA
'Algoritmo C'        MT5000           9500           NA-NA
'Algoritmo C'        MT5000           9255           NA-NA
'Algoritmo C'        MT5000          12009           NA-NA
'Algoritmo C'        MT50000         11000           NA-NA
'Algoritmo C'        MT50000          9509           NA-NA
'Algoritmo C'        MT50000         13009           NA-NA
'Algoritmo C'        MT50000         14005           NA-NA
'Algoritmo C'        MT50000         11001           NA-NA
'Algoritmo A'        MT500           12046           NA-SW
'Algoritmo A'        MT500           14589           NA-SW
'Algoritmo A'        MT500           13723           NA-SW
'Algoritmo A'        MT500            9799           NA-SW
'Algoritmo A'        MT500           14715           NA-SW
'Algoritmo A'        MT1000          11144           NA-SW
'Algoritmo A'        MT1000          13920           NA-SW
'Algoritmo A'        MT1000          13226           NA-SW
'Algoritmo A'        MT1000          14845           NA-SW
'Algoritmo A'        MT1000          15142           NA-SW
'Algoritmo A'        MT5000          12352           NA-SW
'Algoritmo A'        MT5000          11296           NA-SW
'Algoritmo A'        MT5000           9737           NA-SW
'Algoritmo A'        MT5000          11129           NA-SW
'Algoritmo A'        MT5000          16409           NA-SW
'Algoritmo A'        MT50000         13872           NA-SW
'Algoritmo A'        MT50000         10100           NA-SW
'Algoritmo A'        MT50000         13419           NA-SW
'Algoritmo A'        MT50000         17398           NA-SW
'Algoritmo A'        MT50000         13164           NA-SW
'Algoritmo B'        MT500           11047           NA-SW
'Algoritmo B'        MT500           12226           NA-SW
'Algoritmo B'        MT500           12105           NA-SW
'Algoritmo B'        MT500           10418           NA-SW
'Algoritmo B'        MT500           12446           NA-SW
'Algoritmo B'        MT1000          12156           NA-SW
'Algoritmo B'        MT1000          13968           NA-SW
'Algoritmo B'        MT1000          13891           NA-SW
'Algoritmo B'        MT1000          13778           NA-SW
'Algoritmo B'        MT1000          15448           NA-SW
'Algoritmo B'        MT5000          12441           NA-SW
'Algoritmo B'        MT5000          11767           NA-SW
'Algoritmo B'        MT5000          10340           NA-SW
'Algoritmo B'        MT5000          11306           NA-SW
'Algoritmo B'        MT5000          14565           NA-SW
'Algoritmo B'        MT50000         12725           NA-SW
'Algoritmo B'        MT50000         11169           NA-SW
'Algoritmo B'        MT50000         14749           NA-SW
'Algoritmo B'        MT50000         15566           NA-SW
'Algoritmo B'        MT50000         12239           NA-SW
'Algoritmo C'        MT500            9082           NA-SW
'Algoritmo C'        MT500           11887           NA-SW
'Algoritmo C'        MT500           11799           NA-SW
'Algoritmo C'        MT500            9300           NA-SW
'Algoritmo C'        MT500           11049           NA-SW
'Algoritmo C'        MT1000          11378           NA-SW
'Algoritmo C'        MT1000          12659           NA-SW
'Algoritmo C'        MT1000          12905           NA-SW
'Algoritmo C'        MT1000          12782           NA-SW
'Algoritmo C'        MT1000          13196           NA-SW
'Algoritmo C'        MT5000          11795           NA-SW
'Algoritmo C'        MT5000          10316           NA-SW
'Algoritmo C'        MT5000           9947           NA-SW
'Algoritmo C'        MT5000           9420           NA-SW
'Algoritmo C'        MT5000          12699           NA-SW
'Algoritmo C'        MT50000         11024           NA-SW
'Algoritmo C'        MT50000          9556           NA-SW
'Algoritmo C'        MT50000         13900           NA-SW
'Algoritmo C'        MT50000         14006           NA-SW
'Algoritmo C'        MT50000         11738           NA-SW
'Algoritmo A'        MT500           126572          HW-NA
'Algoritmo A'        MT500           140058          HW-NA
'Algoritmo A'        MT500           139580          HW-NA
'Algoritmo A'        MT500            92583          HW-NA
'Algoritmo A'        MT500           148057          HW-NA
'Algoritmo A'        MT1000          110078          HW-NA
'Algoritmo A'        MT1000          131942          HW-NA
'Algoritmo A'        MT1000          133797          HW-NA
'Algoritmo A'        MT1000          140026          HW-NA
'Algoritmo A'        MT1000          155479          HW-NA
'Algoritmo A'        MT5000          125809          HW-NA
'Algoritmo A'        MT5000          114264          HW-NA
'Algoritmo A'        MT5000           98797          HW-NA
'Algoritmo A'        MT5000          113400          HW-NA
'Algoritmo A'        MT5000          168898          HW-NA
'Algoritmo A'        MT50000         133452          HW-NA
'Algoritmo A'        MT50000         101641          HW-NA
'Algoritmo A'        MT50000         133155          HW-NA
'Algoritmo A'        MT50000         175156          HW-NA
'Algoritmo A'        MT50000         131945          HW-NA
'Algoritmo B'        MT500           110317          HW-NA
'Algoritmo B'        MT500           129244          HW-NA
'Algoritmo B'        MT500           127966          HW-NA
'Algoritmo B'        MT500           109783          HW-NA
'Algoritmo B'        MT500           122936          HW-NA
'Algoritmo B'        MT1000          128830          HW-NA
'Algoritmo B'        MT1000          134437          HW-NA
'Algoritmo B'        MT1000          138321          HW-NA
'Algoritmo B'        MT1000          132000          HW-NA
'Algoritmo B'        MT1000          157693          HW-NA
'Algoritmo B'        MT5000          121964          HW-NA
'Algoritmo B'        MT5000          119872          HW-NA
'Algoritmo B'        MT5000          106654          HW-NA
'Algoritmo B'        MT5000          112666          HW-NA
'Algoritmo B'        MT5000          145535          HW-NA
'Algoritmo B'        MT50000         127938          HW-NA
'Algoritmo B'        MT50000         115179          HW-NA
'Algoritmo B'        MT50000         143021          HW-NA
'Algoritmo B'        MT50000         150357          HW-NA
'Algoritmo B'        MT50000         121216          HW-NA
'Algoritmo C'        MT500            95474          HW-NA
'Algoritmo C'        MT500           113776          HW-NA
'Algoritmo C'        MT500           117473          HW-NA
'Algoritmo C'        MT500            92900          HW-NA
'Algoritmo C'        MT500           115582          HW-NA
'Algoritmo C'        MT1000          115279          HW-NA
'Algoritmo C'        MT1000          122184          HW-NA
'Algoritmo C'        MT1000          124770          HW-NA
'Algoritmo C'        MT1000          128403          HW-NA
'Algoritmo C'        MT1000          135219          HW-NA
'Algoritmo C'        MT5000          112562          HW-NA
'Algoritmo C'        MT5000          108736          HW-NA
'Algoritmo C'        MT5000           91064          HW-NA
'Algoritmo C'        MT5000           98171          HW-NA
'Algoritmo C'        MT5000          120277          HW-NA
'Algoritmo C'        MT50000         111299          HW-NA
'Algoritmo C'        MT50000          90193          HW-NA
'Algoritmo C'        MT50000         135178          HW-NA
'Algoritmo C'        MT50000         146158          HW-NA
'Algoritmo C'        MT50000         113845          HW-NA
'Algoritmo A'        MT500           124252          HW-SW
'Algoritmo A'        MT500           143833          HW-SW
'Algoritmo A'        MT500           138907          HW-SW
'Algoritmo A'        MT500            91010          HW-SW
'Algoritmo A'        MT500           143901          HW-SW
'Algoritmo A'        MT1000          116563          HW-SW
'Algoritmo A'        MT1000          136455          HW-SW
'Algoritmo A'        MT1000          130411          HW-SW
'Algoritmo A'        MT1000          140060          HW-SW
'Algoritmo A'        MT1000          154308          HW-SW
'Algoritmo A'        MT5000          124480          HW-SW
'Algoritmo A'        MT5000          111552          HW-SW
'Algoritmo A'        MT5000           99135          HW-SW
'Algoritmo A'        MT5000          110208          HW-SW
'Algoritmo A'        MT5000          167228          HW-SW
'Algoritmo A'        MT50000         134267          HW-SW
'Algoritmo A'        MT50000         102119          HW-SW
'Algoritmo A'        MT50000         138036          HW-SW
'Algoritmo A'        MT50000         171632          HW-SW
'Algoritmo A'        MT50000         130666          HW-SW
'Algoritmo B'        MT500           116942          HW-SW
'Algoritmo B'        MT500           129721          HW-SW
'Algoritmo B'        MT500           128834          HW-SW
'Algoritmo B'        MT500           100390          HW-SW
'Algoritmo B'        MT500           127771          HW-SW
'Algoritmo B'        MT1000          121789          HW-SW
'Algoritmo B'        MT1000          135311          HW-SW
'Algoritmo B'        MT1000          136587          HW-SW
'Algoritmo B'        MT1000          139664          HW-SW
'Algoritmo B'        MT1000          151543          HW-SW
'Algoritmo B'        MT5000          128962          HW-SW
'Algoritmo B'        MT5000          110157          HW-SW
'Algoritmo B'        MT5000          106129          HW-SW
'Algoritmo B'        MT5000          114634          HW-SW
'Algoritmo B'        MT5000          143337          HW-SW
'Algoritmo B'        MT50000         129292          HW-SW
'Algoritmo B'        MT50000         117502          HW-SW
'Algoritmo B'        MT50000         143687          HW-SW
'Algoritmo B'        MT50000         153488          HW-SW
'Algoritmo B'        MT50000         129773          HW-SW
'Algoritmo C'        MT500            99920          HW-SW
'Algoritmo C'        MT500           110833          HW-SW
'Algoritmo C'        MT500           117879          HW-SW
'Algoritmo C'        MT500            96441          HW-SW
'Algoritmo C'        MT500           119688          HW-SW
'Algoritmo C'        MT1000          117995          HW-SW
'Algoritmo C'        MT1000          122984          HW-SW
'Algoritmo C'        MT1000          120317          HW-SW
'Algoritmo C'        MT1000          120213          HW-SW
'Algoritmo C'        MT1000          137806          HW-SW
'Algoritmo C'        MT5000          117014          HW-SW
'Algoritmo C'        MT5000          105529          HW-SW
'Algoritmo C'        MT5000           98755          HW-SW
'Algoritmo C'        MT5000           96010          HW-SW
'Algoritmo C'        MT5000          126548          HW-SW
'Algoritmo C'        MT50000         113527          HW-SW
'Algoritmo C'        MT50000          99385          HW-SW
'Algoritmo C'        MT50000         136573          HW-SW
'Algoritmo C'        MT50000         141965          HW-SW
'Algoritmo C'        MT50000         111994          HW-SW
")


# Se introduce la tabla.
Data <- read.table(textConnection(ln), header=TRUE)

# Se ordenan los datos según los ingresamos. (Evitar orden alfabético por R).
Data$Entrenamiento <- factor(Data$Entrenamiento, levels = unique(Data$Entrenamiento))
Data$Acelerador <- factor(Data$Acelerador, levels = unique(Data$Acelerador))
Data$Algoritmo <- factor(Data$Algoritmo, levels = unique(Data$Algoritmo))

# 2. Verificación de la lectura de datos.

library(psych)
headTail(Data)
str(Data)
summary(Data)
rm(ln)

# 3. Gráfico simple de interacción.

# Variable dependiente: Rendimiento
# Variables independientes: Algoritmo y Método de Entrenamiento.
interaction.plot(x.factor = Data$Entrenamiento,
 trace.factor = Data$Algoritmo,
 response = Data$Rendimiento,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")


# 4. Se realiza cambio al gráfico para agregar el acelerador.
# Algoritmo en función del acelerador para ver el rendimiento.

interaction.plot(x.factor = Data$Acelerador,
                 trace.factor = Data$Algoritmo,
                 response = Data$Rendimiento,
                 fun = mean,
                 type = "b",
                 col = c("black", "red", "green"),
                 pch = c(19, 17, 15),
                 fixed = TRUE,
                 leg.bty = "o")

# 5. Modelo lineal y anova
# * Analisis de factores e interaccinoes de los factores.
model <- lm(Rendimiento ~ Entrenamiento * Algoritmo * Acelerador, data = Data)
library(car)
Anova(model, type = "II")

# Hay diferencias entre los grupos de entrenamiento, los algoritmos y el acelerador.
# Las factores impactan la variable de  respuesta.
# Las interacciones no impactan las variables de respuesta | Algoritmo:Acelerador

# 6. Evaluacion de los supuestos

x <- residuals(model)
library(rcompanion)
plotNormalHistogram(x)
plot(fitted(model), residuals(model))
plot(model)

# Parece haber normalidad, no hay homocedasticidad.

# Se procede a hacer transformación iniciando desde la forma menos agresiva a la mas agresiva, hasta cumplir los supuestos.

# 7. Transformación por raiz cuadrada

library(rcompanion)
T_sqrt <- sqrt(Data$Rendimiento) # Ingresar variable dependiente.
model <- lm(T_sqrt ~ Entrenamiento * Algoritmo * Acelerador, data = Data)

library(car)
Anova(model, type = "II")

# Supuestos

x <- residuals(model)
library(rcompanion)
plotNormalHistogram(x)
plot(fitted(model), residuals(model))
plot(model)

# Estamos llegando a conclusiones no al rendimiento, sino a la raiz cuadrada del rendimiento, porque eso es válido ?


# 8. Transformación por raíz cúbica

library(rcompanion)
 T_cub <- sign(Data$Rendimiento) * abs(Data$Rendimiento)^(1/3) # Ingresar variable dependiente.
model <- lm(T_cub ~ Entrenamiento * Algoritmo * Acelerador, data = Data)

library(car)
Anova(model, type = "II")

# Supuestos

x <- residuals(model)
library(rcompanion)
plotNormalHistogram(x)
plot(fitted(model), residuals(model))
plot(model)


# 9. Transformación por logaritmo

library(rcompanion)
T_log <- log(Data$Rendimiento)

model <- lm(T_log ~ Entrenamiento * Algoritmo * Acelerador, data = Data)
library(car)
Anova(model, type = "II")

x <- residuals(model)
library(rcompanion)

plotNormalHistogram(x)
plot(fitted(model), residuals(model))
plot(model)

# 10. Prueba Levene

leveneTest(T_log ~ Entrenamiento * Algoritmo * Acelerador, data = Data)

# 11. Analisis post-hoc por algoritmo

library(lsmeans)
marginal <- lsmeans(model, pairwise ~ Algoritmo, adjust="tukey")

library(multcomp)
CLD <- cld(marginal, alpha = 0.05, Letters = letters, adjust = "tukey")
CLD

# 12. Analisis post-hoc por entrenamiento

library(lsmeans)
marginal <- lsmeans(model, pairwise ~ Entrenamiento, adjust="tukey")

library(multcomp)
CLD <- cld(marginal, alpha = 0.05, Letters = letters, adjunst = "tukey")
CLD

# 13. Análisis post-hoc para acelerador 2^k

library(lsmeans)
marginal <- lsmeans(model, pairwise ~ Acelerador, adjust="tukey")

library(multcomp)
CLD <- cld(marginal, alpha = 0.05, Letters = letters, adjunst = "tukey")
CLD

# 14. Gráficos finales.

library(FSA)
Sum <- Summarize(T_log ~ Entrenamiento + Algoritmo, data = Data, digits = 3)

# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum

### Ordenamos

Sum$Entrenamiento <- factor(Sum$Entrenamiento, levels = unique(Sum$Entrenamiento))

### Graficamos

library(ggplot2)
pd <- position_dodge(.2)

ggplot(Sum, aes(x = Entrenamiento,
                y = mean, color = Algoritmo)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, size = 0.7, position = pd) +
    geom_point(shape = 15, size = 4, position = pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    scale_colour_manual(values = c("black", "red", "green")) +
    ylab("Logaritmo de rendimiento")


# Para acelerador

Sum <- Summarize(T_log ~ Acelerador + Algoritmo, data = Data, digits = 3)

# Se agrega el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum

### Ordenamos | Correccion: se cambia entrenamiento por acelerador
Sum$Acelerador <- factor(Sum$Acelerador, levels = unique(Sum$Acelerador))

### Graficamos

ggplot(Sum, aes(x = Acelerador,
                y = mean, color = Algoritmo)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, size = 0.7, position = pd) +
    geom_point(shape = 15, size = 4, position = pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    scale_colour_manual(values = c("black", "red", "green")) +
    ylab("Logaritmo de rendimiento")

# Bigotes pequeños por ser dato transformados


# 15. Gráfico de promedios transformados

### Creamos un dato llamado sum con promedios y se
library(FSA)
Sum <- Summarize(T_log ~ Algoritmo, data = Data, digits = 3)

### Agregamos el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum

### Ordenamos | Correccion: se cambia entrenamiento por algoritmo
Sum$Algoritmo <- factor(Sum$Algoritmo, levels = unique(Sum$Algoritmo))

### Graficamos
library(ggplot2)
pd <- position_dodge(.2)

# Correccion: se cambia entrenamiento por algoritmo
ggplot(Sum, aes(x = Algoritmo,
                y = mean, color = Algoritmo)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, size = 0.7, position = pd) +
    geom_point(shape = 15, size = 4, position = pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    scale_colour_manual(values = c("black", "red", "green")) +
    ylab("Logaritmo de rendimiento")

# 16. Des-transformando promedios

library(FSA)
# Corrección, no se agrega Entrenamiento. El grafico final no seria el mismo en la presentacion.
Sum <- Summarize(T_log ~ Algoritmo, data = Data, digits = 3)

Sum$mean <- exp(Sum$mean)
Sum$sd <- exp(Sum$sd)

### Agregamos el se
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum

ggplot(Sum, aes(x = Algoritmo,
                y = mean, color = Algoritmo)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, size = 0.7, position = pd) +
    geom_point(shape = 15, size = 4, position = pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    scale_colour_manual(values = c("black", "red", "green")) +
    ylab("Logaritmo de rendimiento")
