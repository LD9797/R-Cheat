# Kruskal-Wallis no paramétrica.

# Librerías.
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(lattice)){install.packages("lattice")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(rcompanion)){install.packages("rcompanion")}


# 1. Carga inicial de datos.
ln <- ("
Algoritmo         Rendimiento
'Algoritmo A'         3
'Algoritmo A'         5
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         5
'Algoritmo A'         5
'Algoritmo A'         3
'Algoritmo A'         5
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         4
'Algoritmo A'         5
'Algoritmo A'         5
'Algoritmo B'         2
'Algoritmo B'         4
'Algoritmo B'         2
'Algoritmo B'         2
'Algoritmo B'         1
'Algoritmo B'         2
'Algoritmo B'         3
'Algoritmo B'         2
'Algoritmo B'         2
'Algoritmo B'         3
'Algoritmo B'         2
'Algoritmo B'         4
'Algoritmo B'         2
'Algoritmo B'         2
'Algoritmo B'         1
'Algoritmo B'         2
'Algoritmo B'         3
'Algoritmo B'         2
'Algoritmo B'         2
'Algoritmo B'         3
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         5
'Algoritmo C'         3
'Algoritmo C'         5
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         3
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         5
'Algoritmo C'         3
'Algoritmo C'         5
'Algoritmo C'         4
'Algoritmo C'         4
'Algoritmo C'         3")

# Se introduce la tabla.
Data <- read.table(textConnection(ln), header=TRUE)
# Ordenamos los factores y craemos una variable puntaje del rendimiento.
Data$Algoritmo <- factor(Data$Algoritmo, levels = unique(Data$Algoritmo))
Data$Rendimiento.f <- factor(Data$Rendimiento, ordered = TRUE) # Variable puntaje.

# 2. Verificación de la lectura de datos.
library(psych)
headTail(Data)
str(Data)
summary(Data)
rm(ln)

# 3. Resumimos la tabla.
xtabs(~ Algoritmo + Rendimiento.f, data = Data)

# Ponderación entre 0 a 1.
XT <- xtabs(~ Algoritmo + Rendimiento.f, data = Data)
prop.table(XT, margin = 1)

# 4. Gráfico de barras por grupo.
library(lattice)
histogram(~ Rendimiento.f | Algoritmo, data = Data, layout = c(1,3))

# 5. Resumen.
library(FSA)
Summarize(Rendimiento ~ Algoritmo, data =  Data, digits = 3)

# 6. Prueba Kruskal-Wallis.
kruskal.test(Rendimiento ~ Algoritmo, data = Data)

# 7. Análisis post-hoc.
Data$Algoritmo <- factor(Data$Algoritmo, levels = c("Algoritmo A",
                                                    "Algoritmo B",
                                                    "Algoritmo C"))
levels(Data$Algoritmo)

library(FSA)
DT <- dunnTest(Rendimiento ~ Algoritmo, data = Data, method = "bh")
DT

# 8. Despliege compacto con letras.
PT <- DT$res
PT
library(rcompanion)
cldList(P.adj ~ Comparison, data = PT, threshold = 0.05)

# 9. Gráfico de medianas en intervalos de confianza.
library(rcompanion)
Sum <- groupwiseMedian(Rendimiento ~ Algoritmo,
                       data = Data, conf = 0.95, R = 5000, percentile = TRUE,
                       bca = FALSE, digits = 3)
Sum

X <- 1:3
Y <- Sum$Percentile.upper + 0.2
Label <- c("a", "b", "a")
library(ggplot2)
ggplot(Sum, aes(x = Algoritmo, y = Median)) + geom_errorbar(aes(ymin = Percentile.lower,
                                                           ymax = Percentile.upper),
       width = 0.05, size = 0.5) +
       geom_point(shape = 15, size = 4) +
       theme_bw() +
       theme(axis.title = element_text(face = "bold")) +
       ylab("Mediana de puntaje de rendimiento") +
       annotate("text", x = X, y = Y, label = Label)
