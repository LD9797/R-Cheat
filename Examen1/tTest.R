# t-Test

# 1. Carga de datos

# Librerias
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
if(!require(lsr)){install.packages("lsr")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Ingreso de los datos
Datos <- ("
Algoritmo     Ejecucion  Tiempo
'Algoritmo A'   '1'       12070
'Algoritmo A'   '2'       14040
'Algoritmo A'   '3'       13580
'Algoritmo A'   '4'        9540
'Algoritmo A'   '5'       14070
'Algoritmo A'   '6'       11520
'Algoritmo A'   '7'       13030
'Algoritmo A'   '8'       13245
'Algoritmo A'   '9'       14215
'Algoritmo A'   '10'      15070
'Algoritmo A'   '11'      12580
'Algoritmo A'   '12'      11540
'Algoritmo A'   '13'       9580
'Algoritmo A'   '14'      11510
'Algoritmo A'   '15'      16070
'Algoritmo A'   '16'      13010
'Algoritmo A'   '17'      10530
'Algoritmo A'   '18'      13030
'Algoritmo A'   '19'      17080
'Algoritmo A'   '20'      13020
'Algoritmo B'   '1'       11070
'Algoritmo B'   '2'       12010
'Algoritmo B'   '3'       12550
'Algoritmo B'   '4'       10500
'Algoritmo B'   '5'       12000
'Algoritmo B'   '6'       12520
'Algoritmo B'   '7'       13520
'Algoritmo B'   '8'       13540
'Algoritmo B'   '9'       13255
'Algoritmo B'   '10'      15235
'Algoritmo B'   '11'      12235
'Algoritmo B'   '12'      11285
'Algoritmo B'   '13'      10040
'Algoritmo B'   '14'      11295
'Algoritmo B'   '15'      14080
'Algoritmo B'   '16'      12080
'Algoritmo B'   '17'      11580
'Algoritmo B'   '18'      14070
'Algoritmo B'   '19'      15050
'Algoritmo B'   '20'      12050 ")

# 2. Lectura de datos

Data <- read.table(textConnection(Datos), header=TRUE)
rm(Datos)

library(psych)
headTail(Data) # Ordenar datos de mayor a menor

str(Data) # Desplegar de manera compacta
summary(Data) # La estructura del objecto - Verificar que los datos estén correctos

# 3. Resumen organizado

library(FSA)
Summarize(Tiempo ~ Algoritmo, data = Data, digits = 4)


# 4. Análisis de normalidad (histograma + curva normal) Muestras por aparte

# -- Se analiza la normalidad en los datos y se verifica si existe normalidad en los datos.
A <- Data$Tiempo[Data$Algoritmo == "Algoritmo A"]
B <- Data$Tiempo[Data$Algoritmo == "Algoritmo B"]

library(rcompanion)
plotNormalHistogram(A)
plotNormalHistogram(B)

# 5. Diagrama de cajas

M <- tapply(Data$Tiempo, INDEX = Data$Algoritmo, FUN = mean)
boxplot(Tiempo ~ Algoritmo, data = Data)
points(M, col = "red", pch = "+", cex = 2)

# 6. Prueba t

t.test(Tiempo ~ Algoritmo, data = Data)