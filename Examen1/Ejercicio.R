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
Algoritmo      Ejecucion    Puntaje
'ERA'            '1'         45033
'ERA'            '2'         46623
'ERA'            '3'         43845
'ERA'            '4'         48849
'ERA'            '5'         45471
'ERA'            '6'         47132
'ERA'            '7'         46175
'ERA'            '8'         44015
'ERA'            '9'         46189
'ERA'            '10'        48499
'ERA'            '11'        42445
'ERA'            '12'        49155
'ERA'            '13'        48019
'ERA'            '14'        49068
'ERA'            '15'        42040
'ERA'            '16'        42538
'ERA'            '17'        44734
'ERA'            '18'        49899
'ERA'            '19'        47471
'ERA'            '20'        42966
'ERA'            '21'        42895
'ERA'            '22'        49284
'ERA'            '23'        45463
'ERA'            '24'        48812
'ERA'            '25'        43817
'ERA'            '26'        42326
'ERA'            '27'        43323
'ERA'            '28'        43482
'ERA'            '29'        44474
'ERA'            '30'        48576
'ERA'            '31'        42984
'ERA'            '32'        42914
'ERA'            '33'        48492
'ERA'            '34'        44776
'ERA'            '35'        48997
'ERA'            '36'        42966
'ERA'            '37'        42632
'ERA'            '38'        48334
'ERA'            '39'        45233
'ERA'            '40'        43456
'ERA'            '41'        46802
'ERA'            '42'        45332
'ERA'            '43'        43422
'ERA'            '44'        46946
'ERA'            '45'        42401
'ERA'            '46'        43473
'ERA'            '47'        45527
'ERA'            '48'        42785
'ERA'            '49'        47040
'ERA'            '50'        46662
'ERA'            '51'        49270
'ERA'            '52'        45591
'ERA'            '53'        46501
'ERA'            '54'        48277
'ERA'            '55'        47178
'ERA'            '56'        47658
'ERA'            '57'        49259
'ERA'            '58'        46043
'ERA'            '59'        46578
'ERA'            '60'        45165
'CFS'            '1'         42906
'CFS'            '2'         43517
'CFS'            '3'         42032
'CFS'            '4'         41393
'CFS'            '5'         42820
'CFS'            '6'         44951
'CFS'            '7'         41741
'CFS'            '8'         45950
'CFS'            '9'         42535
'CFS'            '10'        44958
'CFS'            '11'        44690
'CFS'            '12'        40945
'CFS'            '13'        44157
'CFS'            '14'        44550
'CFS'            '15'        42781
'CFS'            '16'        43145
'CFS'            '17'        43578
'CFS'            '18'        44312
'CFS'            '19'        45834
'CFS'            '20'        44558
'CFS'            '21'        42529
'CFS'            '22'        44373
'CFS'            '23'        46034
'CFS'            '24'        42572
'CFS'            '25'        41411
'CFS'            '26'        45356
'CFS'            '27'        44186
'CFS'            '28'        43339
'CFS'            '29'        45815
'CFS'            '30'        43666
'CFS'            '31'        45324
'CFS'            '32'        45427
'CFS'            '33'        41425
'CFS'            '34'        43171
'CFS'            '35'        40805
'CFS'            '36'        41931
'CFS'            '37'        40793
'CFS'            '38'        41542
'CFS'            '39'        45018
'CFS'            '40'        41054
'CFS'            '41'        44277
'CFS'            '42'        45672
'CFS'            '43'        46409
'CFS'            '44'        43394
'CFS'            '45'        43966
'CFS'            '46'        46300
'CFS'            '47'        46196
'CFS'            '48'        42279
'CFS'            '49'        45212
'CFS'            '50'        43758
'CFS'            '51'        43222
'CFS'            '52'        41552
'CFS'            '53'        41354
'CFS'            '54'        45633
'CFS'            '55'        44274
'CFS'            '56'        41765
'CFS'            '57'        45777
'CFS'            '58'        45205
'CFS'            '59'        42317
'CFS'            '60'        41959
'ULE'            '1'         45512
'ULE'            '2'         42095
'ULE'            '3'         41297
'ULE'            '4'         43138
'ULE'            '5'         40823
'ULE'            '6'         43642
'ULE'            '7'         40638
'ULE'            '8'         44984
'ULE'            '9'         43633
'ULE'            '10'        42653
'ULE'            '11'        41374
'ULE'            '12'        41558
'ULE'            '13'        41849
'ULE'            '14'        45989
'ULE'            '15'        42002
'ULE'            '16'        44388
'ULE'            '17'        41622
'ULE'            '18'        42974
'ULE'            '19'        44685
'ULE'            '20'        44343
'ULE'            '21'        43707
'ULE'            '22'        45049
'ULE'            '23'        42358
'ULE'            '24'        40590
'ULE'            '25'        45059
'ULE'            '26'        40714
'ULE'            '27'        42034
'ULE'            '28'        45881
'ULE'            '29'        40350
'ULE'            '30'        41908
'ULE'            '31'        41146
'ULE'            '32'        40614
'ULE'            '33'        40719
'ULE'            '34'        40340
'ULE'            '35'        42265
'ULE'            '36'        45164
'ULE'            '37'        44624
'ULE'            '38'        45495
'ULE'            '39'        44174
'ULE'            '40'        40461
'ULE'            '41'        44851
'ULE'            '42'        45542
'ULE'            '43'        42928
'ULE'            '44'        45616
'ULE'            '45'        45940
'ULE'            '46'        45587
'ULE'            '47'        45573
'ULE'            '48'        41625
'ULE'            '49'        40204
'ULE'            '50'        43319
'ULE'            '51'        41055
'ULE'            '52'        40471
'ULE'            '53'        43741
'ULE'            '54'        43889
'ULE'            '55'        40335
'ULE'            '56'        40163
'ULE'            '57'        45193
'ULE'            '58'        40631
'ULE'            '59'        40973
'ULE'            '60'        42638
'Monotonic'      '1'         32490
'Monotonic'      '2'         39163
'Monotonic'      '3'         32927
'Monotonic'      '4'         38382
'Monotonic'      '5'         33590
'Monotonic'      '6'         39663
'Monotonic'      '7'         32283
'Monotonic'      '8'         32621
'Monotonic'      '9'         37613
'Monotonic'      '10'        37005
'Monotonic'      '11'        35527
'Monotonic'      '12'        39317
'Monotonic'      '13'        32426
'Monotonic'      '14'        36812
'Monotonic'      '15'        32478
'Monotonic'      '16'        35713
'Monotonic'      '17'        37565
'Monotonic'      '18'        32738
'Monotonic'      '19'        38524
'Monotonic'      '20'        33706
'Monotonic'      '21'        39618
'Monotonic'      '22'        34218
'Monotonic'      '23'        35823
'Monotonic'      '24'        35597
'Monotonic'      '25'        39642
'Monotonic'      '26'        33650
'Monotonic'      '27'        33173
'Monotonic'      '28'        33812
'Monotonic'      '29'        38799
'Monotonic'      '30'        36139
'Monotonic'      '31'        32847
'Monotonic'      '32'        39100
'Monotonic'      '33'        35042
'Monotonic'      '34'        38256
'Monotonic'      '35'        39075
'Monotonic'      '36'        36629
'Monotonic'      '37'        35159
'Monotonic'      '38'        38597
'Monotonic'      '39'        34461
'Monotonic'      '40'        35573
'Monotonic'      '41'        38843
'Monotonic'      '42'        34925
'Monotonic'      '43'        33918
'Monotonic'      '44'        33043
'Monotonic'      '45'        36867
'Monotonic'      '46'        33323
'Monotonic'      '47'        38749
'Monotonic'      '48'        39796
'Monotonic'      '49'        37803
'Monotonic'      '50'        38739
'Monotonic'      '51'        36559
'Monotonic'      '52'        38500
'Monotonic'      '53'        35794
'Monotonic'      '54'        39715
'Monotonic'      '55'        38674
'Monotonic'      '56'        35441
'Monotonic'      '57'        34091
'Monotonic'      '58'        32393
'Monotonic'      '59'        32975
'Monotonic'      '60'        38212
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

Summarize(Puntaje ~ Algoritmo, data = Data, digits = 4)

# 4. Diagrama de cajas

M <- tapply(Data$Puntaje, INDEX = Data$Algoritmo, FUN = mean)
boxplot(Puntaje ~ Algoritmo, data = Data)
points(M, col = "red", pch = "+", cex = 2)

# 5. Información de promedios e intervalos de confianza

Sum <- groupwiseMean(Puntaje ~ Algoritmo, data = Data, conf = 0.95, digits = 3, traditional = FALSE, percentile = TRUE)
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
                    ylab("Puntaje promedio, s")

#  Validacion de supuestos de ANOVA
#  Supuesto de normalidad y homocedasticidad

# 7. Modelo Lineal

model <- lm(Puntaje ~ Algoritmo, data = Data)
summary(model)

# 8. Histograma de residuos

X <- residuals(model)
library(rcompanion)
dev.new()
#  Para evitar error "figure margins too large"
windows.options(width = 10, height = 8)
plotNormalHistogram(X)
#  Los residuos son normales

# 9. Dispersión de residuos

plot(fitted(model),residuals(model))
#  La dispersion es la misma

# 10. Gráficos del modelo lineal

plot(model)

# Se cumplen los supuestos

# 11. ANOVA

library(car)
Anova(model, type = "II")

# P-Value < alpha -> Se rechaza H0

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

# Ordenamos los niveles para imprimirlos
CLD$Algoritmo <- factor(CLD$Algoritmo, levels = c("ERA", "CFS", "ULE", "Monotonic"))
# Removemos espacios en blanco
CLD$.group <- gsub(" ", "", CLD$.group)

# Era estadisticamente distinto a CFS, ULE y Monotonic
# CFS y ULE estadisticamente equivalentes.
# Monotonic estadisticamente distinto a CFS, ULE y Monotonic

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
