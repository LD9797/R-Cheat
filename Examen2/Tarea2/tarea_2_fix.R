# Bibliotecas
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}

# 1. Carga de datos.
Data <- read.csv2("Datos_fixed_tarea_2_fix.csv", sep = ";", header = TRUE)
Data$Efectos <- as.factor(Data$Efectos)
Data$Objetos <- as.character(Data$Objetos)
Data$Objectos <- as.factor(Data$Objetos)
Data$Arquitectura <- as.factor(Data$Arquitectura)
Data$Resolucion <- as.factor(Data$Resolucion)
Data$Tiempo <- as.numeric(Data$Tiempo)

# 2. Verificación de la lectura de datos.
# Verificar que solo devuelva métricas para el tiempo.
# Si sale NA, factor no definido como tal.
library(psych)
headTail(Data)
str(Data)
summary(Data)

# 3. Gráficos simples de interacción
# Variable dependiente: Tiempo
# Variables independientes: Arquitectura y Objetos
interaction.plot(x.factor = as.numeric(Data$Objetos),
 trace.factor = Data$Arquitectura,
 response = Data$Tiempo,
 fun = mean,
 type = "b",
 col = c("black", "red", "green"),
 pch = c(19,17,15),
 fixed = TRUE,
 leg.bty = "o")
# Parece haber comportamiento exponencial.
# Pero el eje X dice cuantos objetos tenía la escena.
# Son potencias de 2.
# Se debe analizar si estamos forzando un comportamiento en alguno de los factores.
# APU se comporta mejor.

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
# Si hay interacción significativa entre objetos y efectos, los objetos
# impactan en cómo se comportan los efectos.

# 4. Evaluación de los supuestos.
# Función para graficos de los supuestos.
graficos_supuestos <- function(model) {
  par(mfrow = c(3, 1))
  x <- residuals(model)
  library(rcompanion)
  plotNormalHistogram(x)
  plot(fitted(model), residuals(model))
  qqnorm(resid(model), main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standarized residuals")
  qqline(resid(model), col = "red", lwd = 2)
  par(mfrow = c(1, 1))
}

# Datos iniciales originales.
model <- lm(Tiempo ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
graficos_supuestos(model)
leveneTest(Tiempo ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)

# 5. Tranformación por raíz cuadrada.
library(rcompanion)
T_sqrt <- sqrt(Data$Tiempo)
model <- lm(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
graficos_supuestos(model)
leveneTest(T_sqrt ~ Objetos * Arquitectura * Efectos * Resolucion, data = Data)
plot(model)

# 6. Anova
library(car)
Anova(model, type = "II")

# 7. Gráficos finales
# Arquitectura
Sum <- Summarize(T_sqrt ~ Arquitectura, data = Data, digits = 3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Arquitectura, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0),
            legend.position="none") +
      ylab(expression("Promerdio de la raíz cuadrada del tiempo")) +
      ggtitle("Tiempo vs Arquitectura")

# Destransformando
Sum <- Summarize(T_sqrt ~ Arquitectura, data = Data, digits = 3)
Sum$mean <- Sum$mean^2
Sum$sd <- Sum$sd^2
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Arquitectura, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0),
            legend.position="none") +
      ylab(expression("Tiempo promedio (s)")) +
      ggtitle("Tiempo vs Arquitectura")

# Arquitectura + Resolucion
Sum <- Summarize(T_sqrt ~ Arquitectura + Resolucion, data = Data, digits = 3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)

library(ggplot2)
pd <- position_dodge(.2)
 ggplot(Sum,aes(x=Resolucion, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Promerdio de la raíz cuadrada del tiempo")) +
      ggtitle("Tiempo vs Arquitectura")

# Destransformando
Sum <- Summarize(T_sqrt ~ Arquitectura + Resolucion, data = Data, digits = 3)
Sum$mean <- Sum$mean^2
Sum$sd <- Sum$sd^2
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)

library(ggplot2)
pd <- position_dodge(.2)
 ggplot(Sum,aes(x=Resolucion, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Tiempo promedio (s)")) +
      ggtitle("Tiempo vs Arquitectura")

# Arquitectura y efectos
Sum <- Summarize(T_sqrt ~ Arquitectura + Efectos, data = Data, digits = 3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum$Efectos <- factor(Sum$Efectos,
                      levels(Sum$Efectos)[c(8,7,6,5,4,3,2,1)])

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Efectos, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Promerdio de la raíz cuadrada del tiempo")) +
      ggtitle("Tiempo vs Arquitectura")

# Para salvar
# ggsave(plot = q, width = 14, height = 8, dpi = 300, filename = "arquitectura.png")
# Destransformando
Sum <- Summarize(T_sqrt ~ Arquitectura + Efectos, data = Data, digits = 3)
Sum$mean <- Sum$mean^2
Sum$sd <- Sum$sd^2
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)
Sum$Efectos <- factor(Sum$Efectos,
                      levels(Sum$Efectos)[c(8,7,6,5,4,3,2,1)])

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Efectos, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Tiempo promedio (s)")) +
      ggtitle("Tiempo vs Arquitectura")

# Arquitectura y Objetos
Sum <- Summarize(T_sqrt ~ Arquitectura + Objetos, data = Data, digits = 3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits = 3)

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Objetos, y=mean, color = Arquitectura)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=Arquitectura), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Promerdio de la raíz cuadrada del tiempo")) +
      ggtitle("Tiempo vs Arquitectura")

# Destransformando es la misma vara

# 8. Pairwise t-test
pairwise.t.test(T_sqrt, Data$Arquitectura, p.adjust.method = "BH")
pairwise.t.test(T_sqrt, Data$Arquitectura : Data$Resolucion, p.adjust.method = "BH")
pairwise.t.test(T_sqrt, Data$Arquitectura : Data$Efectos, p.adjust.method = "BH")
# Se pueden hacer análisis de todas las interacciones que se quieran.

# 9. Conclusión.
# 1. En la totalidad de experimentos, el APU se comportó mejor.
# 2. Se identificó que para escenarios con pocos objetos, no hay diferencia. En escenarios donde la cantidad de objetos
# aumenta significativamente, entre más objetos hallan mejor el APU con respecto a las otras dos. En escenarios simples
# no hay diferencia, pero en escenarios complejos si.
