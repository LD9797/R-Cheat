# 1. Carga inicial de datos.
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(rcompanion)){install.packages("rcompanion")}

# Lectura de datos
Data <- read.csv2("updated_file.csv", sep = ",", header = TRUE)


Data$Time <- as.numeric(Data$Time)
Data$OS <- factor(Data$OS, levels = unique(Data$OS))
Data$Scene <- factor(Data$Scene, levels = unique(Data$Scene))
Data$Acc.Int <- factor(Data$Acc.Int, levels = unique(Data$Acc.Int))


# 2. Verificación de la lectura de datos.
library(psych)
headTail(Data)
str(Data)
summary(Data)


# 3. Gráfico simple de interacción.
# Variable dependiente: Time
# Variables independientes: OS y Scene.
interaction.plot(x.factor = Data$Scene,
 trace.factor = Data$OS,
 response = Data$Time,
 fun = mean,
 type = "b", col = c("black", "red", "green", "blue", "orange"),
 pch = c(19,17,15,19,17),
 fixed = TRUE, xlab= "Scene", ylab="Time(m)", trace.label="OS",
 leg.bty = "o")


# Variable dependiente: Time
# Variables independientes: OS y Acc.Int.
interaction.plot(x.factor = Data$Acc.Int,
 trace.factor = Data$OS,
 response = Data$Time,
 fun = mean,
 type = "b", col = c("black", "red", "green", "blue", "orange"),
 pch = c(19,17,15,19,17),
 fixed = TRUE, xlab= "Accelerator-Integrator", ylab="Time(m)", trace.label="OS",
 leg.bty = "o")

# 4. Modelo lineal
model <- lm(Time ~ Scene * OS * Acc.Int, data = Data)

# 5. Evaluación de los supuestos
x <- residuals(model)
library(rcompanion)
par(mfrow = c(2, 1))
plotNormalHistogram(x, xlab="Time(m)")
qqnorm(resid(model), main = "Normal Q-Q", xlab = "Theoretical Quantiles", ylab = "Standarized residuals")
qqline(resid(model), col = "red", lwd = 2)
par(mfrow = c(1, 1))
plot(fitted(model), residuals(model))
plot(model)
# La normalidad es debatible, sin embargo igual se puede apreciar.
# En el gráfico de dispersión no se observa un patrón claro entre los datos, del mismo modo se procede a realizar la
# prueba Levene para confirmar homocedasticidad.

leveneTest(Time ~ Scene * OS * Acc.Int, data = Data)
# La prueba Levene retorna un P-Value 0.9856, lo que sugiere homocedasticidad en los datos.

# Debido a que la normalidad es el supuesto más permisivo y que tanto el gráfico de dispersión y la prueba Levene
# sugieren la presencia de homocedasticidad en los datos, no se procede a realizar transformación de los mismos.
# Para no incurrir en el riesgo de acercar mucho los datos innecesariamente. (No incurrir en error tipo-II)

# 6. ANOVA
library(car)
Anova(model, type = "II")

# 7. Gráfico bigotes con error estándar OS
Sum <- Summarize(Time ~ OS, data=Data, digits=3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits=3)
Sum

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=OS, y=mean, color = OS)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=OS), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0),
            legend.position="none") +
      ylab(expression("Time (m)")) +
      ggtitle("Time vs OS")

# 8. Gráfico bigotes con error estándar Scene:OS
Sum <- Summarize(Time ~ OS + Scene, data=Data, digits=3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits=3)
Sum

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Scene, y=mean, color = OS)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=OS), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Time (m)")) +
      ggtitle("Time vs Scene in function of OS")


# 9. Gráfico bigotes con error estándar Acc.Int:OS
Sum <- Summarize(Time ~ OS + Acc.Int, data=Data, digits=3)
Sum$se <- Sum$sd / sqrt(Sum$n)
Sum$se <- signif(Sum$se, digits=3)
Sum

library(ggplot2)
pd <- position_dodge(.2)
ggplot(Sum,aes(x=Acc.Int, y=mean, color = OS)) + geom_errorbar(aes(ymin =
      mean - se,ymax = mean + se),width=.2,size=0.7, position=pd)+
      geom_point(aes(shape=OS), size=5, position=pd)+ theme_bw() +
      theme(plot.title = element_text(face="bold", hjust=0.5),
            axis.title = element_text(face="bold"),
            axis.text = element_text(face="bold"),
            plot.caption= element_text(hjust=0),
            legend.text = element_text(face="bold"),
            legend.title = element_text(face="bold"),
            legend.justification = c(1,0)) +
      ylab(expression("Time (m)")) +
      ggtitle("Time vs Acc.Int in function of OS")

# 10. Pruebas T
# Prueba para OS
t_test_os <- pairwise.t.test(Data$Time, Data$OS, p.adjust.method = "BH")
# Prueba para Scene:OS
t_test_os_scene <- pairwise.t.test(Data$Time, Data$OS : Data$Scene, p.adjust.method = "BH")
# Prueba para Acc.Int
t_test_accint <- pairwise.t.test(Data$Time, Data$Acc.Int, p.adjust.method = "BH")

