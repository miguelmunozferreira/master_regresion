##########################
# EJERCICIOS INICIALES
##########################

#############
# EJ1: BUITRES LEONADOS
#############
# Cargar datos
heartbpm <- c(47.53, 48.27, 49.51, 51.09, 52.57, 54.30,
              54.25, 54.45, 57.95, 60.92, 61.91, 77.92,
              82.07, 82.95, 83.94, 86.96, 90.42, 92.93, 100.05)
metabol <- c(6.15, 6.31, 6.43, 6.78, 6.86, 6.90, 7.37, 7.41,
             8.24, 9.22, 8.16, 12.61, 15.26, 13.09, 14.59,
             17.35, 18.57, 19.00, 20.70)
buitre <-data.frame(heartbpm,metabol)
rm(heartbpm,metabol)
attach(buitre)
# Dibujar nube de puntos
plot(heartbpm,metabol,pch=20,col="blue",xlab = "Frecuencia cardiaca", ylab = "Metabolismo")
# Añadir recta de regresión
recta <- lm(metabol~heartbpm)
abline(recta$coefficients,col="red")
sum(recta$residuals^2)
# Grafico de residuos contra valores ajustados
residuos <- recta$residuals
prediccion <- recta$fitted.values
plot(prediccion,residuos)
abline(h=0)
# Curva de regresión parabolica
plot(heartbpm,metabol,pch=20,col="blue",xlab = "Frecuencia cardiaca", ylab = "Metabolismo")
heartbpm2 <- heartbpm ^ 2
parabola <- lm (metabol ~ heartbpm + heartbpm2)
lines(heartbpm, parabola$fitted.values, col='green')
sum(parabola$residuals^2)

#############
# EJ2: TENSION ARTERIAL
#############
# Cargar datos
tension <- c(114,134,124,128,116,120,138,130,139,125,132,130,140,144,110,
             148,124,136,150,120,144,153,134,152,158,124,128,138,142,160,
             135,138,142,145,149,156,159,130,157,142,144,160,174,156,158,
             174,150,154,165,164,168,140,170,185,154,169,172,144,162,158,
             162,176,176,158,170,172,184,175,180)
edad <- c(17,18,19,19,20,21,21,22,23,25,26,29,33,33,34,35,36,36,
          38,39,39,40,41,41,41,42,42,42,44,44,45,45,46,47,47,47,
          47,48,48,50,50,51,51,52,53,55,56,56,56,57,57,59,59,60,
          61,61,62,63,64,65,65,65,66,67,67,68,68,69,70)
# Calcular coeficinetes de regresión de la RMC
recta <- lm(tension~edad)
recta$coefficients
plot(edad,tension)
abline(recta$coefficients)

##########################
# EJERCICIOS FARAWAY
##########################
library(faraway)
#############
# EJ1: DATASET teengamb
#############
data("teengamb")
head(teengamb)
str(teengamb)
teengamb$sex <- factor(teengamb$sex,labels = c('male','female'))
teengamb$verbal <- factor(teengamb$verbal)
summary(teengamb) #Util para localizar valores no esperados
par(mfrow = c(3,2))
plot(teengamb$sex, main = "Sex")
boxplot(teengamb$status, main = "Status")
boxplot(teengamb$income, main = "Income")
plot(teengamb$verbal, main = "Verbal")
boxplot(teengamb$gamble, main = "Gamble")
sort(teengamb$gamble)

#############
# EJ2: DATASET uswages
#############
data("uswages")
head(uswages)
str(uswages)
uswages$race <- factor(uswages$race)
uswages$smsa <- factor(uswages$smsa)
uswages$ne <- factor(uswages$ne)
uswages$mw <- factor(uswages$mw)
uswages$so <- factor(uswages$so)
uswages$we <- factor(uswages$we)
uswages$pt <- factor(uswages$pt)
#Eliminamos los valores negativos
uswages$exper[uswages$exper<0] <- NA
summary(uswages)
par(mfrow=c(5,2))

for (i in 1:dim(uswages)[2]){
  if (class(uswages[,i]) == 'factor'){
    plot(uswages[,i], main = colnames(uswages)[i])
  } else {
    boxplot(uswages[,i], main = colnames(uswages)[i])
  }
}

##########################
# EJERCICIOS CARMONA
##########################

#############
# EJERCICIO 3
#############

densidad <- c(12.7,17.0,66.0,50.0,87.8,81.4,75.6,66.2,81.1,62.8,77.0,89.6,
          18.3,19.1,16.5,22.2,18.6,66.0,60.3,56.0,66.3,61.7,66.6,67.8)
velocidad <- c(62.4,50.7,17.1,25.9,12.4,13.4,13.7,17.9,13.8,17.9,15.8,12.6,
         51.2,50.8,54.7,46.5,46.3,16.9,19.8,21.2,18.3,18.0,16.6,18.3)
rvelocidad <- sqrt(velocidad)


#Dibujar nube de puntes y recta que pasa por los puntos (12.7,???62.4) y (87.8,???12.4)
x <- c(12.7, 87.8)
y <- sqrt(c(62.4,12.4))
slope <- diff(y)/diff(x)
intercept <- y[1]-slope*x[1]
plot(densidad,rvelocidad,xlab = "Densidad", ylab = "Velocidad")
abline(c(intercept, slope))

# Dibujar el gráfico de los residuos con la densidad y el gráfico con las predicciones. Calcular la suma de cuadrados de residuos
predicciones <- intercept + slope * densidad
residuos <- rvelocidad - predicciones
par(mfrow=c(2,1))
plot(densidad,residuos)
abline(h=0,lty=2)
plot(predicciones,residuos)
abline(h=0,lty=2)
sum(residuos^2)

# Hallar la recta de regresión simple. Dibujar el gráfico de los residuos con la densidad y el
# gráfico con las predicciones. Calcular la suma de cuadrados de los residuos
recta <- lm(rvelocidad ~ densidad)
par(mfrow=c(2,1))
plot(densidad,recta$residuals)
abline(h=0,lty=2)
plot(recta$fitted.values,recta$residuals)
abline(h=0,lty=2)
sum(recta$residuals^2)

# Mejorar el modelo anterior considerando una regresión parabólica. Dibujar el gráfico de los
# residuos con la densidad y el gráfico con las predicciones. Calcular la suma de cuadrados de
# los residuos
densidad2 <- densidad^2
parabola <- lm(rvelocidad ~ densidad + densidad2)
par(mfrow=c(2,1))
plot(densidad,parabola$residuals)
abline(h=0,lty=2)
plot(parabola$fitted.values,parabola$residuals)
abline(h=0,lty=2)
sum(parabola$residuals^2)

# Calcular la capacidad de la carretera o punto de máximo flujo. Recordar que flujo = vel ×
# densidad.
cc <- parabola$coefficients
flujo <- (cc[1] + cc[2]*densidad + cc[3]*densidad^2)^2 * densidad
par(mfrow=c(1,1))
plot(densidad,flujo)
max(flujo)
densidad[which.max(flujo)]


#############
# EJERCICIO 4
#############

tm_h <- c(9.84,19.32,43.19,102.58,215.78,787.96,1627.34,7956)
tm_m <- c(10.94,22.12,48.25,117.73,240.83,899.88,1861.63,8765)
distancia <- c(100,200,400,800,1500,5000,10000,42192)

#apartado A
recta_h <- lm(tm_h~distancia)
plot(distancia,tm_h)
abline(recta_h)
plot(distancia,recta_h$residuals,xlab="distancia",ylab="residuos")
plot(recta_h$fitted.values,recta_h$residuals,xlab="predicción",ylab="residuos") 
sum(recta_h$residuals^2)
summary(recta_h)$r.squared

#apartado B
ltm_h <- log(tm_h)
ldistancia <- log(distancia)
recta_lh <- lm(ltm_h~ldistancia)
plot(ldistancia,ltm_h)
abline(recta_lh)
plot(ldistancia,recta_lh$residuals,xlab="distancia",ylab="residuos")
plot(recta_lh$fitted.values,recta_lh$residuals,xlab="predicción",ylab="residuos") 
sum(recta_lh$residuals^2)
summary(recta_lh)$r.squared

#apartado C
recta_m <- lm(tm_m~distancia)
plot(distancia,tm_m)
abline(recta_m)
plot(distancia,recta_m$residuals,xlab="distancia",ylab="residuos")
plot(recta_m$fitted.values,recta_m$residuals,xlab="predicción",ylab="residuos") 
sum(recta_m$residuals^2)
summary(recta_m)$r.squared

ltm_m <- log(tm_m)
ldistancia <- log(distancia)
recta_lm <- lm(ltm_m~ldistancia)
plot(ldistancia,ltm_m)
abline(recta_lm)
plot(ldistancia,recta_lm$residuals,xlab="distancia",ylab="residuos")
plot(recta_lm$fitted.values,recta_lm$residuals,xlab="predicción",ylab="residuos") 
sum(recta_lm$residuals^2)
summary(recta_lm)$r.squared
