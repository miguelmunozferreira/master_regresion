# Ejercicios de Faraway
install.packages("faraway")
library(faraway)

# Ejercicio 1
data("teengamb",package = "faraway")
str(teengamb)
summary(teengamb)
teengamb$sex <- factor(teengamb$sex)
lmod <- lm(gamble~sex+status+income+verbal,data = teengamb)
lmodsum <- summary(lmod)

#A
lmodsum$r.squared*100
#B
max(lmodsum$residuals)
which.max(lmodsum$residuals)
#C
mean(lmodsum$residuals)
median(lmodsum$residuals)
#D
cor(lmod$fitted.values,lmod$residuals)
#E
cor(teengamb$income,lmod$residuals)
#F
lmod$coefficients["sex1"]
#comprobación
data0 <- data.frame(sex="0",status=mean(teengamb$status),income=mean(teengamb$income),verbal=mean(teengamb$verbal))
data1 <- data.frame(sex="1",status=mean(teengamb$status),income=mean(teengamb$income),verbal=mean(teengamb$verbal))
predic0 <- predict(lmod,newdata = data0)
predic1 <- predict(lmod,newdata = data1)
predic0-predic1

#Ejercicio2
data("uswages",package = "faraway")
str(uswages)
summary(uswages)
lmod <- lm(wage ~ educ + exper, data=uswages)
coefficients(lmod)
lmod_log2 <- lm(log2(wage) ~ educ + exper, data=uswages)
coefficients(lmod_log2)
lmod_log <- lm(log(wage) ~ educ + exper, data=uswages)
coefficients(lmod_log)

#Ejercicio3
x <- 1:20
y <- x+rnorm(20)
g2 <- lm(y ~ x + I(x^2))
coef(g2)
mx <- model.matrix(g2)
solve(t(mx)%*%mx)%*%t(mx)%*%y

g3 <- update(g2,.~.+I(x^3))
coef(g3)
mx <- model.matrix(g3)
solve(t(mx)%*%mx)%*%t(mx)%*%y

g4 <- update(g3,.~.+I(x^4))
coef(g4)
mx <- model.matrix(g4)
solve(t(mx)%*%mx)%*%t(mx)%*%y

g5 <- update(g4,.~.+I(x^5))
coef(g5)
mx <- model.matrix(g5)
solve(t(mx)%*%mx)%*%t(mx)%*%y

g6 <- update(g5,.~.+I(x^6))
coef(g5)
mx <- model.matrix(g6)
mx
solve(t(mx)%*%mx)%*%t(mx)%*%y


#Ejercicio4 
addValue <- function(df,nvar,sigma,r2){
  df[nvar,1]<-nvar
  df[nvar,2]<-sigma
  df[nvar,3]<-r2
  return (df)
}

data("prostate",package="faraway")
lm <- lm(lpsa ~ lcavol, data=prostate)
lmsum <- summary(lm)
result <- data.frame(Variables=1,Sigma=lmsum$sigma,R2=lmsum$r.squared)

#Add lweight
lm2 <- update(lm,.~. + prostate$lweight)
lmsum2 <- summary(lm2)
result <- addValue(result,2,lmsum2$sigma,lmsum2$r.squared)

#Add svi
lm3 <- update(lm2,.~. + prostate$svi)
lmsum3 <- summary(lm3)
result <- addValue(result,3,lmsum3$sigma,lmsum3$r.squared)

#Add lbph
lm4 <- update(lm3,.~. + prostate$lbph)
lmsum4 <- summary(lm4)
result <- addValue(result,4,lmsum4$sigma,lmsum4$r.squared)

#Add age
lm5 <- update(lm4,.~. + prostate$age)
lmsum5 <- summary(lm5)
result <- addValue(result,5,lmsum5$sigma,lmsum5$r.squared)

#Add lcp
lm6 <- update(lm5,.~. + prostate$lcp)
lmsum6 <- summary(lm6)
result <- addValue(result,6,lmsum6$sigma,lmsum6$r.squared)

#Add pgg45
lm7 <- update(lm6,.~. + prostate$pgg45)
lmsum7 <- summary(lm7)
result <- addValue(result,7,lmsum7$sigma,lmsum7$r.squared)

matplot(result[,2:3],xlab = "Número de variables", ylab="Valores",type = "b", col=2:3, lty=2:3)
legend("topright",c(expression(hat(sigma)),expression(R^2)),col=2:3,lty=2:3)

#EJERCICIO 5
data("prostate",package="faraway")
plot(prostate$lpsa,prostate$lcavol)
lm1 <- lm(lpsa~lcavol,data=prostate)
lm2 <- lm(lcavol~lpsa,data=prostate)
abline(lm1$coefficients,col="red")
abline(lm2$coefficients,col="blue")









