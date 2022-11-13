
# Lab: Linear Regression


## Libraries

###
library(MASS)
library(ISLR)

## Simple Linear Regression

names(Boston)
?Boston

#Use Boston dataset predictors medv (median house val) and lstat (percent households with low socioeconomic status).
plot(medv~lstat,Boston)


# ?lm()
# lm is used to fit linear models, including multivariate ones. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance (although aov may provide a more convenient interface for these).

# lineary model with data "y,x" - > medv ~ "modelled acording to lstat
fit1=lm(medv~lstat,data = Boston)
fit1
summary(fit1)

# Add lineary model to the plot
abline(fit1,col = "red")

names(fit1)
# ?confint
# Computes confidence intervals for one or more parameters in a fitted model. There is a default and a method for objects inheriting from class
confint(fit1)

predict(fit1,data.frame(lstat=c(5,10,15)),interval = "confidence")


### MUltiple Linear Regression
fit2=lm(medv~lstat+age,data = Boston)
summary(fit2)
fit3=lm(medv~.,data = Boston)
summary(fit3) # age is suddenly not so signifiance any more
par(mfrow=c(2,2))
plot(fit3) # many plots here are out of scope for this topic
fit4=update(fit3,~.-age-indus) # update fit3 with removing age and indus
summary(fit4)

### NonLinear terms and interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit6=lm(medv~lstat + I(lstat^2),Boston); summary(fit6) # two commands seperated with ;

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)

fit7=lm(medv~poly(lstat,4)) # 4 degreed poly
points(lstat,fitted(fit7),col="blue",pch=20)
#plot(1:20,1:20,pch=1:20,cex=2) plotting characters


### Qualitative predictors

#fix(Carseats)
names(Carseats)
summary(Carseats)

fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

### Writing R Functions

regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}

attach(Carseats)
regplot(Price,Sales)

regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
