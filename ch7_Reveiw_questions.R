require(ISLR2)
load("7.R.RData")

plot(x,y)
model1 <- lm(y ~ 1+x+x^2)
summary(model1)

coef(model1)
abline(model1,col="red")