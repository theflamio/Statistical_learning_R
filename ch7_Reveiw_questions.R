require(ISLR)
dataSet = load("7.R.RData")

fit=lm(wage~poly(age,4),data = dataSet)
summary(fit)