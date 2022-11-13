### credit card balance default chance on 50% ###

## Libraries

###
library(MASS)
library(ISLR)

#fix(Default)
names(Default)

plot(default~balance,Default)

# lineary model with data "y,x" - > medv ~ "modelled acording to lstat
fit1=lm(default~balance,data = Default)
fit1
summary(fit1)

# Add lineary model to the plot
abline(fit1,col = "red")
