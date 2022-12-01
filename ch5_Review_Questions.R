# Big shout out to kiy from https://github.com/kjy/StatisticalLearning_classstuff/blob/master/HW_TheBootstrap.R#L50

# Download the file 5.R.RData and load it into R using load("5.R.RData").
# Consider the linear regression model of y on X1 and X2. What is the standard error for B1?

load("5.R.RData")

model1 <- lm(y ~ X1 + X2)
summary(model1)

# answeer is B1 = 0.026

# Q2 Next, plot the data using matplot(Xy,type="l").
# Which of the following do you think is most likely given what you see?
# B1 is too high
# B1 is too low
# B1 is about right

length(Xy)
colnames(Xy) <- c("X1", "X2", "y")
Xy
?matplot # Plot Columns of Matrices
matplot(Xy,type="l",col = c(2:5,7))
legend("bottomleft",pch = 15:18, legend = colnames(Xy), col = c(2:5,7))

# answeer B1 is to low

#There is very strong autocorrelation between consecutive rows of the data matrix.
# Roughly speaking, we have about 10-20 repeats of every data point,
# so the sample size is in effect much smaller than the number of rows (1000 in this case).



# Now, use the (standard) bootstrap to estimate B1. To within 10%, what do you get?
## Bootstrap
# see (5.8) page 211

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y) # coveriance
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Xy$X1,Xy$y)

alpha.fn=function(data, index){
  with(data[index,],alpha(Xy$X1,Xy$y))
}

alpha.fn<-function(data, index) {
  fit1<-lm(y~., data=Xy[index,])
  coefficients(fit1)[['X1']]}

set.seed(1)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))

boot.out=boot(Xy,alpha.fn,R=1000)
boot.out

# Q4. Finally, use the block bootstrap to estimate s.e.(β^1).
# Use blocks of size 100. To within 10%, what do you get?

new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)

new.Xy = Xy[new.rows, ]

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y) # coveriance
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(new.Xy$X1,new.Xy$y)

alpha.fn=function(data, index){
  with(data[index,],alpha(new.Xy$X1,new.Xy$y))
}

alpha.fn<-function(data, index) {
  fit1<-lm(y~., data=new.Xy[index,])
  coefficients(fit1)[['X1']]}

set.seed(1)
alpha.fn (new.Xy,sample(1:100,100,replace=TRUE))

boot.out=boot(new.Xy,alpha.fn,R=1000)
boot.out