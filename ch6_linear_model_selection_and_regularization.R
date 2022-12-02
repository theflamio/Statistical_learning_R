
# Model Selection
#======================


library(ISLR)
summary(Hitters)

# 59 missing values with this function we can remove them.
Hitters=na.omit(Hitters)

with(Hitters,sum(is.na(Salary))) #check if any players are missing their salary data.

########### Best subset regression #############
# we will now use the package "leaps" to evaluate all the best-subset models

library(leaps)

regfit.full=regsubsets(Salary~.,data=Hitters)
summary(regfit.full)


regfit.full=regsubsets(Salary~.,data=Hitters, nvmax = 19)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
#points(10,reg.summary@cp[10],pch=20,col= "red")

#plot(regfit.full,scale = "Cp")

library(glmnet)

data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y
?cv.glmnet()


####### Forward Stepwise Selection #########

# Here we use the "regsubsets" function but specify the "method~"forward"
# option

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")


# Model Selection using a Validation Set

# Lets make a training and validation set, so that we can choose a good subset model

# we will do it using a slighly different approach from what was done in the book.

dim(Hitters) #dimension of hitters 2/3 trainging is 118 observations and 1/3 test
set.seed(1)
train=sample(seq(263),180,replace=FALSE)
train
regfit.fwd=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward")

# Now we will make predictions on the observations not used for training.
# We know there are 19 models, so we set up some vectors to record the errors.
# We have to do a bit of work here, because there is no predict method for "regsubsets"

val.errors=rep(NA,19)
x.test=model.matrix(Salary~.,data = Hitters[-train,])# exclude train set from test
for (i in 1:19){
  coefi = coef(regfit.fwd, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[-train] - pred)^2)
}
plot(sqrt(val.errors),ylab = "Root MSE",ylim = c(300,400),pch=19,type="b")


###
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }
###


####### cross validation "10 fold cross vailidation ########

set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters)))
folds
table(folds)
cv.errors=matrix(NA,10,19) # 10 folds and 19 subsets
for (k in 1:10){
  best.fit=regsubsets(Salary~.,data = Hitters[folds!=k,],nvmax = 19,method = "forward")
  for (i in 1:19){
    pred = predict(best.fit,Hitters[folds==k,],id=i)
    cv.errors[k,i]=mean((Hitters$Salary[folds==k] - pred)^2)
  }
}

rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch = 19,type = "b") # best model 9 or 10

###### Ridge Regression and the Lasso ######

# We will use the package "glmnet", which dose not use the model formula language, so we will set up on "x" and "y"

library(glmnet)
x=model.matrix(Salary~.-1,data = Hitters)
y=Hitters$Salary

# First we will fit a ridge-regression model.
# This is achieved by calling "glmnet" with "alpha=0" (see help file)
# If we wanted to use Lasso then "alpha=1"
# "alpha = 0 < x < 1" we get elastic net models
# There is also a "cv.glmnet" function which will do the cross-validation for us.

fit.ridge=glmnet(x,y,alpha = 0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0) # dose k-fold cross validation
plot(cv.ridge) # the 20 in the top mean 19 variables plus the intercept

# Now for the Lasso model: for this "alpha=1" glmnet default is alpha=1
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar = "lambda",label = TRUE) # top of the graph we seen numbers of non zero variables
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

# Suppose we want to use our earlier train/validation division to select the "lampda" for the lasso
# This is easy to do

lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab = "log(Lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]] # order aseend them first value the smallest
lam.best
coef(lasso.tr,s=lam.best)

