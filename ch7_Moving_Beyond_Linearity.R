# Nonlinear Models

# Here we explore the use of nonlinear models using some tools in R

require(ISLR)
attach(Wage)

###### Polynomials #######

# First we will ise polynomials, and focus on a single predictor "age"

fit=lm(wage~poly(age,4),data = Wage)
summary(fit)

# The "poly()" function generates a basic of "orthogonal polynomials"
# Lets make a plot of the fitted function, along with the standard errors of the fit.

agelims=range(age)
age.grid=seq(from = agelims[1],to=agelims[2])
preds = predict(fit,newdata = list(age=age.grid),se=TRUE)# predictors
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se) # standard error bands
plot(age,wage,col = "darkgrey")
lines(age.grid,preds$fit,lwd = 2,col = "blue")
matlines(age.grid,se.bands,col = "blue",lty = 2) # plot the fitted function, plus minus two standard errors

# There are other more direct ways of doing this in R. For example

fita=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage) # I() identity function inside linear model function
summary(fita)

plot(fitted(fit),fitted(fita))

# By using orthogonal polynomials in this simple way, it turns out that we can separately test
# for each coefficient. So if we look at the summart again, we can see that the linear, quadratic and
# cubic terms are significnat, but not the quartic.

summary(fit)

# This only works with linear regression, and if there is a single predictor. In general we would use
# anova() as this next example demonstrates.

fita=lm(wage~education, data=Wage) # not needed
fitb=lm(wage~education+age, data=Wage) # looks good se Pr
fitc=lm(wage~education+poly(age,2), data=Wage) # looks good se Pr
fitd=lm(wage~education+poly(age,3), data=Wage) # not needed
anova(fita,fitb,fitc,fitd)


###### Polynominal logistic regression ######

# Now we fit a logistic regression model to a binary
# respons variable, constructed from "wage". We code the big earners (">250k") as 1, else 0

fit=glm(I(wage>250) ~ poly(age,3), data = Wage, family = binomial)
summary(fit)
preds=predict( fit,list(age=age.grid),se=T)
se.bands=preds$fit + cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)
se.bands[1:5,]

# we have done the computerations on the logit scale. To transform we need to apply the inverse logit
# mapping

prob.bands=exp(se.bands)/(1+exp(se.bands))
matplot(age.grid,prob.bands,col = "blue",lwd = c(2,1,1),lty=c(1,2,2),type = "l",ylim = c(0,.1))
points(jitter(age),I(wage>250)/10,pch = "I",cex=.5)



######## Splines #############

# Spilnes are more flexible than polynomials, but the idea is rather similar.
# Here we will expore cbic spilnes.

require(splines)
fit=lm(wage~bs(age,knots = c(25,40,60)),data = Wage)
plot(age,wage,col = "darkgrey")
lines(age.grid,predict(fit,list(age=age.grid)),col = "darkgreen",lwd = 2)
abline(v=c(25,40,60),lty=2,col="darkgreen")

# The smoothing splines dose not require knot selection, but it dose have a smoothin parameter.
# which can conveniently be specified via the effective degrees of freedom of "df"

fit=smooth.spline(age,wage,df=16)
lines(fit,col = "red",lwd = 2)

# Or we can ise LDO cross-validation to select the smoothing parameter for us automatically:

fit=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)
fit


####### Generalized Additive Models #########

# So far we have focused on fitting models with mostly single nonlinear terms.
# The "gam" package makes it easier to work with multiple nonlinear tearms.
# In addition it knows how to plot these functions and their standard errors.

require(gam)
gam1=gam(wage~s(age,df=4)+s(year,df=4)+education, data = Wage) # s() is a smoothing function in gam()

par(mfrow=c(1,3))
plot(gam1,se=T)

gam2=gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education, data = Wage, family = "binomial") # s() is a smoothing function in gam()

plot(gam2)

# One nice feature of the "gam" package is that it knows how to plot the functions nicely,
# even for models fit by "lm" and "glm"


###### Helper function ########
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }

