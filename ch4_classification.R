# We will begin by examining some numerical and graphical summaries of the Smarket data, which is part of the ISLR2 library.
# This data set consists of percentage returns for the S&P 500 stock index over 1,250~days,
# from the beginning of 2001 until the end of 2005. For each date,
# we have recorded the percentage returns for each of the five previous trading days, lagone through lagfive.
# We have also recorded volume (the number of shares traded on the previous day, in billions),
# Today (the percentage return on the date in question) and direction (whether the market was Up or Down on this date).

# Our goal is to predict direction (a qualitative response) using the other features.

## The Stock Market Data

library(ISLR2)
names(Smarket)
summary(Smarket)

?Smarket

pairs(Smarket,col=Smarket$Direction)
#Logistic regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)

summary(glm.fit)

# The cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.
# The first command below gives an error message because the direction variable is qualitative.

#cor(Smarket) error

cor(Smarket[, -9])

# As one would expect, the correlations between the lag variables and today’s returns are close to zero. In other words,
# there appears to be little correlation between today’s returns and previous days’ returns.
# The only substantial correlation is between Year and volume. By plotting the data,
# which is ordered chronologically, we see that volume is increasing over time. In other words,
# the average number of shares traded daily increased from 2001 to 2005.

# "correlation" may indicate any type of association, in statistics it normally refers to the degree to which a pair of variables are linearly related.

attach(Smarket)
?attach()
plot(Volume)


# Logistic Regression

# Next, we will fit a logistic regression model in order to predict direction using lagone through lagfive and volume.
# The glm() function can be used to fit many types of generalized linear models ,
# including logistic regression. The syntax of the glm() function is similar to that of lm(),
# except that we must pass in the argument family = binomial in order to tell R to run a logistic regression rather than some other type of generalized linear model.

glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial
  )
summary(glm.fits)


# The smallest p-value here is associated with lagone.
# The negative coefficient for this predictor suggests that if the market had a positive return yesterday,
# then it is less likely to go up today. However, at a value of 0.15, the p-value is still relatively large,
# and so there is no clear evidence of a real association between lagone and direction.

# We use the coef() function in order to access just the coefficients for this fitted model.
# We can also use the summary() function to access particular aspects of the fitted model, such as the p-values for the coefficients.

coef(glm.fits)

summary(glm.fits)$coef

summary(glm.fits)$coef[, 4]