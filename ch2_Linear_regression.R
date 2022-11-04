# Y = B0 + B1*x + Err

# We run a linear regression and the slope estimate is 0.5
B1 = 0.5
# with estimated standard error of 0.2.
SE = 0.2

# What is the largest value of b for which we would NOT reject the null hypothesis that B1 = b ?

# (assume normal approximation to t distribution,
# and that we are using the 5% significance level for a two-sided test; B1 - x * SE, B1 + x * SE " 5 % means 95 %"
# need two significant digits of accuracy)

# why 1.96 and not 2 like in the material ? link: https://en.wikipedia.org/wiki/97.5th_percentile_point thats why

upperDiviation = (B1 + 1.96 * SE)
upperDiviation
lowerDiviation = (B1 - 1.96 * SE)
lowerDiviation