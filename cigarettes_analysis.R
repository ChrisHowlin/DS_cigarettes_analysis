
#   In 1970, a national insurance organisation wanted to study the consumption pattern of cigarettes in all
#   50 states. Variables in this data are:
#       - Age       Median age of a person living in a state.
#       - HS        Percentage of people over 25 years of age in a state who had completed high school.
#       - Income    Per capita personal income for a state (in USD).
#       - Black     Percentage of black people living in a state.
#       - Female    Percentage of females living in a state.
#       - Price     Weighted average price (in cents) of a pack of cigarettes in a state.
#       - Sales     Number of packs of cigarettes sold in a state on a per capita basis.
#
#   Source: http://www.ats.ucla.edu/stat/examples/chp/p081.txt
#
# Objective: Find best model for predicting for sales

library(ggplot2)

cigarettes <- read.csv('D:\\DataScience\\lesson2\\DS_cigarettes_analysis\\cigarettes.txt', sep='\t', header=T)

# Checking out the Sales histogram before we start shows that the data peaks in the 100-150 region with a long tail
# going up to 300 
hist(cigarettes$Sales)

# From checking the pairwise correlations, there don't see to be any good indicators for sales.
# We can see that HS and Income have a linear relationship which is what you might expect

pairs(cigarettes)

# We should remove the State vector for now, as it is not comparable across samples. I.e. we don't have any information
# about the dependence between the states. Later we could put this against some geographic data, such as lat/long. One
# thing to notice is that DC is a massive outlier. Mean Black% is ~10%, in DC the value is 71.1!

plot(cigarettes$Black ~ cigarettes$State)
mean (cigarettes$Black)

cig_no_states <- cigarettes
cig_no_states$State <- NULL
pairs(cig_no_states)

# Create a linear model of Sales against everything else
no_states_fit <- lm(Sales ~ ., data=cig_no_states)
summary(no_states_fit)

# Summary output:
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -48.398 -12.388  -5.367   6.270 133.213 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 103.34485  245.60719   0.421  0.67597   
# Age           4.52045    3.21977   1.404  0.16735   
# HS           -0.06159    0.81468  -0.076  0.94008   
# Income        0.01895    0.01022   1.855  0.07036 . 
# Black         0.35754    0.48722   0.734  0.46695   
# Female       -1.05286    5.56101  -0.189  0.85071   
# Price        -3.25492    1.03141  -3.156  0.00289 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 28.17 on 44 degrees of freedom
# Multiple R-squared:  0.3208,	Adjusted R-squared:  0.2282 
# F-statistic: 3.464 on 6 and 44 DF,  p-value: 0.006857
#
# The residual stats indicate that using all the variables is not a good fit: the 1Q-3Q comparison shows we are
# skewed towards overestimating, with some large underestimations
#
# The R-squared values are high - only 1/3 of the variance in the sample is accounted for by the model. The F-statistic 
# p-value is low which gives a good chance that this value is better than a random dist.

# Plotting the residuals, we see that there are two large outliers (index 30 (NV) and 31 (NH)), hmm. 
plot(resid(no_states_fit))
resid(no_states_fit)

# Backwards elimination
# Let's try removing the variables with the highest p-value: HS
no_states_fit2 = update(no_states_fit, .~. -HS)
summary(no_states_fit2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -48.136 -12.508  -5.277   6.351 133.141 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 100.133255 239.217507   0.419  0.67751   
# Age           4.593832   3.035858   1.513  0.13722   
# Income        0.018418   0.007371   2.499  0.01618 * 
# Black         0.379049   0.391066   0.969  0.33759   
# Female       -1.067115   5.496066  -0.194  0.84692   
# Price        -3.243835   1.009591  -3.213  0.00243 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 27.86 on 45 degrees of freedom
# Multiple R-squared:  0.3208,	Adjusted R-squared:  0.2453 
# F-statistic:  4.25 on 5 and 45 DF,  p-value: 0.002998
#
# The quartiles are just as bad, the RSE has gone down a bit and the R-squared has gone up

# The residuals are pretty much the same, NV and NH are still outliers on the residuals
plot(resid(no_states_fit2))
resid(no_states_fit2)

# Try removing the next highest variable: Female
no_states_fit3 = update(no_states_fit2, .~. -Female)
summary(no_states_fit3)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -46.784 -11.810  -5.380   5.758 132.789 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 55.329580  62.395293   0.887   0.3798   
# Age          4.191538   2.195535   1.909   0.0625 . 
# Income       0.018892   0.006882   2.745   0.0086 **
# Black        0.334162   0.312098   1.071   0.2899   
# Price       -3.239941   0.998778  -3.244   0.0022 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 27.57 on 46 degrees of freedom
# Multiple R-squared:  0.3202,	Adjusted R-squared:  0.2611 
# F-statistic: 5.416 on 4 and 46 DF,  p-value: 0.001168
#
# No difference in the quartiles, still the same skew.
# The RSE is down and the R-squared is up, so this is a bit better

# The residuals are pretty much the same, NV and NH are still outliers on the residuals
plot(resid(no_states_fit3))
resid(no_states_fit3)

# Another round for the backwards elimination: Black
no_states_fit4 = update(no_states_fit3, .~. -Black)
summary(no_states_fit4)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -50.430 -13.853  -4.962   6.691 128.947 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 64.248227  61.933008   1.037  0.30487   
# Age          4.155909   2.198699   1.890  0.06491 . 
# Income       0.019281   0.006883   2.801  0.00737 **
# Price       -3.399234   0.989172  -3.436  0.00124 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 27.61 on 47 degrees of freedom
# Multiple R-squared:  0.3032,	Adjusted R-squared:  0.2588 
# F-statistic: 6.818 on 3 and 47 DF,  p-value: 0.0006565
#
# The quartiles are pretty much the same. The RSE is up and the R-squared is down, so it seems like we are bottoming
# out.

# The residuals are pretty much the same, NV and NH are still outliers on the residuals
plot(resid(no_states_fit4))
resid(no_states_fit4)

##############
# Conclusion
#
# This data is does not present any strong linear relationships with the raw data. It may be that some of the
# variables need to be transformed to exhibit a linear relationship to the sales.
#
# Looking at the pairs, it seems there is a pretty good inverse relationship between price and sales
# (low price --> high sales), also relationship between income and sales, looks logarithmic relationship between
# sales and income and an inverse relationship between price and sales.
#
# OK, so in all these cases, there were large residuals with NV + NH which suggests these are outliers of some kind.
# They are by far the highest values looking at the sales values. This implies there is some extra data that we don't
# model, or that the variables don't correspond directly
##############

##############
# Extension
# 
# The following extension looks at removing the NH and NV values as these seem to be persistent outliers. By doing
# perhaps some other relationships are more obvious
##############

cig_no_states_no_outlier <- cig_no_states[-c(30, 31), ]
pairs(cig_no_states_no_outlier)

# It becomes clear that there is a pretty good inverse relationship between price and sales (low price --> high sales),
# also relationship between income and sales, looks logarithmic relationship between sales and income and an inverse
# relationship between price and sales.

plot(Sales ~ I(1/Price), data=cig_no_states_no_outlier)
sales_price_lm = lm(Sales ~ I(1/Price), data=cig_no_states_no_outlier)
plot(sales_price_lm)
summary(sales_price_lm)
plot(resid(sales_price_lm))
hist(resid(sales_price_lm))

plot(Sales ~ log(Income), data =cig_no_states_no_outlier)
sales_income_lm <- lm(Sales ~ log(Income), data=cig_no_states_no_outlier)
summary(sales_income_lm)
hist(resid(sales_income_lm))

# Update the vectors with the new disributions

cig_no_states_no_outlier$Price <- (1/cig_no_states_no_outlier$Price)
cig_no_states_no_outlier$Income <-  (log(cig_no_states_no_outlier$Income))
pairs(cig_no_states)
pairs(cig_no_states_no_outlier)

no_outlier_fit <- lm(Sales ~ ., data=cig_no_states_no_outlier)
summary(no_outlier_fit)
plot(resid(no_outlier_fit))

no_outlier_fit2 <- update(no_outlier_fit, .~. -HS)
summary(no_outlier_fit2)
plot(resid(no_outlier_fit2))
hist(resid(no_outlier_fit2))

no_outlier_fit3 <- update(no_outlier_fit2, .~. -Female)
summary(no_outlier_fit3)
plot(resid(no_outlier_fit3))
hist(resid(no_outlier_fit3))

no_outlier_fit4 <- update(no_outlier_fit3, .~. -Age -Black)
summary(no_outlier_fit4)
plot(resid(no_outlier_fit4))
hist(resid(no_outlier_fit4))

# After all this, the model isn't much better than before! I don't think I got the relationships correct, as
# after the inverse and log transforms, the data still wasn't a great linear relationship
