## Started 13 Nov 2017 ##
## By Lizzie ##

## Some quick notes on interpreting logistic regression ##
# My resources were: #
# (1) ARM pp. 81-82, 89-90
# (2) https://stats.idre.ucla.edu/r/dae/logit-regression/
# (3) And ... https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

library(arm)

# Start by running ghanalysis.R including these lines:
#inflor developed
mod.12diam <- glm(flowering12yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.12diam)

display(mod.12diam)

# You can get the odd ratios with exp:
exp(coef(mod.12diam))
# The intercept is around 0, which is good, that makes sense! No big change in the odds of developing a flower when you have a diameter of zero.
# Since the curve is non-linear, predictions vary depending on where you evaluate, Gelman recommends intepreting this value near the mean of our data ...
mean(datnd$spurdiam_mean, na.rm=TRUE)
hist(datnd$spurdiam_mean)
# We can look at a change of one ... around here, say from 5 to 6
invlogit(coef(mod.12diam)[1] + coef(mod.12diam)[2]*6)-
    invlogit(coef(mod.12diam)[1] + coef(mod.12diam)[2]*5)

# Alternative method ... 
coef(mod.12diam)[1]+coef(mod.12diam)[2]*mean(datnd$spurdiam_mean, na.rm=TRUE)
# just checking the above ...
-3.260879+0.2602072*mean(datnd$spurdiam_mean, na.rm=TRUE)
# then we look at the slope of the curve (derivative) at this point
(coef(mod.12diam)[2]*exp(-1.778519))/((1+exp(-1.778519))^2)
# meaning that a change of one in spurdiam at the mean corresponds to an difference in the probability of flowering of about 3%
# This should be similar to the divide by four rule ... (this works less well depending on the curve and when it passes through the middle of the data)
coef(mod.12diam)[2]/4
# This gives 6.5% but is not as accurate as the above method.
