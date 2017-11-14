## Started 13 Nov 2017 ##
## By Lizzie ##

## Some quick notes on interpreting logistic regression ##
# My resources were: #
# (1) ARM pp. 89-90
# (2) https://stats.idre.ucla.edu/r/dae/logit-regression/
# (3) And ... https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

# Start by running ghanalysis.R including these lines:
#inflor developed
mod.12diam <- glm(flowering12yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.12diam)

# You can get the odd ratios with exp:
exp(coef(mod.12diam))
# The intercept is around 0, which is good, that makes sense! No big change in the odds of developing a flower when you have a diameter of zero.
# I think the 1.3 for spurdiam_mean means that for one unit increase in spurdiam a plant the odds of flowering increased by a factor of 1.3.
# But Gelman recommends intepreting this value near the mean of our data ...
mean(datnd$spurdiam_mean, na.rm=TRUE)
coef(mod.12diam)[1]+coef(mod.12diam)[2]*mean(datnd$spurdiam_mean, na.rm=TRUE)
# just checking the above ...
-3.260879+0.2602072*mean(datnd$spurdiam_mean, na.rm=TRUE)
# then we look at the slope of the curve at this points
(coef(mod.12diam)[2]*exp(-1.778519))/((1+exp(-1.778519))^2)
# meaning one unit of spurdiam at the mean corresponds to an difference in the probability of flowering of about 3%
# This should be similar to the divide by four rule ... (this works less well depending on the curve and when it passes through the middle of the data)
coef(mod.12diam)[2]/4
# This gives 6.5% but is not as accurate as the above method.

# To discuss more tomorrow! http://www.gettinggeneticsdone.com/2010/12/using-divide-by-4-rule-to-interpret.html and http://www.mypolyuweb.hk/~sjpolit/logisticregression.html
x=seq(-5,5,.01)
invlogit=function(x) exp(x)/(1+exp(x))
y=invlogit(x)
plot(x,y,pch=16,ylab=expression(paste(logit^{-1},(x))))
abline(v=0)
abline(h=.5)
text(.55,.55,expression(paste("Slope is ",beta/4)),adj=c(0,0))
