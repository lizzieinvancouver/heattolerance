## Started 17 May 2017 ##
## By Lizzie and Nicole! ##

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# Get packages
library(ggplot2)
library(car)

dat <- read.csv ("output/clchambdata.csv", header=TRUE)
dat50 <- read.csv ("output/chamb50fl.csv", header=TRUE)
sumdat <- read.csv ("output/chdatsum.csv", header=TRUE)

# alert! This is wrong because we don't have 203 obs
mod.days50 <- lm(days~as.factor(Treat), data=dat50)
Anova(mod.days50)
anova(mod.days50) # Type I Sums of Squares is the same here, so okay to use default anova in R here!

# note to selves! EL_mean is percent flowering, but this is also replicated too much, best to do anova on data where each RowNumNumRep has one row of data
mod.perflow <- lm(EL_mean~as.factor(Treat), data=dat)
mod.perflow # shows means of each chamber
Anova(mod.perflow)
anova(mod.perflow)

# mod.perflow <- lme(EL_mean~as.factor(Treat), random=~1|RowNumNumRep, data=dat)

# Try anova (on data where each RowNumNumRep has one row of data):
# max(perflow)
mod.maxperflo <- lm(max.pf_mean~as.factor(Treat), data=sumdat)
Anova(mod.maxperflo)
anova(mod.maxperflo)
# days to 50% (corr. for only those that made it that far (and do some spot-checking on those data)
# sum of all bag buds (taking mean across stems, if two stems) so basically sum of one cluster
mod.bagbuds <- lm(sum.bfall_mean~as.factor(Treat), data=sumdat)
Anova(mod.bagbuds)
anova(mod.bagbuds)
# sum of all capfall (taking mean across stems, if two stems) so basically sum of one cluster
mod.capfall <- lm(sum.capfall_mean~as.factor(Treat), data=sumdat)
Anova(mod.capfall)
anova(mod.capfall)
# change in length
# change in leafnum
# anova on mean soil moisture (mean per pot across time in chamber)
mod.smoist <- lm(mean.smoist~as.factor(Treat), data=sumdat)
Anova(mod.smoist)
anova(mod.smoist)

# Also! Try plotting some of your estimates from the models and compare to your raw data graphs. 
plot(mod.perflow$coef~as.factor(c(1:5)), ylim=c(-10, 20))
arrows(c(1:5), confint(mod.perflow)[1:5], c(1:5), confint(mod.perflow)[1:5,2], length = 0)
points(dat$EL_mean~as.factor(Treat), data=dat)

plot(mod.bagbuds$coef~as.factor(c(1:5)), ylim=c(-50, 100))
arrows(c(1:5), confint(mod.bagbuds)[1:5], c(1:5), confint(mod.bagbuds)[1:5,2], length = 0)
points(dat$bfall_mean~as.factor(Treat), data=dat)




