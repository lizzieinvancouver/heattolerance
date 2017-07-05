## Started 17 May 2017 ##
## By Lizzie and Nicole! ##

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# Get packages
library(ggplot2)
library(car)
library(colorspace)

dat <- read.csv ("output/clchambdata.csv", header=TRUE)
dat50 <- read.csv ("output/chamb50fl.csv", header=TRUE)
sumdat <- read.csv ("output/chdatsum.csv", header=TRUE)

##############
### Models ###
##############

# 50% flowering
mod.days50 <- lm(days~as.factor(Treat), data=dat50)
Anova(mod.days50)
anova(mod.days50) # Type I Sums of Squares is the same here, so okay to use default anova in R here!
mod.ni.days50 <- lm(days ~ -1 + as.factor(Treat), data=dat50)

hist(dat50$days)
hist(log10(dat50$days))

  ##cont
cont.days50 <- lm(days~temp, data=dat50)
Anova(cont.days50)
anova(cont.days50)


# note to selves! EL_mean is percent flowering, but this is also replicated too much, best to do anova on data where each RowNumNumRep has one row of data
mod.perflow <- lm(EL_mean~as.factor(Treat), data=dat)
mod.perflow # shows means of each chamber
Anova(mod.perflow)
anova(mod.perflow)

hist(dat$EL_mean)
hist(log10(dat$EL_mean))

# mod.perflow <- lme(EL_mean~as.factor(Treat), random=~1|RowNumNumRep, data=dat)

# Try anova (on data where each RowNumNumRep has one row of data):
# max(perflow)
mod.maxperflo <- lm(max.pf_mean~as.factor(Treat), data=sumdat)
Anova(mod.maxperflo)
anova(mod.maxperflo)

hist(sumdat$max.pf_mean)
hist(log10(sumdat$max.pf_mean))

cont.maxperflo <- lm(max.pf_mean~temp, data=sumdat)
Anova(cont.maxperflo)
anova(cont.maxperflo)


# days to 50% (corr. for only those that made it that far (and do some spot-checking on those data)
# sum of all bag buds (taking mean across stems, if two stems) so basically sum of one cluster
mod.bagbuds <- lm(sum.bfall_mean~as.factor(Treat), data=sumdat)
Anova(mod.bagbuds)
anova(mod.bagbuds)
mod.ni.bagbuds <- lm(sum.bfall_mean ~ -1 + as.factor(Treat), data=sumdat)

hist(sumdat$sum.bfall_mean) # These data are pretty skewed, might be worth transforming, we can discuss later
hist(log10(sumdat$sum.bfall_mean))

cont.bagbuds <- lm(sum.bfall_mean~temp, data=sumdat)
Anova(cont.bagbuds)
anova(cont.bagbuds)


# sum of all capfall (taking mean across stems, if two stems) so basically sum of one cluster
mod.capfall <- lm(sum.capfall_mean~as.factor(Treat), data=sumdat)
An.cf <- Anova(mod.capfall)
anova(mod.capfall)

hist(sumdat$sum.capfall_mean)
hist(log10(sumdat$sum.capfall_mean))

cont.capfall <- lm(sum.capfall_mean~temp, data=sumdat)
Anova(cont.capfall)
anova(cont.capfall)

# change in length
mod.lengthchange <- lm(max.lengthchange~as.factor(Treat), data=sumdat)
Anova(mod.lengthchange)
anova(mod.lengthchange)
mod.ni.lengthchange <- lm(max.lengthchange ~ -1 + as.factor(Treat), data=sumdat)

hist(sumdat$max.lengthchange)
hist(log10(sumdat$max.lengthchange))

cont.lengthchange <- lm(max.lengthchange~temp, data=sumdat)
Anova(cont.lengthchange)
anova(cont.lengthchange)


# change in leafnum
mod.lfchange <- lm(max.lfchange~as.factor(Treat), data=sumdat)
Anova(mod.lfchange)
anova(mod.lfchange)
mod.ni.lfchange <- lm(max.lfchange ~ -1 + as.factor(Treat), data=sumdat)

hist(sumdat$max.lfchange)
hist(log10(sumdat$max.lfchange))

cont.lfchange <- lm(max.lfchange~temp, data=sumdat)
Anova(cont.lfchange)
anova(cont.lfchange)


# anova on mean soil moisture (mean per pot across time in chamber)
mod.smoist <- lm(mean.smoist~as.factor(Treat), data=sumdat)
Anova(mod.smoist)
anova(mod.smoist)
mod.ni.smoist <- lm(mean.smoist ~ -1 + as.factor(Treat), data=sumdat)


hist(sumdat$mean.smoist)
hist(log10(sumdat$mean.smoist))

cont.smoist <- lm(mean.smoist~temp, data=sumdat)
Anova(cont.smoist)
anova(cont.smoist)

#################
### Plotting ###
#################

# A few things to remember:
# a model like this:
mod.capfall <- lm(sum.capfall_mean~as.factor(Treat), data=sumdat)
# will report coefficients (coef command) as Chamber 1, then EVERY other values as relative to Chamber 1
# so you will need to adjust them to absolute values before you plot them, like this:
abs.coeff.capfall <- mod.capfall$coef
abs.coeff.capfall[2:5] <- abs.coeff.capfall[2:5] + abs.coeff.capfall[1]
# the above just adds the intercept to rows 2-5
# and you need to adjust the confint (the current ones are the confint around the relative values)
# ... by adding the intercept again to each (I am pretty sure I have this right)
abs.confint.capfall <-  confint(mod.capfall)
abs.confint.capfall[2:5,1] <- confint(mod.capfall)[2:5,1] + mod.capfall$coef[1]
abs.confint.capfall[2:5,2] <- confint(mod.capfall)[2:5,2] + mod.capfall$coef[1]
# Phew! Now, you can try plotting:
range(sumdat$sum.capfall_mean) # make sure ylim is big enough
plot(abs.coeff.capfall~as.factor(c(1:5)), ylim=c(-30, 120))
arrows(c(1:5), abs.confint.capfall[1:5,1], c(1:5), abs.confint.capfall[1:5,2], length = 0)
points(sum.capfall_mean~as.factor(Treat), data=sumdat)

# A slightly easier way to do this for just plotting is to write the model without an intercept
mod.ni.capfall <- lm(sum.capfall_mean~ - 1 + as.factor(Treat), data=sumdat) # that -1 says 'no intercept' 
# then things are absolute, not all relative ...
plot(coef(mod.ni.capfall)~as.factor(c(1:5)), ylim=c(-30, 120))
arrows(c(1:5), confint(mod.ni.capfall)[1:5,1], c(1:5), confint(mod.ni.capfall)[1:5,2], length = 0)
points(sum.capfall_mean~as.factor(Treat), data=sumdat)
nhere <- tapply(sumdat$sum.capfall_mean, sumdat$Treat, length)
text(x = as.factor(row.names(nhere)), y = confint(mod.ni.capfall)[1:5,2]+8, label = nhere, pos = 3, cex = 0.8, col = "black")
# You might notice the confidence intervals (confint) change a little, ...
# there are lots of ways to calculate confint so this is not terribly surprising
# we'll just want to be careful about how we do it for publication
# and think more before using the no intercept model for publication, but is fine at this stage!

# Bagged buds (sad buds)
range(sumdat$sum.bfall_mean) # make sure ylim is big enough
plot(coef(mod.ni.bagbuds)~as.factor(c(1:5)), ylim=c(-50, 170))
arrows(c(1:5), confint(mod.ni.bagbuds)[1:5,1] , c(1:5), confint(mod.ni.bagbuds)[1:5,2], length = 0)
points(sum.bfall_mean~as.factor(Treat), data=sumdat) # use the data used for the model!
nhere <- tapply(sumdat$sum.bfall_mean, sumdat$Treat, length)
text(x = as.factor(row.names(nhere)), y = confint(mod.ni.bagbuds)[1:5,2]+8, label = nhere, pos = 3, cex = 0.8, col = "black")


# 50% flowering (I also show here nicer axes and colored by variety)
range(dat50$days) # make sure ylim is big enough
plot(coef(mod.ni.days50)~as.factor(c(1:5)), ylim=c(30, 70), ylab="days to 50% flowering", xlab="treatment")
arrows(c(1:5), confint(mod.ni.days50)[1:5,1] , c(1:5), confint(mod.ni.days50)[1:5,2], length = 0)
points(days~as.factor(Treat), data=dat50, col=dat50$Var)
legend("topright", legend=unique(dat50$Var), col=1:length(dat50$Var), pch=1) # you can tweak this many ways, try:
?legend # to see the options
nhere <- tapply(dat50$days,dat50$Treat,length)
text(x = as.factor(row.names(nhere)), y = confint(mod.ni.days50)[1:5,2]+2, label = nhere, pos = 3, cex = 0.8, col = "black")

## Working on setting our own color palette
dat50$color <- factor(dat50$Var, levels=c("Pinot gris", "Durif1", "Syrah", "Valdepenas", "Verdelho",
    "Cabernet Sauvignon", "Sauvignon blanc", "Tempranillo", "Durif2"),
     labels=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6"))
plot(coef(mod.ni.days50)~as.factor(c(1:5)), ylim=c(30, 70), ylab="days to 50% flowering", xlab="treatment")
arrows(c(1:5), confint(mod.ni.days50)[1:5,1] , c(1:5), confint(mod.ni.days50)[1:5,2], length = 0)
points(days~as.factor(Treat), data=dat50, col=as.character(dat50$color), pch=20, cex=1.25) # careful, the as.character() for color seemed to be screwing it up
legend("topright", legend=unique(dat50$Var), col=unique(as.character(dat50$color)), pch=20, bty="n", pt.cex=c(1.25))
    # Note for above: color -> col and a couple small tweaks made it run! Also, bty=n removes the box around the legend


## change in length
range(sumdat$max.lengthchange) # make sure ylim is big enough
plot(coef(mod.ni.lengthchange)~as.factor(c(1:5)), ylim=c(0, 220))
arrows(c(1:5), confint(mod.ni.lengthchange)[1:5,1] , c(1:5), confint(mod.ni.lengthchange)[1:5,2], length = 0)
points(max.lengthchange~as.factor(Treat), data=sumdat)
nhere <- tapply(sumdat$max.lengthchange, sumdat$Treat, length)
text(x = as.factor(row.names(nhere)), y = confint(mod.ni.lengthchange)[1:5,2]+20, label = nhere, pos = 3, cex = 0.8, col = "black")

## change in leaf number
range(sumdat$max.lfchange) # make sure ylim is big enough
plot(coef(mod.ni.lfchange)~as.factor(c(1:5)), ylim=c(-1, 10))
arrows(c(1:5), confint(mod.ni.lfchange)[1:5,1] , c(1:5), confint(mod.ni.lfchange)[1:5,2], length = 0)
points(max.lfchange~as.factor(Treat), data=sumdat)
nhere <- tapply(sumdat$max.lfchange, sumdat$Treat, length)
text(x = as.factor(row.names(nhere)), y = confint(mod.ni.lfchange)[1:5,2]+1, label = nhere, pos = 3, cex = 0.8, col = "black")

## change in soimoisture # mod.smoist <- lm(mean.smoist~as.factor(Treat), data=sumdat)

range(sumdat$mean.smoist) # make sure ylim is big enough
plot(coef(mod.ni.smoist)~as.factor(c(1:5)), ylim=c(9, 22))
arrows(c(1:5), confint(mod.ni.smoist)[1:5,1] , c(1:5), confint(mod.ni.smoist)[1:5,2], length = 0)
points(mean.smoist~as.factor(Treat), data=sumdat)


##
## Older code below ... may need to be updated!
plot(mod.perflow$coef~as.factor(c(1:5)), ylim=c(-10, 20))
arrows(c(1:5), confint(mod.perflow)[1:5], c(1:5), confint(mod.perflow)[1:5,2], length = 0)
points(dat$EL_mean~as.factor(Treat), data=dat)





