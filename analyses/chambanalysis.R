## Started 17 May 2017 ##
## By Lizzie and Nicole! ##

## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# Get packages
library(ggplot2)
library(car)
library(colorspace)
library(RColorBrewer) # makes nice colors
library(plyr)
library(dplyr)

dat <- read.csv ("output/clchambdata.csv", header=TRUE)
dat10 <- read.csv("output/chamb10fl.csv", header=TRUE)
dat50 <- read.csv ("output/chamb50fl.csv", header=TRUE)
sumdat <- read.csv ("output/chdatsum.csv", header=TRUE)

###################
## Some clean up ##
###################

dat$Var_corr <- dat$Var
dat$Var_corr[which(dat$Var_corr=="Durif1")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Durif2")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat$Var_corr)

dat10$Var_corr <- dat10$Var
dat10$Var_corr[which(dat10$Var_corr=="Durif1")] <- "Durif"
dat10$Var_corr[which(dat10$Var_corr=="Durif2")] <- "Durif"
dat10$Var_corr[which(dat10$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat10$Var_corr)

dat50$Var_corr <- dat50$Var
dat50$Var_corr[which(dat50$Var_corr=="Durif1")] <- "Durif"
dat50$Var_corr[which(dat50$Var_corr=="Durif2")] <- "Durif"
dat50$Var_corr[which(dat50$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat50$Var_corr)

sumdat$Var_corr <- sumdat$Var
sumdat$Var_corr[which(sumdat$Var_corr=="Durif1")] <- "Durif"
sumdat$Var_corr[which(sumdat$Var_corr=="Durif2")] <- "Durif"
sumdat$Var_corr[which(sumdat$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(sumdat$Var_corr)


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


## 10 flowering
mod.days10 <- lm(days~as.factor(Treat), data=dat10)
Anova(mod.days10)
anova(mod.days10) # Type I Sums of Squares is the same here, so okay to use default anova in R here!
mod.ni.days10 <- lm(days ~ -1 + as.factor(Treat), data=dat10)

hist(dat10$days)

cont.days10 <- lm(days~temp, data=dat10)
Anova(cont.days10)
anova(cont.days10)

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
dat50$color <- factor(dat50$Var_corr, levels=c("Pinot gris", "Durif", "Syrah", "Verdelho",
    "Cabernet Sauvignon", "Sauvignon blanc", "Tempranillo"),
     labels=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f"))
plot(coef(mod.ni.days50)~as.factor(c(1:5)), ylim=c(30, 70), ylab="days to 50% flowering", xlab="treatment")
arrows(c(1:5), confint(mod.ni.days50)[1:5,1] , c(1:5), confint(mod.ni.days50)[1:5,2], length = 0)
points(days~as.factor(Treat), data=dat50, col=as.character(dat50$color), pch=20, cex=1.25) # careful, the as.character() for color seemed to be screwing it up
legend("topright", legend=unique(dat50$Var_corr), col=unique(as.character(dat50$color)), pch=20, bty="n", pt.cex=c(1.25))

##Copy of above for 10% flowering for kicks
dat10$color <- factor(dat10$Var_corr, levels=c("Pinot gris", "Durif", "Syrah", "Verdelho",
    "Cabernet Sauvignon", "Sauvignon blanc", "Tempranillo", "Gewurtztraminer"),
     labels=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"))
plot(coef(mod.ni.days10)~as.factor(c(1:5)), ylim=c(30, 70), ylab="days to 10% flowering", xlab="treatment")
arrows(c(1:5), confint(mod.ni.days10)[1:5,1] , c(1:5), confint(mod.ni.days10)[1:5,2], length = 0)
points(days~as.factor(Treat), data=dat10, col=as.character(dat10$color), pch=20, cex=1.25) # careful, the as.character() for color seemed to be screwing it up
legend("topright", legend=unique(dat10$Var_corr), col=unique(as.character(dat10$color)), pch=20, bty="n", pt.cex=c(1.25))
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
##
## Playing around with making percent flowering figures ... and bagged buds
##
##

# Set up plotting colors, symbols and treatment labels
display.brewer.all() # these are all the RColorBrewer palettes
treatcol <- rev(brewer.pal(10, "Spectral")[1:5])
varsym <- c(0, 1, 3, 4, 6, 7, 15, 16, 18)
daynightemp <- c(expression(paste("17/23",degree,"C")),
    expression(paste("23/29",degree,"C")),
    expression(paste("27/33",degree,"C")),
    expression(paste("31/37",degree,"C")),
    expression(paste("34/40",degree,"C")))

# 
##############################
## A few plots through time ##
##############################

# First, summarize the data: Here by Treatment and variety
pfvarsummary <-
      ddply(dat, c("Treat", "Var_corr", "days"), summarise,
      mean = mean(pf_mean),
      sd = sd(pf_mean),
      sem = sd(pf_mean)/sqrt(length(pf_mean)))

# Next, make an empty plot
plot(mean~days, data=pfvarsummary, type="n")
# Now add lines -- each color is a treatment
# And each symbol is a variety
for (treatnum in c(1:length(unique(pfvarsummary$Treat)))){
    subtreat <- subset(pfvarsummary, Treat==unique(pfvarsummary$Treat)[treatnum])
    subtreat$Var_corr <- as.factor(subtreat$Var_corr)
    points(mean~days, data=subtreat, col=treatcol[treatnum],
        pch=varsym[subtreat$Var_corr])
    lines(mean~days, data=subtreat, col=treatcol[treatnum])
    }

# Easier to just show treatments and skip varieties...
pfsummary <-
      ddply(dat, c("Treat", "days"), summarise,
      mean = mean(pf_mean),
      sd = sd(pf_mean),
      sem = sd(pf_mean)/sqrt(length(pf_mean)))
   
plot(mean~days, data=pfsummary, type="n")

for (treatnum in c(1:length(unique(pfsummary$Treat)))){
    subtreat <- subset(pfsummary, Treat==unique(pfsummary$Treat)[treatnum])
    points(mean~days, data=subtreat, col=treatcol[treatnum])
    lines(mean~days, data=subtreat, col=treatcol[treatnum])
    }
legend("topleft", legend=unique(pfsummary$Treat), lty=1, 
    col=treatcol,  bty="n")

# Repeat above for bagged buds...
bbudssummary <-
      ddply(dat, c("Treat", "days"), summarise,
      mean = mean(bagbuds),
      sd = sd(bagbuds),
      sem = sd(bagbuds)/sqrt(length(bagbuds)))
   
plot(mean~days, data=bbudssummary, type="n")

for (treatnum in c(1:length(unique(bbudssummary$Treat)))){
    subtreat <- subset(bbudssummary, Treat==unique(bbudssummary$Treat)[treatnum])
    points(mean~days, data=subtreat, col=treatcol[treatnum])
    lines(mean~days, data=subtreat, col=treatcol[treatnum])
    }
legend("topleft", legend=unique(bbudssummary$Treat), lty=1, 
    col=treatcol,  bty="n")

########################################
## Showing means and raw data overall ##
########################################

# First, summarize the data, without days: Here by Treatment and variety
# We should do this also with 25% flowering
bbudsvarmeans <-
      ddply(sumdat, c("Treat", "temp", "Var_corr"), summarise,
      mean = mean(sum.bfall_mean),
      sd = sd(sum.bfall_mean),
      n = length(sum.bfall_mean),
      sem = sd(sum.bfall_mean)/sqrt(length(sum.bfall_mean)))
# This shows we only have reps of Durif and Tempranillo within a treatment

bbudsmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(sum.bfall_mean),
      max = max(sum.bfall_mean), # used to plot the sample number values
      sd = sd(sum.bfall_mean),
      n = length(sum.bfall_mean),
      sem = sd(sum.bfall_mean)/sqrt(length(sum.bfall_mean)))

# Set up the blank plot, then do each data point; then plot the means and errors
plot(mean~temp, data=bbudsmeans, ylim=c(-5, 180), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Flower buds lost")
axis(1, at=bbudsmeans$temp, labels=bbudsmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(sum.bfall_mean~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=bbudsmeans, ylim=c(-5, 180), lwd=2)
arrows(bbudsmeans$temp, bbudsmeans$mean-bbudsmeans$sem,  bbudsmeans$temp,
    bbudsmeans$mean+bbudsmeans$sem, length = 0, lwd=2)
nhere <- tapply(sumdat$sum.capfall_mean, sumdat$Treat, length)
text(x = bbudsmeans$temp, y = bbudsmeans$max+5,
     label = bbudsmeans$n, pos = 3, cex = 0.8, col = "black")

## and stem length change
lenmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(max.lengthchange),
      max = max(max.lengthchange),
      sd = sd(max.lengthchange),
      n = length(max.lengthchange),
      sem = sd(max.lengthchange)/sqrt(length(max.lengthchange)))

# Set up the blank plot, then do each data point; then plot the means and errors
plot(mean~temp, data=lenmeans, ylim=c(-5, 250), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Change in stem length")
axis(1, at=lenmeans$temp, labels=lenmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(max.lengthchange~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=lenmeans, ylim=c(-5, 250), lwd=2)
arrows(lenmeans$temp, lenmeans$mean-lenmeans$sem,  lenmeans$temp,
    lenmeans$mean+lenmeans$sem, length = 0, lwd=2)
text(x = lenmeans$temp, y = lenmeans$max+10,
     label = lenmeans$n, pos = 3, cex = 0.8, col = "black")
     
     
## change in leaf number

lfmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(max.lfchange),
      max = max(max.lfchange),
      sd = sd(max.lfchange),
      n = length(max.lfchange),
      sem = sd(max.lfchange)/sqrt(length(max.lfchange)))

# Set up the blank plot, then do each data point; then plot the means and errors
plot(mean~temp, data=lfmeans, ylim=c(-5, 15), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Change in leaf number")
axis(1, at=lfmeans$temp, labels=lfmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(max.lfchange~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=lfmeans, ylim=c(-5, 15), lwd=2)
arrows(lfmeans$temp, lfmeans$mean-lfmeans$sem,  lfmeans$temp,
    lfmeans$mean+lfmeans$sem, length = 0, lwd=2)
text(x = lfmeans$temp, y = lfmeans$max+1.5,
     label = lfmeans$n, pos = 3, cex = 0.8, col = "black")


## mean soil moisture

smmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(mean.smoist),
      max = max(mean.smoist),
      sd = sd(mean.smoist),
      n = length(mean.smoist),
      sem = sd(mean.smoist)/sqrt(length(mean.smoist)))

# Set up the blank plot, then do each data point; then plot the means and errors
plot(mean~temp, data=smmeans, ylim=c(10,25), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Mean soil moisture")
axis(1, at=smmeans$temp, labels=smmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(mean.smoist~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=smmeans, ylim=c(10, 25), lwd=2)
arrows(smmeans$temp, smmeans$mean-smmeans$sem,  smmeans$temp,
    smmeans$mean+smmeans$sem, length = 0, lwd=2)
text(x = smmeans$temp, y = smmeans$max+1.5,
     label = smmeans$n, pos = 3, cex = 0.8, col = "black")


## max percent flowering

pfmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(max.pf_mean),
      max = max(max.pf_mean),
      sd = sd(max.pf_mean),
      n = length(max.pf_mean),
      sem = sd(max.pf_mean)/sqrt(length(max.pf_mean)))

plot(mean~temp, data=pfmeans, ylim=c(-5, 150), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Maximum percent flowering")
axis(1, at=pfmeans$temp, labels=pfmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(max.pf_mean~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=pfmeans, ylim=c(-5, 150), lwd=2)
arrows(pfmeans$temp, pfmeans$mean-pfmeans$sem,  pfmeans$temp,
    pfmeans$mean+pfmeans$sem, length = 0, lwd=2)
text(x = pfmeans$temp, y = pfmeans$max+8,
     label = pfmeans$n, pos = 3, cex = 0.8, col = "black")


## days to 10% flowering

tfmeans <-
      ddply(dat10, c("Treat", "temp"), summarise,
      mean = mean(days),
      max = max(days),
      sd = sd(days),
      n = length(days),
      sem = sd(days)/sqrt(length(days)))

plot(mean~temp, data=tfmeans, ylim=c(30, 70), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Days to 10% flowering")
axis(1, at=tfmeans$temp, labels=tfmeans$temp)

for (treatnum in c(1:length(unique(dat10$temp)))){
    subtreat <- subset(dat10, temp==sort(unique(dat10$temp))[treatnum])
    points(days~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=tfmeans, ylim=c(30, 70), lwd=2)
arrows(tfmeans$temp, tfmeans$mean-tfmeans$sem,  tfmeans$temp,
    tfmeans$mean+tfmeans$sem, length = 0, lwd=2)
text(x = tfmeans$temp, y = tfmeans$max+5,
     label = tfmeans$n, pos = 3, cex = 0.8, col = "black")
     
##days to 50% flowering     
ffmeans <-
      ddply(dat50, c("Treat", "temp"), summarise,
      mean = mean(days),
      max = max(days),
      sd = sd(days),
      n = length(days),
      sem = sd(days)/sqrt(length(days)))

plot(mean~temp, data=ffmeans, ylim=c(30, 70), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Days to 50% flowering")
axis(1, at=ffmeans$temp, labels=ffmeans$temp)

for (treatnum in c(1:length(unique(dat50$temp)))){
    subtreat <- subset(dat50, temp==sort(unique(dat50$temp))[treatnum])
    points(days~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=ffmeans, ylim=c(30, 70), lwd=2)
arrows(ffmeans$temp, ffmeans$mean-ffmeans$sem,  ffmeans$temp,
    ffmeans$mean+ffmeans$sem, length = 0, lwd=2)
text(x = ffmeans$temp, y = ffmeans$max+5,
     label = ffmeans$n, pos = 3, cex = 0.8, col = "black")
          
##     

write.csv(tfmeans, file="output/tenpercentstat.csv", row.names = FALSE)
write.csv(ffmeans, file="output/fiftypercentstat.csv", row.names = FALSE)
write.csv(lenmeans, file="output/stemlengthstat.csv", row.names = FALSE)
write.csv(lfmeans, file="output/leafnumstat.csv", row.names = FALSE)
write.csv(bbudsmeans, file="output/bagbudstat.csv", row.names = FALSE)

