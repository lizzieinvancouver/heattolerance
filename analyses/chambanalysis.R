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
dat$Var_corr[which(dat$Var_corr=="Durif2")] <- "Vinhao"
dat$Var_corr[which(dat$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat$Var_corr)

dat10$Var_corr <- dat10$Var
dat10$Var_corr[which(dat10$Var_corr=="Durif1")] <- "Durif"
dat10$Var_corr[which(dat10$Var_corr=="Durif2")] <- "Vinhao"
dat10$Var_corr[which(dat10$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat10$Var_corr)

dat50$Var_corr <- dat50$Var
dat50$Var_corr[which(dat50$Var_corr=="Durif1")] <- "Durif"
dat50$Var_corr[which(dat50$Var_corr=="Durif2")] <- "Vinhao"
dat50$Var_corr[which(dat50$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat50$Var_corr)

sumdat$Var_corr <- sumdat$Var
sumdat$Var_corr[which(sumdat$Var_corr=="Durif1")] <- "Durif"
sumdat$Var_corr[which(sumdat$Var_corr=="Durif2")] <- "Vinhao"
sumdat$Var_corr[which(sumdat$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(sumdat$Var_corr)

####################
##Treatment colors##
####################

# Set up plotting colors, symbols and treatment labels
display.brewer.all() # these are all the RColorBrewer palettes
treatcol <- rev(brewer.pal(10, "Spectral")[1:5])
varcol <- brewer.pal(11, "RdYlGn")[-c(6:7)]
varcol[5] <- "#AF8DC3"
varsym <- c(0, 1, 3, 4, 6, 7, 15, 16, 18)
varsym2 <- rep(c(0:2), 4)
daynightemp <- c(expression(paste("17/23",degree,"C")),
    expression(paste("23/29",degree,"C")),
    expression(paste("27/33",degree,"C")),
    expression(paste("31/37",degree,"C")),
    expression(paste("34/40",degree,"C")))
    
########################
###Additions to plots### 
###  (after meeting  ###
###  with coauthors)  ##
########################

##Duration flowering 10-50%
#clarify column names for joining
names(dat10)[names(dat10)=="days"] <- "days10"
names(dat50)[names(dat50)=="days"] <- "days50"
#join dat10 and dat50
datflo <- join(dat10, dat50, by=c("RowNumNumRep"))
#create and fill flowering duration column
datflo$flodur <- NA
datflo$flodur <- datflo$days50 - datflo$days10
#plot
##plot(flodur~temp, data=datflo)
df.sub <- subset (datflo, select=c("RowNumNumRep", "stage", "Treat", "temp", "Var_corr", "flodur"), )
fdmeans <-
      ddply(df.sub, c("Treat", "temp"), summarise,
      mean = mean(flodur),
      max = max(flodur),
      sd = sd(flodur),
      n = length(flodur),
      sem = sd(flodur)/sqrt(length(flodur)))

# Set up the blank plot, then do each data point; then plot the means and errors
pdf(file.path("graphs/chamber_flodur.pdf"), width = 8, height = 7)
plot(mean~temp, data=fdmeans, ylim=c(0, 15), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Duration of flowering (days)")
axis(1, at=fdmeans$temp, labels=fdmeans$temp)

for (treatnum in c(1:length(unique(df.sub$temp)))){
    subtreat <- subset(df.sub, temp==sort(unique(df.sub$temp))[treatnum])
    points(flodur~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=fdmeans, ylim=c(0, 15), lwd=2)
arrows(fdmeans$temp, fdmeans$mean-fdmeans$sem,  fdmeans$temp,
    fdmeans$mean+fdmeans$sem, length = 0, lwd=2)
text(x = fdmeans$temp, y = fdmeans$max+1.5,
     label = fdmeans$n, pos = 3, cex = 0.8, col = "black")
dev.off()

##percent buds excised
sumdat$totalbuds <- NA
sumdat$totalbuds <- sumdat$sum.bfall_mean + sumdat$sum.capfall_mean
sumdat$perbudex <- NA
sumdat$perbudex <- (sumdat$sum.bfall_mean/sumdat$totalbuds)*100
#plot
plot(perbudex~temp, data=sumdat)

##leaf number / stem length
plot((max.lengthchange/max.lfchange)~temp, data = sumdat)

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

##
##
## Playing around with making percent flowering figures ... and bagged buds
##
##

# Set up plotting colors, symbols and treatment labels
display.brewer.all() # these are all the RColorBrewer palettes
treatcol <- rev(brewer.pal(10, "Spectral")[1:5])
varcol <- brewer.pal(11, "RdYlGn")[-c(6:7)]
varcol[5] <- "#AF8DC3"
varsym <- c(0, 1, 3, 4, 6, 7, 15, 16, 18)
varsym2 <- rep(c(0:2), 4)
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
# Colored by treatment...
pdf(file.path("graphs/chamber_bagbudsfin.pdf"), width = 8, height = 7)
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
dev.off()

# Colored by variety
pdf(file.path("graphs/chamber_bagbudsfinvar.pdf"), width = 8, height = 7)
plot(mean~temp, data=bbudsmeans, ylim=c(-5, 180), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Flower buds lost")
axis(1, at=bbudsmeans$temp, labels=bbudsmeans$temp)

for (varname in c(1:length(unique(sumdat$Var_corr)))){
    subvar <- subset(sumdat, Var_corr==sort(unique(sumdat$Var_corr))[varname])
    points(sum.bfall_mean~temp, data=subvar, col=varcol[varname], pch=varsym2[varname], lwd=1.5)
    }
legend("topleft", legend=sort(unique(sumdat$Var_corr)), col=varcol,  
    pch=varsym2[1:length(unique(sumdat$Var_corr))], bty="n")

points(mean~temp, data=bbudsmeans, ylim=c(-5, 180), lwd=2)
arrows(bbudsmeans$temp, bbudsmeans$mean-bbudsmeans$sem,  bbudsmeans$temp,
    bbudsmeans$mean+bbudsmeans$sem, length = 0, lwd=2)
nhere <- tapply(sumdat$sum.capfall_mean, sumdat$Treat, length)
text(x = bbudsmeans$temp, y = bbudsmeans$max+5,
     label = bbudsmeans$n, pos = 3, cex = 0.8, col = "black")
dev.off()

## and stem length change
lenmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(max.lengthchange),
      max = max(max.lengthchange),
      sd = sd(max.lengthchange),
      n = length(max.lengthchange),
      sem = sd(max.lengthchange)/sqrt(length(max.lengthchange)))

# Set up the blank plot, then do each data point; then plot the means and errors
pdf(file.path("graphs/chamber_stemlenfin.pdf"), width = 8, height = 7)
plot(mean~temp, data=lenmeans, ylim=c(-5, 250), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Change in stem length (mm)")
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
dev.off()
     
     
## change in leaf number

lfmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(max.lfchange),
      max = max(max.lfchange),
      sd = sd(max.lfchange),
      n = length(max.lfchange),
      sem = sd(max.lfchange)/sqrt(length(max.lfchange)))

# Set up the blank plot, then do each data point; then plot the means and errors
pdf(file.path("graphs/chamber_leafnumfin.pdf"), width = 8, height = 7)
plot(mean~temp, data=lfmeans, ylim=c(0, 15), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Change in leaf number")
axis(1, at=lfmeans$temp, labels=lfmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(max.lfchange~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=lfmeans, ylim=c(0, 15), lwd=2)
arrows(lfmeans$temp, lfmeans$mean-lfmeans$sem,  lfmeans$temp,
    lfmeans$mean+lfmeans$sem, length = 0, lwd=2)
text(x = lfmeans$temp, y = lfmeans$max+1.5,
     label = lfmeans$n, pos = 3, cex = 0.8, col = "black")
dev.off()

## change stem length v change leaf num

pdf(file.path("graphs/chamber_lfylnxchange.pdf"), width = 8, height = 7)
plot(max.lfchange~max.lengthchange, data=sumdat, xlim=c(-5, 220), ylim=c(0, 15), xlab="Change in stem length (mm)", ylab="Change in leaf number")
for (treatnum in c(1:length(unique(sumdat$temp)))){
  subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
  points(max.lfchange~max.lengthchange, data=subtreat, col=treatcol[treatnum], lwd=2)
}
legend("topleft", legend=daynightemp, pch=16, 
       col=treatcol,  bty="n")
dev.off()

#switch axis

pdf(file.path("graphs/chamber_lfxlnychange.pdf"), width = 8, height = 7)
plot(max.lengthchange~max.lfchange, data=sumdat, xlim=c(0, 10), ylim=c(-5, 250), xlab="Change in leaf number", ylab="Change in stem length (mm)")
for (treatnum in c(1:length(unique(sumdat$temp)))){
  subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
  points(max.lengthchange~max.lfchange, data=subtreat, col=treatcol[treatnum], lwd=2)
}
legend("topleft", legend=daynightemp, pch=16, 
       col=treatcol,  bty="n")
dev.off()

## mean soil moisture

sumdat$corr_mean.smoist <- NA
sumdat$corr_mean.smoist <- 44.474+1.836*(sumdat$mean.smoist)


smmeans <-
      ddply(sumdat, c("Treat", "temp"), summarise,
      mean = mean(corr_mean.smoist),
      max = max(corr_mean.smoist),
      sd = sd(corr_mean.smoist),
      n = length(corr_mean.smoist),
      sem = sd(corr_mean.smoist)/sqrt(length(corr_mean.smoist)))




# Set up the blank plot, then do each data point; then plot the means and errors
pdf(file.path("graphs/chamber_smfin.pdf"), width = 8, height = 7)
plot(mean~temp, data=smmeans, ylim=c(65,80), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Mean percent soil moisture")
axis(1, at=smmeans$temp, labels=smmeans$temp)

for (treatnum in c(1:length(unique(sumdat$temp)))){
    subtreat <- subset(sumdat, temp==sort(unique(sumdat$temp))[treatnum])
    points(corr_mean.smoist~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=smmeans, ylim=c(65, 80), lwd=2)
arrows(smmeans$temp, smmeans$mean-smmeans$sem,  smmeans$temp,
    smmeans$mean+smmeans$sem, length = 0, lwd=2)
text(x = smmeans$temp, y = smmeans$max+1.5,
     label = smmeans$n, pos = 3, cex = 0.8, col = "black")
dev.off()

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

pdf(file.path("graphs/chamber_10percfin.pdf"), width = 8, height = 7)
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
dev.off()
     
##days to 50% flowering     
ffmeans <-
      ddply(dat50, c("Treat", "temp"), summarise,
      mean = mean(days),
      max = max(days),
      sd = sd(days),
      n = length(days),
      sem = sd(days)/sqrt(length(days)))

pdf(file.path("graphs/chamber_50percfin.pdf"), width = 8, height = 7)
plot(mean~temp, data=ffmeans, ylim=c(35, 65), type="n", xaxt="n",
    xlab=expression(paste("Mean chamber temperature (",degree,"C)")),
    ylab="Days to 50% flowering")
axis(1, at=ffmeans$temp, labels=ffmeans$temp)

for (treatnum in c(1:length(unique(dat50$temp)))){
    subtreat <- subset(dat50, temp==sort(unique(dat50$temp))[treatnum])
    points(days~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
    }
legend("topleft", legend=daynightemp, pch=16, 
    col=treatcol,  bty="n")

points(mean~temp, data=ffmeans, ylim=c(35, 65), lwd=2)
arrows(ffmeans$temp, ffmeans$mean-ffmeans$sem,  ffmeans$temp,
    ffmeans$mean+ffmeans$sem, length = 0, lwd=2)
text(x = ffmeans$temp, y = ffmeans$max+5,
     label = ffmeans$n, pos = 3, cex = 0.8, col = "black")
dev.off()
          
##     

write.csv(tfmeans, file="output/tenpercentstat.csv", row.names = FALSE)
write.csv(ffmeans, file="output/fiftypercentstat.csv", row.names = FALSE)
write.csv(lenmeans, file="output/stemlengthstat.csv", row.names = FALSE)
write.csv(lfmeans, file="output/leafnumstat.csv", row.names = FALSE)
write.csv(bbudsmeans, file="output/bagbudstat.csv", row.names = FALSE)

