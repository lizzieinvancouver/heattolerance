## Started 20 March 2018 ##
## from napaclim (in Nacho's Pheno_projections repo) ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)

## set working directory
# setwd("~/GitHub/heattolerance/analyses/")
# source("~/GitHub/heattolerance/analyses/wangengelcurve/source/Script_functions_pheno_models.R")
setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses")
source("wangengelcurve/source/Script_functions_pheno_models.R")

testclim.min <- seq(-5, 40, by=0.25)
testclim.max <- seq(-4, 41, by=0.25) # higher than 41 and the curve freaks out
testclim.avg <- (testclim.min+testclim.max)/2
testclim.avg.F <- testclim.avg*(9/5) + 32

# Nacho has an alpha f(x) but I am not sure when to use it
testalpha <- Alphafx(testclim.min, testclim.max, 29)

# Wang & Engel model seems to want static values for first 4 inputs:
# WangEngelfx <- function(Tmin, Tmax, Topt, Alpha, Tavg)
wangeng24 <- WangEngelfx(0, 41, 24.3, 2.6, testclim.avg)
wangeng27 <- WangEngelfx(0, 41, 27, 2.85, testclim.avg)

# Now just format and plot
wangeng24clim <- data.frame(we=wangeng24[,1], tempC=testclim.avg,
   temp=testclim.avg)
wangeng27clim <- data.frame(we=wangeng27[,1], tempC=testclim.avg,
   temp=testclim.avg)

wangeng24clim.sm <- subset(wangeng24clim, we>=0)
wangeng27clim.sm <- subset(wangeng27clim, we>=0)

plot(we~temp, data=wangeng27clim.sm, type="l", xlim=c(0,40))
points(we~temp, data=wangeng24clim.sm, type="l", col="red")
abline(v=c(20, 26, 30, 34, 37), col="darkslategray2")

## for Nicole's work: Some simple curves
pdf(file.path("graphs/wengeng_possible.pdf"), width = 8, height = 7)
plot(we~tempC, data=wangeng27clim.sm,  xlim=c(-5,42), ylab="Developmental rate",
     xlab=expression(paste("Temperature "( degree~C))), type="n")
abline(v=c(20, 26, 30, 34, 37), col="darkslategray2")
points(we~tempC, data=wangeng27clim.sm, type="l", lwd=2, col="red")
points(we~tempC, data=wangeng24clim.sm, type="l", lwd=2, col="blue")
dev.off()
