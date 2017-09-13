## Started 22 August 2017 ##
## By Lizzie and Nicole ##


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

dat <- read.csv(file="output/clghdata.csv", header = TRUE)
rmidat <- read.csv(file="input/bblo_2014to2015.csv", header=TRUE) # this file is created in the vin repo in davis/analyses/davisphen_analysis.R; I just copied it and put it here
rmidat <- subset(rmidat, select=c("variety",  "n.2015", "sd.2015",  "doy.2015", "se.2015")) # we should only use the 2015 data

###################
## Some clean up ##
###################

dat$Var_corr <- dat$Var
dat$Var_corr[which(dat$Var_corr=="Durif1")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Durif2")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Valdepenas")] <- "Tempranillo"
dat$Var_corr[which(dat$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
dat$Var_corr[which(dat$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
dat$Var_corr[which(dat$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

unique(dat$Var_corr) 

rmidat$variety[which(rmidat$variety=="Ungni blanc/Trebbiano")] <- "Ugni blanc/Trebbiano"
rmi <- rmidat[which(rmidat$variety %in% c(unique(dat$Var_corr), "Valdepenas")),]
rmi$variety[which(rmi$variety=="Valdepenas")] <- "Tempranillo"

setdiff(unique(dat$Var_corr),unique(rmi$variety))

##############
## Plotting ##
##############

##50% flowering
hist(dat$days)
hist(log10(dat$days))
plot(dat$days)
ggplot(dat, aes(days, Var_corr, color=Var_corr)) + 
    geom_point()

ggplot(rmi, aes(doy.2015, variety, color=variety)) +
    geom_point()

# If you want ggplot to order by days ...
dat$var.ord <- factor(dat$Var_corr, levels = dat$Var_corr[order(dat$days)])
ggplot(dat, aes(days, var.ord, color=var.ord)) + 
    geom_point()

rmi$var.ord <- factor(rmi$variety, levels = rmi$variety[order(rmi$doy.2015)])
ggplot(rmi, aes(doy.2015, var.ord, color=var.ord)) +
    geom_point()

varsum <-
  ddply(dat, c("Var_corr", "days"), summarise)


##budburst

ggplot(dat, aes(days.to.bb, Var_corr, color=Var_corr)) + 
  geom_point()

dat$var.ord <- factor(dat$Var_corr, levels = dat$Var_corr[order(dat$days.to.bb)])
ggplot(dat, aes(days.to.bb, var.ord, color=var.ord)) + 
  geom_point()

##leafout

ggplot(dat, aes(days.to.lo, Var_corr, color=Var_corr)) + 
  geom_point()

dat$var.ord <- factor(dat$Var_corr, levels = dat$Var_corr[order(dat$days.to.lo)])
ggplot(dat, aes(days.to.lo, var.ord, color=var.ord)) + 
  geom_point()