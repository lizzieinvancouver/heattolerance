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

###################
## Some clean up ##
###################

dat$Var_corr <- dat$Var
dat$Var_corr[which(dat$Var_corr=="Durif1")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Durif2")] <- "Durif"
dat$Var_corr[which(dat$Var_corr=="Valdepenas")] <- "Tempranillo"
unique(dat$Var_corr) ##should we remove the NAs?

##############
## Plotting ##
##############

hist(dat$days)
hist(log10(dat$days))
plot(dat$days)

varsum <-
  ddply(dat, c("Var_corr", "days"), summarise)

