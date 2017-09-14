## Started 14 September 2017 ##
## By Lizzie ##

## Copied from davisphen_analysis.R mostly #
## but adjusted to put Valdepenas and Tempranillo together ##

# housekeeping
rm(list=ls())
options(stringsAsFactors=FALSE)

# packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

# simple (fx)s

avgerageplants <- function(datafile, varcolname){
    dat.var <- ddply(datafile, c(varcolname), summarise,
    n = sum(n),
    nplants = length(doy),
    sd = sd(doy, na.rm=TRUE),                   
    doy = mean(doy, na.rm=TRUE))
    dat.var$se <- dat.var$sd/sqrt(dat.var$nplants)
    return(dat.var)
}

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# get the data
# budburst, leafout ..
bb15 <- read.csv("input/budburst_est_2015.csv", header=TRUE)
lo15 <- read.csv("input/leafout_est_2015.csv", header=TRUE)

###################
## Some clean up ##
###################
bb15$variety[which(bb15$variety=="Valdepenas")] <- "Tempranillo"
lo15$variety[which(lo15$variety=="Valdepenas")] <- "Tempranillo"

## Get rid of the NAs ##
bb15.noNA <- subset(bb15, is.na(doy)==FALSE)
lo15.noNA <- subset(lo15, is.na(doy)==FALSE)

###################
## Get mean by plants ##
###################
bb15.var <- avgerageplants(bb15.noNA, "variety")
lo15.var <- avgerageplants(lo15.noNA, "variety")

bblo <- full_join(bb15.var, lo15.var, by="variety", suffix = c(".bb", ".lo"))

## write it out ##
write.csv(bblo, "output/bblo_2015syn.csv", row.names=FALSE)
