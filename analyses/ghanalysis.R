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

dat4_7 <- read.csv(file="output/ghestphen4_7.csv", header = TRUE)
dat30_50 <- read.csv(file="output/ghestphen30_50.csv", header = TRUE)
gendat <- read.csv(file="output/clghdata.csv", header = TRUE)
rmidat <- read.csv(file="output/bblo_2015syn.csv", header=TRUE) # see data/README_rmi.txt

###################
## Some clean up ##
###################

dat4_7$Var_corr <- dat4_7$Var
dat4_7$Var_corr[which(dat4_7$Var_corr=="Durif1")] <- "Durif"
dat4_7$Var_corr[which(dat4_7$Var_corr=="Durif2")] <- "Durif"
dat4_7$Var_corr[which(dat4_7$Var_corr=="Valdepenas")] <- "Tempranillo"
dat4_7$Var_corr[which(dat4_7$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
dat4_7$Var_corr[which(dat4_7$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
dat4_7$Var_corr[which(dat4_7$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"



dat30_50$Var_corr <- dat30_50$Var
dat30_50$Var_corr[which(dat30_50$Var_corr=="Durif1")] <- "Durif"
dat30_50$Var_corr[which(dat30_50$Var_corr=="Durif2")] <- "Durif"
dat30_50$Var_corr[which(dat30_50$Var_corr=="Valdepenas")] <- "Tempranillo"
dat30_50$Var_corr[which(dat30_50$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
dat30_50$Var_corr[which(dat30_50$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
dat30_50$Var_corr[which(dat30_50$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

unique(dat30_50$Var_corr)

gendat$Var_corr <- gendat$Var
gendat$Var_corr[which(gendat$Var_corr=="Durif1")] <- "Durif"
gendat$Var_corr[which(gendat$Var_corr=="Durif2")] <- "Durif"
gendat$Var_corr[which(gendat$Var_corr=="Valdepenas")] <- "Tempranillo"
gendat$Var_corr[which(gendat$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
gendat$Var_corr[which(gendat$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
gendat$Var_corr[which(gendat$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

unique(gendat$Var_corr) 

rmidat$variety[which(rmidat$variety=="Ungni blanc/Trebbiano")] <- "Ugni blanc/Trebbiano"
rmi <- rmidat[which(rmidat$variety %in% c(unique(gendat$Var_corr), "Valdepenas")),]
rmi$variety[which(rmi$variety=="Valdepenas")] <- "Tempranillo"

setdiff(unique(gendat$Var_corr),unique(rmi$variety))

##############
## Plotting ##
##############

##50% flowering
hist(dat30_50$days.to.50)
hist(log10(dat30_50$days.to.50))
plot(dat30_50$days.to.50)
ggplot(dat, aes(days.to.50, Var_corr, color=Var_corr)) + 
    geom_point()

ggplot(rmi, aes(doy.2015, variety, color=variety)) +
    geom_point()

# If you want ggplot to order by days ...
dat30_50$var.ord <- factor(dat30_50$Var_corr, levels = dat30_50$Var_corr[order(dat30_50$days.to.50)])
ggplot(dat, aes(days.to.50, var.ord, color=var.ord)) + 
    geom_point()

rmi$var.ord <- factor(rmi$variety, levels = rmi$variety[order(rmi$doy.2015)])
ggplot(rmi, aes(doy.2015, var.ord, color=var.ord)) +
    geom_point()

varsum <-
  ddply(dat30_50, c("Var_corr", "days.to.50"), summarise)


##budburst

ggplot(dat4_7, aes(days.to.bb, Var_corr, color=Var_corr)) + 
  geom_point()

dat4_7$var.ord <- factor(dat4_7$Var_corr, levels = dat4_7$Var_corr[order(dat4_7$days.to.bb)])
ggplot(dat, aes(days.to.bb, var.ord, color=var.ord)) + 
  geom_point()

##leafout

ggplot(dat4_7, aes(days.to.lo, Var_corr, color=Var_corr)) + 
  geom_point()

dat4_7$var.ord <- factor(dat4_7$Var_corr, levels = dat4_7$Var_corr[order(dat4_7$days.to.lo)])
ggplot(dat, aes(days.to.lo, var.ord, color=var.ord)) + 
  geom_point()

