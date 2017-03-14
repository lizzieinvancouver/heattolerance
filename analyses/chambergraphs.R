<<<<<<< Updated upstream
##Started 19 Feb 2016##
##By Nicole ##

##chamber data##

## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/Desktop/Wolkovich/analysis_wolk/")

## get data
chambdater <- read.csv("input/chamberobservations_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

## change header names here
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
names(chambdater)[names(chambdater)=="Treatment"] <- "Treat"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

# change a few things to numeric
unique(chambdater$stem1_percflow) # hypens and no entry mean zero, so update
chambdater$stem1_percflow[which(chambdater$stem1_percflow=="-")] <- 0
chambdater$stem1_percflow[which(chambdater$stem1_percflow=="")] <- 0
chambdater$stem1_percflow <- as.numeric(chambdater$stem1_percflow)

## join dfs
chambdats <- join(chambdater, ids.sm, by=c("RowNum", "Num"))

## format date (see http://www.statmethods.net/input/dates.html)
chambdats$Date <- as.Date(chambdater$Date, format="%m/%d/%Y")
chambdats$days <- as.numeric(format(chambdats$Date, "%j"))-228 # 245 is around the start of September

##get means
chambdats$length_mean <- rowMeans(chambdats[,7:8], na.rm=TRUE)
chambdats$fnum_mean <- rowMeans(chambdats[,9:10], na.rm=TRUE)
chambdats$sm_mean <- rowMeans(chambdats[15:17], na.rm=TRUE)
chambdats$pf_mean <- rowMeans(chambdats[,11:12], na.rm=TRUE)

## categorize by soil moisture
chambdats <- within(chambdats, {
  sm_cat <- NA
  sm_cat[sm_mean<=13.8]           <- "driest"
  sm_cat[sm_mean>13.8 & sm_mean<=15.5] <- "dry"
  sm_cat[sm_mean>15.5 & sm_mean<=17.2]           <- "moist"
  sm_cat[sm_mean>17.2] <- "very moist"
})

##subset
chds <- subset(chambdats, select=c(
  "Date",
  "RowNumNumRep", 
  "Var", 
  "days",
  "sm_cat",
  "length_mean",
  "fnum_mean",
  "pf_mean",
  "Treat"))

##super basic plot
ggplot(chds, aes(days, length_mean, color=Var, group=RowNumNumRep)) + geom_point(aes(size=sm_cat), shape=1) + geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots by var
ggplot(chds, aes(days, length_mean, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots length by treat
ggplot(chds, aes(days, length_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots leafnum by treat
ggplot(chds, aes(days, fnum_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")
