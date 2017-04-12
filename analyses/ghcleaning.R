## Started April 9 2017 ##
## By Nicole ##

##cleaning for greenhouse graphs

## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# get data
dater <- read.csv("input/phenmoist_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

## change header names here
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

##cleaning out non numerics
dater$EL_stem1[which(dater$EL_stem1=="0")] <- NA
dater$EL_stem2[which(dater$EL_stem2=="0")] <- NA

## format date (see http://www.statmethods.net/input/dates.html)
dater$Date <- as.Date(dater$Date, format="%m/%d/%Y")
dater$days <- as.numeric(format(dater$Date, "%j"))-228 # 245 is around the start of September

## join dfs
dats <- join(dater, ids.sm, by=c("RowNum", "Num"))

write.csv(dats, file="output/clghdata.csv", row.names = FALSE)







