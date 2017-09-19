## Started March 20 2017 ##
## By Nicole ##

##Cleaning for heattolerance graphs 

## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

## get data
chambdater <- read.csv("input/chamberobservations_grapes2016.csv", header=TRUE)
nodespurdater <- read.csv("input/baselinespursize.csv", header=TRUE)
dater <- read.csv("input/phenmoist_grapes2016.csv", header=TRUE)
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
chambdater$stem1_bagbuds[which(chambdater$stem1_bagbuds=="")] <- 0
chambdater$stem1_bagbuds[which(chambdater$stem1_bagbuds=="-")] <- 0
chambdater$stem2_bagbuds[which(chambdater$stem2_bagbuds=="")] <- 0
chambdater$stem2_bagbuds[which(chambdater$stem2_bagbuds=="-")] <- 0
chambdater$stem1_vFcount[which(chambdater$stem1_vFcount=="")] <- 0
chambdater$stem1_vFcount[which(chambdater$stem1_vFcount=="c")] <- 0
dater$EL_stem1[which(dater$EL_stem1=="0")] <- NA
dater$EL_stem2[which(dater$EL_stem2=="0")] <- NA

# delete a couple random rows of data after checking what's in them
unique(chambdater$X)
unique(chambdater$X.1)
chambdater$X <- NULL
chambdater$X.1 <- NULL

## format date (see http://www.statmethods.net/input/dates.html)
dater$Date <- as.Date(dater$Date, format="%m/%d/%Y")
dater$days <- as.numeric(format(dater$Date, "%j"))-228 # 245 is around the start of September

chambdater$Date <- as.Date(chambdater$Date, format="%m/%d/%Y")
chambdater$days <- as.numeric(format(chambdater$Date, "%j"))-228 # 245 is around the start of September

nodespurdater$Date <- as.Date(nodespurdater$Date, format="%m/%d/%Y")
nodespurdater$days <- as.numeric(format(nodespurdater$Date, "%j"))-228 # 245 is around the start of September

## join dfs
chambdats <- join(chambdater, ids.sm, by=c("RowNum", "Num"))
dats <- join(dater, ids.sm, by=c("RowNum", "Num"))
nsdats <- join(nodespurdater, ids.sm, by=c("RowNum", "Num"))

write.csv(nsdats, file = "output/clnodespursize.csv", row.names = FALSE)

