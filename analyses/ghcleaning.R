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

# this corrects for the fact that not all of the plants could be sampled in one day, so the code was calculating averages per day, and if the second day plants were not developing as quickly as the first day plants, it would look like the average was dropping
d$sampleday <- d$days
d$sampleday[d$sampleday == 8] <- 7
d$sampleday[d$sampleday == 15] <- 14
d$sampleday[d$sampleday == 18] <- 17
d$sampleday[d$sampleday == 22] <- 21
d$sampleday[d$sampleday == 25] <- 24
d$sampleday[d$sampleday == 36] <- 35
d$sampleday[d$sampleday == 66] <- 65
d$sampleday[d$sampleday == 67] <- 65
d$sampleday[d$sampleday == 74] <- 73
d$sampleday[d$sampleday == 80] <- 79
d$sampleday[d$sampleday == 87] <- 86
d$sampleday[d$sampleday == 88] <- 86
d$sampleday[d$sampleday == 94] <- 93

##remove plants that died
datr<-dater[!(dater$RowNumNumRep=="13.3.R5" | 
              dater$RowNumNumRep=="17.3.R1" | 
              dater$RowNumNumRep=="34.7.R1" |
              dater$RowNumNumRep=="23.7.R2"), ]

## join dfs
dats <- join(datr, ids.sm, by=c("RowNum", "Num"))

write.csv(dats, file="output/clghdata.csv", row.names = FALSE)







