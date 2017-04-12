## Started March 29 2017 ##
## By Nicole ##

##cleaning for chambergraphs

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

## get data
chambdater <- read.csv("input/chamberobservations_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

## change header names here
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
names(chambdater)[names(chambdater)=="Treatment"] <- "Treat"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

## fix non-numerics
chambdater$stem1_percflow[which(chambdater$stem1_percflow=="-")] <- 0
chambdater$stem1_percflow[which(chambdater$stem1_percflow=="")] <- 0
chambdater$stem1_percflow <- as.numeric(chambdater$stem1_percflow)
chambdater$stem1_bagbuds[which(chambdater$stem1_bagbuds=="")] <- 0
chambdater$stem1_bagbuds[which(chambdater$stem1_bagbuds=="-")] <- 0
chambdater$stem2_bagbuds[which(chambdater$stem2_bagbuds=="")] <- 0
chambdater$stem2_bagbuds[which(chambdater$stem2_bagbuds=="-")] <- 0
chambdater$stem1_vFcount[which(chambdater$stem1_vFcount=="")] <- 0
chambdater$stem1_vFcount[which(chambdater$stem1_vFcount=="c")] <- 0
chambdater$stem1_vFcount <- as.numeric(chambdater$stem1_vFcount)

# delete a couple random rows of data after checking what's in them (straight from og flowgraphs)
unique(chambdater$X)
unique(chambdater$X.1)
chambdater$X <- NULL
chambdater$X.1 <- NULL

## date to days 
chambdater$Date <- as.Date(chambdater$Date, format="%m/%d/%Y")
chambdater$days <- as.numeric(format(chambdater$Date, "%j"))-228 # 245 is around the start of September

## join dfs
chambdats <- join(chambdater, ids.sm, by=c("RowNum", "Num"))

## categorize by soil moisture
chambdats <- within(chambdats, {
  sm_cat <- NA
  sm_cat[sm_mean<=13.8]           <- "driest"
  sm_cat[sm_mean>13.8 & sm_mean<=15.5] <- "dry"
  sm_cat[sm_mean>15.5 & sm_mean<=17.2]           <- "moist"
  sm_cat[sm_mean>17.2] <- "very moist"

## how well do values across stems compare?
plot(stem1_percflow~stem2_percflow, chambdats)
plot(stem1_caps~stem2_caps, chambdats)
plot(stem1_bagbuds~stem2_bagbuds, chambdats)
plot(stem1_vFcount~stem2_vFcount, chambdats)
plot(stem1_vFexpec~stem2_vFexpec, chambdats)

# check moisture
plot(moist_1~moist_2, chambdats) # Nicole: outlier originally noted here?
plot(moist_2~moist_3, chambdats)

# average up moisture, percent flowering ...
# needs to be finished! -Nicole:not sure what else needs to be done here
head(chambdats[,15:17]) # make sure we're selecting the right columns
chambdats$mean.moist <- rowMeans(chambdats[,15:17], na.rm=TRUE)
head(chambdats[,11:12])
chambdats$perflow <- rowMeans(chambdats[,11:12], na.rm=TRUE)
# total up counts
head(chambdats[,18:19])
chambdats$bagbuds <- rowSums(chambdats[,11:12], na.rm=TRUE) # need to deal with numeric
head(chambdats[,20:21])
head(chambdats[,22:23])

##
# get % total flowering from vFcount
# first, find out max flower count per individual plant
#check for non-numerics
sapply(chambdats, is.numeric)

chambdatsummary <-
  ddply(chambdats, c("RowNum", "Num", "Rep", "RowNumNumRep", "Treat"), summarise,
        max.stem1_vFcount= max(stem1_vFcount, na.rm=TRUE),
        max.stem2_vFcount= max(stem2_vFcount, na.rm=TRUE))

# next, step through data by each indiviual and divide by this max
unique.ind <- unique(chambdats$RowNumNumRep)

chambdats$stem1_vFper <- NA
chambdats$stem2_vFper <- NA


for(i in seq_along(unique.ind)){ # i = 1
  indhere <- unique.ind[i]
  # indhere <- "16.1.R3" # example of one with a couple values ... 
  max1.hereis <- chambdatsummary$max.stem1_vFcount[which(chambdatsummary$RowNumNumRep==indhere)]
  
  chambdats$stem1_vFper[which(chambdats$RowNumNumRep==indhere)] <-
    chambdats$stem1_vFcount[which(chambdats$RowNumNumRep==indhere)]/max1.hereis
  
  max2.hereis <- chambdatsummary$max.stem2_vFcount[which(chambdatsummary$RowNumNumRep==indhere)]
  
  chambdats$stem2_vFper[which(chambdats$RowNumNumRep==indhere)] <-
    chambdats$stem2_vFcount[which(chambdats$RowNumNumRep==indhere)]/max2.hereis
}


##
# 
#attempting to use code for leafnum and stem length
#check for non-numerics
sapply(chambdats, is.numeric)


# next, step through data by each indiviual and divide by this max
unique.ind <- unique(chambdats$RowNumNumRep)

chambdats$daysinchamb <- NA
chambdats$stem1lfchange <- NA
chambdats$stem2lfchange <- NA
chambdats$stem1change <- NA
chambdats$stem2change <- NA
chambdats$stem1vFestcount <- NA
chambdats$stem2vFestcount <- NA


for(i in seq_along(unique.ind)){ # i = 1
   # indhere <- "16.1.R3" # example of one with a couple values ...
   indhere <- unique.ind[i]
   subby <- chambdats[which(chambdats$RowNumNumRep==indhere),]
   minhere <- subby$days[which(subby$days==min(subby$days))]
   chambdats$daysinchamb[which(chambdats$RowNumNumRep==indhere)] <- subby$days-minhere
   chambdats$stem1lfchange[which(chambdats$RowNumNumRep==indhere)] <- subby$stem1_leafnum -
       subby$stem1_leafnum[which(subby$days==min(subby$days))]
   chambdats$stem2lfchange[which(chambdats$RowNumNumRep==indhere)] <- subby$stem2_leafnum -
     subby$stem2_leafnum[which(subby$days==min(subby$days))]
   chambdats$stem1change[which(chambdats$RowNumNumRep==indhere)] <- subby$stem1_length -
       subby$stem1_length[which(subby$days==min(subby$days))]
   chambdats$stem2change[which(chambdats$RowNumNumRep==indhere)] <- subby$stem2_length -
      subby$stem2_length[which(subby$days==min(subby$days))]
   chambdats$stem1vFestcount[which(chambdats$RowNumNumRep==indhere)] <- subby$stem1_vFexpec -
     subby$stem1_vFexpec[which(subby$days==min(subby$days))]
   chambdats$stem2vFestcount[which(chambdats$RowNumNumRep==indhere)] <- subby$stem2_vFexpec -
     subby$stem2_vFexpec[which(subby$days==min(subby$days))]
}


chambdats$stemlenchange <- rowMeans(chambdats[c("stem1change", "stem2change")], na.rm=TRUE)

write.csv(chambdats, file="output/clchambdata.csv", row.names = FALSE)
