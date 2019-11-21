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

source("source/estimatephen.R")

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

## fix incorrect leaf numbers
chambdater[229, ][which(chambdater[229, ]=="7")] <- 10
chambdater[146, ][which(chambdater[146, ]=="11")] <- 10

##mean temp for each chamber
chambdater$temp <- NA
chambdater$temp[which(chambdater$Treat=="Chamber 1")] <- 20
chambdater$temp[which(chambdater$Treat=="Chamber 2")] <- 26
chambdater$temp[which(chambdater$Treat=="Chamber 3")] <- 30
chambdater$temp[which(chambdater$Treat=="Chamber 4")] <- 34
chambdater$temp[which(chambdater$Treat=="Chamber 5")] <- 37

## remove sad stem 1s
sadstemlist <- c("13.3.R4", "16.1.R7", "18.5.R5", "19.9.R3", "20.2.R4", "20.5.R3")
chambdater$stem1_percflow[which(chambdater$RowNumNumRep %in% sadstemlist)] <- NA
chambdater$stem1_length[which(chambdater$RowNumNumRep %in% sadstemlist)] <- NA
chambdater$stem1_leafnum[which(chambdater$RowNumNumRep %in% sadstemlist)] <- NA

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

##get means
chambdats$length_mean <- rowMeans(chambdats[,7:8], na.rm=TRUE)
chambdats$lfnum_mean <- rowMeans(chambdats[,9:10], na.rm=TRUE)
chambdats$sm_mean <- rowMeans(chambdats[15:17], na.rm=TRUE)
chambdats$pf_mean <- rowMeans(chambdats[,11:12], na.rm=TRUE)
chambdats$bfall_mean <- rowMeans(chambdats[,18:19], na.rm=TRUE)
chambdats$vFcount_mean <- rowMeans(chambdats[,20:21], na.rm=TRUE)
chambdats$vFest_mean <- rowMeans(chambdats[,22:23], na.rm=TRUE)
chambdats$capfall_mean <- rowMeans(chambdats[,13:14], na.rm=TRUE)

## categorize by soil moisture
chambdats <- within(chambdats, {
  sm_cat <- NA
  sm_cat[sm_mean<=13.8]           <- "driest"
  sm_cat[sm_mean>13.8 & sm_mean<=15.5] <- "dry"
  sm_cat[sm_mean>15.5 & sm_mean<=17.2]           <- "moist"
  sm_cat[sm_mean>17.2] <- "very moist"
})
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


##
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
chambdats$stem1pflowr <- NA
chambdats$stem2pflowr <- NA



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
   chambdats$stem1pflowr[which(chambdats$RowNumNumRep==indhere)] <- subby$stem1_percflow -
     subby$stem1_percflow[which(subby$days==min(subby$days))]
   chambdats$stem2pflowr[which(chambdats$RowNumNumRep==indhere)] <- subby$stem2_percflow -
     subby$stem2_percflow[which(subby$days==min(subby$days))]
}



## taking some means
chambdats$stemlenchange <- rowMeans(chambdats[c("stem1change", "stem2change")], na.rm=TRUE)
chambdats$lfchange <- rowMeans(chambdats[c("stem1lfchange", "stem2lfchange")], na.rm=TRUE)
chambdats$pflowr_mean <- rowMeans(chambdats[c("stem1pflowr", "stem2pflowr")], na.rm=TRUE)
chambdats$vFper_mean <- rowMeans(chambdats[c("stem1_vFper", "stem2_vFper")], na.rm=TRUE)


## renaming pflowr_mean so it works with estiamtephen
colnames(chambdats)[which(names(chambdats) == "pflowr_mean")] <- "EL_mean"

##subset for etimatephen 10%
ep10list <- c("16.1.R1", "16.1.R4", "24.9.R7")
ep10dats <- chambdats [which(!chambdats$RowNumNumRep %in% ep10list),]

##subset for estimatephen 50%
ep50list <- c("16.1.R1", "16.1.R4", "24.9.R7", "16.1.R3", "18.5.R7", "18.5.R8", "20.5.R3", "38.7.R2")
ep50dats <- chambdats [which(!chambdats$RowNumNumRep %in% ep50list),]

##estimatephen flowering
chdat <- get_pheno_est(ep50dats,"50% flowering",50,NA)
chdat10 <- get_pheno_est(ep10dats,"10% flowering",10,NA)


##remove duplicates
cdf <- unique(chdat)
cdf10 <- unique(chdat10)

##subset for Treat
cdftreat <- subset(chambdats, select=c("RowNumNumRep", "Treat", "temp"))
cdft <- unique(cdftreat)
cd <- join(cdf,cdft, by=c("RowNumNumRep"))
cd10 <- join(cdf10,cdft, by=c("RowNumNumRep"))

chdatsum <-
  ddply(chambdats, c("RowNumNumRep", "Treat", "Var", "temp"), summarise,
        max.pf_mean = max(pf_mean, na.rm=TRUE),
        sum.bfall_mean = sum(bfall_mean, na.rm=TRUE),
        sum.capfall_mean = sum(capfall_mean, na.rm=TRUE),
        mean.smoist = mean(sm_mean, na.rm=TRUE),
        max.lfchange = max(lfchange, ra.rm=TRUE),
        max.lengthchange = max(stemlenchange, ra.rm=TRUE))
        



write.csv(chambdats, file="output/clchambdata.csv", row.names = FALSE)
write.csv(cd, file="output/chamb50fl.csv", row.names = FALSE)
write.csv(cd10, file="output/chamb10fl.csv", row.names = FALSE)
write.csv(chdatsum, file="output/chdatsum.csv", row.names = FALSE)

