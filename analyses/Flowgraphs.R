## Started 14 February 2017 ##
## By Lizzie ##

## Looking at flower counts ##
## Quick work to get started, much to do ##

## Lots to do: ##
# Finish taking means or sums of data where appropriate, then summarize (code started below) #
# Convert VitisFlower app estimate to % flowering (discuss with Lizzie)
# Perhaps re-code the ones that drop flowers as 0 on the date they drop flowers? #
# ... and much more! Plot results etc!
# Also do some plots of stem length and N leaves over time by Var/rep (for Inaki) #


## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
setwd("/Users/Nicole/GitHub/heattolerance/analysis/")

## get data
dater <- read.csv("input/chamberobservations_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

# delete a couple random rows of data after checking what's in them
unique(dater$X)
unique(dater$X.1)
dater$X <- NULL
dater$X.1 <- NULL

## change header names here
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

## join dfs
dat <- join(dater, ids.sm, by=c("RowNum", "Num"))

## how well do values across stems compare?
plot(stem1_percflow~stem2_percflow, dat)
plot(stem1_caps~stem2_caps, dat)
plot(stem1_bagbuds~stem2_bagbuds, dat)
plot(stem1_vFcount~stem2_vFcount, dat)
plot(stem1_vFexpec~stem2_vFexpec, dat)

# check moisture
plot(moist_1~moist_2, dat) # eek! outlier here, check value and fix?
plot(moist_2~moist_3, dat)


# average up moisture, percent flowering ...
# needs to be finished!
head(dat[,15:17]) # make sure we're selecting the right columns
dat$mean.moist <- rowMeans(dat[,15:17], na.rm=TRUE)
head(dat[,11:12])
dat$perflow <- rowMeans(dat[,11:12], na.rm=TRUE)
# total up counts
head(dat[,18:19])
dat$bagbuds <- rowSums(dat[,11:12], na.rm=TRUE) # need to deal with numeric
head(dat[,20:21])
head(dat[,22:23])

##
# get % total flowering from vFcount
# first, find out max flower count per individual plant
dat$stem1_vFcount <- as.numeric(dat$stem1_vFcount)
dat$stem2_vFcount <- as.numeric(dat$stem2_vFcount)

datsummary <-
      ddply(dat, c("RowNum", "Num", "Rep", "RowNumNumRep", "Treatment"), summarise,
      max.stem1_vFcount= max(stem1_vFcount, na.rm=TRUE),
      max.stem2_vFcount= max(stem2_vFcount, na.rm=TRUE))

# next, step through data by each indiviual and divide by this max
unique.ind <- unique(dat$RowNumNumRep)

dat$stem1_vFper <- NA
dat$stem2_vFper <- NA

for(i in seq_along(unique.ind)){ # i = 1
    indhere <- unique.ind[i]
    # indhere <- "16.1.R3" # example of one with a couple values ... 
    max1.hereis <- datsummary$max.stem1_vFcount[which(datsummary$RowNumNumRep==indhere)]
    
    dat$stem1_vFper[which(dat$RowNumNumRep==indhere)] <-
        dat$stem1_vFcount[which(dat$RowNumNumRep==indhere)]/max1.hereis
    
    max2.hereis <- datsummary$max.stem2_vFcount[which(datsummary$RowNumNumRep==indhere)]
    
    dat$stem2_vFper[which(dat$RowNumNumRep==indhere)] <-
        dat$stem2_vFcount[which(dat$RowNumNumRep==indhere)]/max2.hereis
    }
## end get total for vFcount ... but general method above would work for vFexpect too
## or for anything where we want to compare to some one value across individuals

## and values across methods?
# should add plots here that compare VitisFlower estimates to our counts ...

## thinking about getting change in stem length (or n leaves) ... need those counts on the min(days)
# So, skip datsummary and do all the work inside the loop!
# Inside the loop (column names not quite correct!):
# Step 1: Subset down to the data for that individual
# subby <- dat[which(dat$RowNumNumRep==indhere),] 
# Step 2: Get the value you want
# leaves_on_chamberday1 <- subby$stem1_leafnum[which(subby$days==min(subby$days))]
# Step 3: then adjust column names and follow as above (e.g., adjust dat$stem1_vFper[which(dat$RowNumNumRep==indhere)] <-
       # dat$stem1_vFcount[which(dat$RowNumNumRep==indhere)]/max1.hereis
## end of thinking ... 

# once we finish the above, we can summarize the data if we want
datsummary <-
      ddply(dat, c("RowNum", "Num", "Rep", "RowNumNumRep", "Treatment", "Var"), summarise,
      mean.moist = mean(mean.moist),
      sd = sd(mean.moist),
      sem = sd(mean.moist)/sqrt(length(mean.moist))) # you can add as much as you want to this!



# how many vars?
unique(dat$Var) # 9
unique(dat$RowNumNumRep) # 26
# how many unique plants per variety? Fires we have to aggregate to one level ...
howmanyinds.agg <- aggregate(dat["moist_1"], dat[c("RowNumNumRep", "Var")],
   FUN=length) # note that the moist_1 variable is not what we're interested in
howmanyinds <- aggregate(howmanyinds.agg[c("RowNumNumRep")], 
   howmanyinds.agg["Var"], FUN=length)
whichtreat.agg <- aggregate(dat["moist_1"], dat[c("RowNumNumRep", "Treatment", "Var")],
   FUN=length) # note that the moist_1 variable is not what we're interested in
whichtreat <- aggregate(whichtreat.agg[c("RowNumNumRep")], 
   whichtreat.agg[c("Treatment", "Var")], FUN=length)
