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

source("source/estimatephen.R")

# get data
dater <- read.csv("input/phenmoist_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)
nsdater <- read.csv("output/clnodespursize.csv", header=TRUE)

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
dater$sampleday <- dater$days
dater$sampleday[dater$sampleday == 8] <- 7
dater$sampleday[dater$sampleday == 15] <- 14
dater$sampleday[dater$sampleday == 18] <- 17
dater$sampleday[dater$sampleday == 22] <- 21
dater$sampleday[dater$sampleday == 25] <- 24
dater$sampleday[dater$sampleday == 36] <- 35
dater$sampleday[dater$sampleday == 66] <- 65
dater$sampleday[dater$sampleday == 67] <- 65
dater$sampleday[dater$sampleday == 74] <- 73
dater$sampleday[dater$sampleday == 80] <- 79
dater$sampleday[dater$sampleday == 87] <- 86
dater$sampleday[dater$sampleday == 88] <- 86
dater$sampleday[dater$sampleday == 94] <- 93

##remove plants that died
datr <- dater[!(dater$RowNumNumRep=="13.3.R5" | 
              dater$RowNumNumRep=="17.3.R1" | 
              dater$RowNumNumRep=="34.7.R1" |
              dater$RowNumNumRep=="23.7.R2"), ]

##get averages
datr$EL_mean <- rowMeans(datr[,6:7], na.rm=TRUE)
datr$sm_mean <- rowMeans(datr[,8:10], na.rm=TRUE)
nsdater$nodesize_mean <- rowMeans(nsdater[,6:9], na.rm=TRUE)
nsdater$spurdiam_mean <- rowMeans(nsdater[,10:11], na.rm=TRUE)

##subset nsdater
nsdat <- subset(nsdater, select=c("RowNumNumRep", "nodesize_mean", "spurdiam_mean"))

## join dfs
datss <- join(datr, ids.sm, by=c("RowNum", "Num"))

##remove unkown varieties
dats <- datss[!(is.na(datss$Var)), ]


##
## START CODE by Lizzie to figure out which plants made it to which stage ...
## Hypothetical example: figure out which plants made it to EL 14 ....

## summarizing data 
maxstage <-
      ddply(dats, c("RowNumNumRep"), summarise,
      minEL = min(EL_mean, na.rm=TRUE),
      maxEL = max(EL_mean, na.rm=TRUE))

# Now subset to correct level and reduce the original dataframe ..
stage15ind <- subset(maxstage, maxEL>13.999)
datsEL14 <- dats[which(dats$RowNumNumRep %in% stage15ind$RowNumNumRep),]

# quick check
sort(unique(datsEL14$RowNumNumRep))
sort(unique(stage15ind$RowNumNumRep))

##
## END CODE by Lizzie to figure out which plants made it to which stage
##

##which plants flowered (50% flowering = EL 23)

submaxst23 <- subset(maxstage, maxEL>22.999)
datsEL23 <- dats[which(dats$RowNumNumRep %in% submaxst23$RowNumNumRep),]

sort(unique(datsEL23$RowNumNumRep))
sort(unique(submaxst$RowNumNumRep))

## 10% flowering
submaxst20 <- subset(maxstage, maxEL>19.999)
datsEL20 <- dats[which(dats$RowNumNumRep %in% submaxst20$RowNumNumRep),]

## develop inflor
submaxst12 <- subset(maxstage, maxEL>11.999)
datsEL12 <- dats[which(dats$RowNumNumRep %in% submaxst12$RowNumNumRep),]

##EL 7
submaxst7 <- subset(maxstage, maxEL>6.999)
datsEL7 <- dats[which(dats$RowNumNumRep %in% submaxst7$RowNumNumRep),]

##EL 4
submaxst4 <- subset(maxstage, maxEL>3.999)
datsEL4 <- dats[which(dats$RowNumNumRep %in% submaxst4$RowNumNumRep),]
# checking by Lizzie
setdiff(unique(maxstage$RowNumNumRep), unique(datsEL4$RowNumNumRep))

##estimatephen 4 and 7
bbdf <- get_pheno_est(dats,"budbreak",4,NA) 
ubb <- unique(bbdf, na.rm=TRUE)
subdats <- subset(dats, select=c("RowNumNumRep", "Var"))
sdt <- unique(subdats)
subb <- join(ubb, sdt, by=c("RowNumNumRep"))
names(subb)[names(subb)=="days"] <- "days.to.bb"
lodf <- get_pheno_est(dats,"leafout",7,NA) 
ulo <- unique(lodf, na.rm = TRUE)
sulo <- join(ulo, sdt, by=c("RowNumNumRep"))
names(sulo)[names(sulo)=="days"] <- "days.to.lo"
dat <- join(subb, sulo, by=c("RowNumNumRep"))

##estimatephen 50%
ffdf <- get_pheno_est(datsEL23,"50% flowering",23,NA) 
uff <- unique(ffdf, na.rm = TRUE)
suff <- join(uff, sdt, by=c("RowNumNumRep"))
names(suff)[names(suff)=="days"] <- "days.to.50"

##estimatephen 10%
tfdf <- get_pheno_est(datsEL20,"10% flowering",20,NA) 
utf <- unique(tfdf, na.rm = TRUE)
sutf <- join(utf, sdt, by=c("RowNumNumRep"))
names(sutf)[names(sutf)=="days"] <- "days.to.10"

##join estimatephens
dt <- join(sutf,suff, by=c("RowNumNumRep"))

ssdt <- subset(dt, select=c("RowNumNumRep", "Var", "days.to.50", "days.to.10"))

dts <- subset(dat, select=c("RowNumNumRep", "Var", "days.to.bb", "days.to.lo"))

epdt <- join(ssdt,dts, by=c("RowNumNumRep"))

##flowering yes/no column
##50% flowering
sdt$flowering23yn <- NA
sdt$flowering23yn[which(sdt$RowNumNumRep %in% submaxst23$RowNumNumRep)] <- 1
sdt$flowering23yn[which(!sdt$RowNumNumRep %in% submaxst23$RowNumNumRep)] <- 0

##inflorescence
sdt$flowering12yn <- NA
sdt$flowering12yn[which(sdt$RowNumNumRep %in% submaxst12$RowNumNumRep)] <- 1
sdt$flowering12yn[which(!sdt$RowNumNumRep %in% submaxst12$RowNumNumRep)] <- 0

##7
sdt$flowering7yn <- NA
sdt$flowering7yn[which(sdt$RowNumNumRep %in% submaxst7$RowNumNumRep)] <- 1
sdt$flowering7yn[which(!sdt$RowNumNumRep %in% submaxst7$RowNumNumRep)] <- 0

##4
sdt$flowering4yn <- NA
sdt$flowering4yn[which(sdt$RowNumNumRep %in% submaxst4$RowNumNumRep)] <- 1
sdt$flowering4yn[which(!sdt$RowNumNumRep %in% submaxst4$RowNumNumRep)] <- 0

##subset
yndt <- subset(sdt, select=c("RowNumNumRep", "Var", "flowering23yn", "flowering12yn", "flowering7yn", "flowering4yn"))

##join wiht nsdat

nddt <- join(yndt, nsdat, by=c("RowNumNumRep"))

write.csv(epdt, file="output/clghestphen.csv", row.names = FALSE)
write.csv(dats, file="output/clghdata.csv", row.names = FALSE)
write.csv(nddt, file="output/cldiam_node.csv", row.names = FALSE)







