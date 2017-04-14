## Started 27 January 2017 ##
## By Nicole ##

##EL stage##

## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
setwd("/Users/Nicole/Desktop/Wolkovich/analysis/")

# get one f(x)
source("source/estimatephen.R") # more notes in this file, check it out!

## get data
nodespurdater <- read.csv("input/baselinespursize.csv", header=TRUE)
dater <- read.csv("input/phenmoist_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

## change header names here
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

## join dfs
dats <- join(dater, ids.sm, by=c("RowNum", "Num"))
nsdats <- join(nodespurdater, ids.sm, by=c("RowNum", "Num")) 

## format date (see http://www.statmethods.net/input/dates.html)
dats$Date <- as.Date(dater$Date, format="%m/%d/%Y")
dats$days <- as.numeric(format(dats$Date, "%j"))-228 # 245 is around the start of September

##get averages
dats$EL_mean <- rowMeans(dats[,6:7], na.rm=TRUE)
dats$sm_mean <- rowMeans(dats[,8:10], na.rm=TRUE)

## fix this 
nsdats$diam_mean <- rowMeans(nsdats[,10:11], na.rm=TRUE)
nsdats$ndsz_mean <- rowMeans(nsdats[6:9], na.rm=TRUE)

## histogram soil moisture
ggplot(dats, aes(sm_mean), na.rm=TRUE) + geom_histogram(breaks=seq(0, 35, by = 1))

## categorize by soil moisture
dats <- within(dats, {
  sm_cat <- NA
  sm_cat[sm_mean<=11.2]           <- "driest"
  sm_cat[sm_mean>11.2 & sm_mean<=13.1] <- "dry"
  sm_cat[sm_mean>13.1 & sm_mean<=15.1]           <- "moist"
  sm_cat[sm_mean>15.1] <- "very moist"
})

##histogram diam_mean
ggplot(nsdats, aes(diam_mean), na.rm=TRUE) + geom_histogram(breaks=seq(0, 12, by = 1))

##categorize by spur diameter
nsdats <- within(nsdats, {
  diam_cat <- NA
  diam_cat[diam_mean<3] <- "1.00-3.00cm"
  diam_cat[diam_mean>3 & diam_mean<=5]           <- "3.00-5.00cm"
  diam_cat[diam_mean>5 & diam_mean<=7] <- "5.00-7.00cm"
  diam_cat[diam_mean>7 & diam_mean<=9]           <- "7.00-9.00cm"
  diam_cat[diam_mean>9] <- "9.00-11.00cm"
})

##histogram node size
ggplot(nsdats, aes(ndsz_mean), na.rm=TRUE) + geom_histogram(breaks=seq(0, 8, by = 1))

##categorize node size
nsdats <- within(nsdats, {
  ndsz_cat <- NA
  ndsz_cat[ndsz_mean<3] <- "1.00-3.00cm"
  ndsz_cat[ndsz_mean>3 & ndsz_mean<=4]           <- "3.00-4.00cm"
  ndsz_cat[ndsz_mean>4 & ndsz_mean<=5] <- "4.00-5.00cm"
  ndsz_cat[ndsz_mean>5] <- "5.00-6.00cm"
})

##subset nsdats
ns <- subset(nsdats, select=c("RowNumNumRep", "diam_cat", "ndsz_cat"))

## join ns and dats
ds <- join(dats, ns, by=c("RowNumNumRep"))
ds.sm <- subset(ds, select=c(
  "Date", 
  "RowNumNumRep", 
  "Var", 
  "days", 
  "EL_mean", 
  "sm_cat", 
  "diam_cat", 
  "ndsz_cat"))
ds.om <- na.omit(ds.sm)

ggplot(ds.om, aes(days, EL_mean, color=Var, group=RowNumNumRep)) + geom_point(aes(size=sm_cat), shape=1) + geom_line() + labs(x = "Time (days)", y = "EL Stage")

ggplot(ds.om, aes(days, EL_mean, color=Var, group=RowNumNumRep)) + geom_point(shape=1) + geom_line(aes(linetype=ndsz_cat)) + labs(x = "Time (days)", y = "EL Stage")

ggplot(ds.om, aes(days, EL_mean, color=Var, group=RowNumNumRep)) + geom_point(shape=1) + geom_line(aes(linetype=diam_cat)) + labs(x = "Time (days)", y = "EL Stage")

# alternative version of one of the above plots
ggplot(ds.om, aes(days, EL_mean, color=sm_cat)) +
    geom_point() +
    facet_wrap(~Var) +
    geom_line() + labs(x = "Time (days)", y = "EL Stage")


#################################
## Estimating dates of events! ##
#################################


##
## Working on getting estimates of day each ind reached a certain stage
bbday <- get_pheno_est(ds.om, "budburst", 5, NA) # should double-check what we call budburst with EL paper

# hmm, some weird Tempranillo data; how can EL go from 10 to 0?
weird <- dat[which(dat$RowNumNumRep=="35.3.R5"),]

# exclude for now
bbday <- subset(bbday, RowNumNumRep != "35.3.R5")

## get mean bbday by var (this is not the most efficient way!)
# and SD and SE (std deviation and error, respectively)
bb.mean <-
      ddply(bbday, c("Var"), summarise,
      mean.bb = mean(days),
      sd = sd(days),
      sem = sd(days)/sqrt(length(days)))

# now merge this back in
bbday.full <- merge(bb.mean, bbday, by="Var", all.y=TRUE)


# We should figure out how to order this plot by the bbday of each var ... figure out how to suppress the legend
pdf(file="graphs/bbday_byvar.pdf")
ggplot(bbday, aes(days, Var, color=Var)) +
    geom_point()
dev.off()

# Colors not the best ... need to change to rainbow or heat or such, I think
ggplot(bbday.full, aes(days, Var, color=mean.bb)) +
    geom_point()
