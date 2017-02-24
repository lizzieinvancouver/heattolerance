## Started 30 August 2016 ##
## By Lizzie ##

## Plots to look at the winegrape data ##
## Modified from https://github.com/lizzieinvancouver/buds/blob/c2956efd3d7a2f510adcd82e3e0a5c6502fd3add/analyses/Pheno%20Budburst%20analysis.R
## See lines 328 and onward #

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)
require(plyr); require(dplyr); require(tidyr) # data formatting

## set working directory
setwd("~/GitHub/heattolerance/analyses/")
# setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses")

## grab the data and merge them
dater <- read.csv("input/phenmoist_grapes2016.csv", header=TRUE)
ids <- read.csv("input/VitisExpReps2.csv", header=TRUE)

dat <- subset(dater, is.na(Date)==FALSE)

# make some changes for the merge
names(ids)[names(ids)=="Row"] <- "RowNum"
names(ids)[names(ids)=="Plant"] <- "Num"
names(ids)[names(ids)=="Variety"] <- "Var"
ids.sm <- subset(ids, select=c("RowNum", "Num", "Var"))

# merge the data
d <- join(dat, ids.sm, by=c("RowNum", "Num")) 

# format date (see http://www.statmethods.net/input/dates.html)
d$date <- as.Date(dat$Date, format="%m/%d/%Y")
d$days <- as.numeric(format(d$date, "%j"))-228 # 228 is around 15 August

d$days[d$days == "8"] <- "7"
d$days[d$days == "15"] <- "14"
d$days[d$days == "18"] <- "17"
d$days[d$days == "22"] <- "21"
d$days[d$days == "25"] <- "24"
d$days[d$days == "36"] <- "35"
d$days[d$days == "66"] <- "65"
d$days[d$days == "67"] <- "65"
d$days[d$days == "74"] <- "73"
d$days[d$days == "80"] <- "79"
d$days[d$days == "87"] <- "86"
d$days[d$days == "88"] <- "86"
d$days[d$days == "94"] <- "93"

# for now add the same treatment code to all, change to real treatments someday
d$treatcode <- rep("notreat", nrow(d))

# mean phen per plant
d$EL_mean <- rowMeans(d[,7:8], na.rm=TRUE) # careful! Relies on column numbers

##
##


plot(EL_stem1~date, data=d)

##
colz <- c("darkred","mediumturquoise") # need to adjust how to use lcol
lcol <- alpha(colz, 0.1)
cepages <- sort(unique(d$Var))

pdf(paste("graphs/VitisPheno", Sys.Date(), ".pdf", sep=""))

par(mfcol=c(3, 4), mar = c(3,3,1,0.5))
for(var in seq_along(cepages)){ # var <- 1
  
  dx <- subset(d, Var==cepages[var])
    unique(dx$days) # need to switch to numeric for code to run
    dx$days <- as.numeric(dx$days)
  
  counter = 1
  for(i in sort(as.character((unique(dx$treatcode))))){
    
    dseq = seq(0, max(dx$days))
    # Will need to up above 10 when stages get higher!
    plot(dseq, seq(0, 30, length=length(dseq)), type = "n", 
         ylab = "EL Stage",
         xlab = "day", main=cepages[var])
    # if(counter == 1) mtext(cepages, line = -2, adj = 0.5)
    legend("topleft", bty="n",i, cex = 0.85, inset = 0)
    xx <- dx[dx$treatcode == i,]
    # calculate mean response by date
    xt <- tapply(pmax(xx$EL_mean, na.rm=TRUE), list(xx$days), mean, na.rm=TRUE)
    
    for(j in unique(xx$Rep)){ 
      xj <- xx[xx$Rep == j,]
      lines(xj$days, xj$EL_mean, col = colz[2])
    }
    lines(rownames(xt), xt[], col = colz[1])
    
    counter = counter + 1
  }
  
}
dev.off()
