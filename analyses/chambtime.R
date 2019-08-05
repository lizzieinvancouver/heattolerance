## Started July 30 2019 ##
## By Nicole ##

## Started March 29 2017 ##
## By Nicole ##

##cleaning for chambergraphs

## general housekeeping ##
rm(list=ls())
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# get data
chambtime <- read.csv("input/plants_in_treatments.csv", header=TRUE)

# change header names
names(chambtime)[names(chambtime)=="Date.into.chamber"] <- "Indate"
names(chambtime)[names(chambtime)=="Date.out.of.chamber"] <- "Outdate"
names(chambtime)[names(chambtime)=="Treatment"] <- "Treat"

# remove unnecessry columns
chambtime.sm <- subset(chambtime, select=c("Indate", "RowNumNumRep", "Treat", "Outdate"))

# date to days 
chambtime.sm$Indate <- as.Date(chambtime.sm$Indate, format="%m/%d/%Y")
chambtime.sm$Inday <- as.numeric(format(chambtime.sm$Indate, "%j"))-228 # 245 is around the start of September
chambtime.sm$Outdate <- as.Date(chambtime.sm$Outdate, format="%m/%d/%Y")
chambtime.sm$Outday <- as.numeric(format(chambtime.sm$Outdate, "%j"))-228 # 245 is around the start of September

# time in chamber
chambtime.sm$Tottime <- (chambtime.sm$Outday-chambtime.sm$Inday)

# plot

ttmeans <-
  ddply(chambtime.sm, c("Treat"), summarise,
        mean = mean(Tottime),
        max = max(Tottime),
        sd = sd(Tottime),
        n = length(Tottime),
        sem = sd(Tottime)/sqrt(length(Tottime)))

plot(mean~Treat, data=ttmeans, ylim=c(30, 70), type="n", xaxt="n",
     xlab=expression(paste("Chamber),
     ylab="Time in Chamber")
axis(1, at=ttmeans$Treat, labels=ttmeans$Treat)

for (treatnum in c(1:length(unique(dat10$temp)))){
  subtreat <- subset(dat10, temp==sort(unique(dat10$temp))[treatnum])
  points(days~temp, data=subtreat, col=treatcol[treatnum], lwd=1.5)
}
legend("topleft", legend=daynightemp, pch=16, 
       col=treatcol,  bty="n")

points(mean~temp, data=tfmeans, ylim=c(30, 70), lwd=2)
arrows(tfmeans$temp, tfmeans$mean-tfmeans$sem,  tfmeans$temp,
       tfmeans$mean+tfmeans$sem, length = 0, lwd=2)
text(x = tfmeans$temp, y = tfmeans$max+5,
     label = tfmeans$n, pos = 3, cex = 0.8, col = "black")