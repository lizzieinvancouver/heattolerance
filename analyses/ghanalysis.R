## Started 22 August 2017 ##
## By Lizzie and Nicole ##

## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# Get packages
library(ggplot2)
library(car)
library(colorspace)
library(RColorBrewer) # makes nice colors
library(plyr)
library(dplyr)

datep <- read.csv(file="output/clghestphen.csv", header = TRUE)
datnd <- read.csv(file="output/cldiam_node.csv", header = TRUE)
gendat <- read.csv(file="output/clghdata.csv", header = TRUE)
rmidat <- read.csv(file="output/bblo_2015syn.csv", header=TRUE) # see data/README_rmi.txt
rmibb <- read.csv(file="input/budburst_est_2015.csv", header=TRUE)

###################
## Some clean up ##
###################

datep$Var_corr <- datep$Var
datep$Var_corr[which(datep$Var_corr=="Durif1")] <- "Durif"
datep$Var_corr[which(datep$Var_corr=="Durif2")] <- "Durif"
datep$Var_corr[which(datep$Var_corr=="Valdepenas")] <- "Tempranillo"
datep$Var_corr[which(datep$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
datep$Var_corr[which(datep$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
datep$Var_corr[which(datep$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

datnd$Var_corr <- datnd$Var
datnd$Var_corr[which(datnd$Var_corr=="Durif1")] <- "Durif"
datnd$Var_corr[which(datnd$Var_corr=="Durif2")] <- "Durif"
datnd$Var_corr[which(datnd$Var_corr=="Valdepenas")] <- "Tempranillo"
datnd$Var_corr[which(datnd$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
datnd$Var_corr[which(datnd$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
datnd$Var_corr[which(datnd$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

unique(datnd$Var_corr)

gendat$Var_corr <- gendat$Var
gendat$Var_corr[which(gendat$Var_corr=="Durif1")] <- "Durif"
gendat$Var_corr[which(gendat$Var_corr=="Durif2")] <- "Durif"
gendat$Var_corr[which(gendat$Var_corr=="Valdepenas")] <- "Tempranillo"
gendat$Var_corr[which(gendat$Var_corr=="Alicante Bouchet")] <- "Alicante Bouschet"
gendat$Var_corr[which(gendat$Var_corr=="Pinot  Meunier")] <- "Pinot Meunier"
gendat$Var_corr[which(gendat$Var_corr=="Gewurtztraminer")] <- "Gewurztraminer"

unique(gendat$Var_corr) 

rmidat$variety[which(rmidat$variety=="Ungni blanc/Trebbiano")] <- "Ugni blanc/Trebbiano"
rmi <- rmidat[which(rmidat$variety %in% c(unique(gendat$Var_corr), "Valdepenas")),]
rmi$variety[which(rmi$variety=="Valdepenas")] <- "Tempranillo"

rmibb$variety[which(rmibb$variety=="Ungni blanc/Trebbiano")] <- "Ugni blanc/Trebbiano"
rmi.indbb <- rmibb[which(rmibb$variety %in% c(unique(gendat$Var_corr), "Valdepenas")),]
rmi.indbb$variety[which(rmi.indbb$variety=="Valdepenas")] <- "Tempranillo"

setdiff(unique(gendat$Var_corr),unique(rmi$variety))
setdiff(unique(datep$Var_corr),unique(rmi$variety))
setdiff(unique(datep$Var_corr),unique(rmi.indbb$variety))


####################
## Making a Table ##
####################

expttable1 <- 
  ddply(datnd, c("Var_corr"), summarise,
        n = length(Var_corr),
        numflow = sum(flowering12yn, na.rm = TRUE),
        nodemean = mean(nodesize_mean, na.rm = TRUE))

expttable2 <- 
  ddply(datep, c("Var_corr"), summarise,
        meanbb = mean(days.to.bb, na.rm = TRUE))

expttable <- join(expttable1, expttable2, by=c("Var_corr"))

##export as csv

write.csv(expttable, file = "output/expttable.csv", row.names = FALSE)

##############
## Plotting ##
##############

##50% flowering
hist(datep$days.to.50)
hist(log10(datep$days.to.50))
plot(datep$days.to.50)
ggplot(datep, aes(days.to.50, Var_corr, color=Var_corr)) + 
    geom_point()

ggplot(rmi, aes(doy.2015, variety, color=variety)) +
    geom_point()

# If you want ggplot to order by days ...
datep$var.ord <- factor(datep$Var_corr, levels = datep$Var_corr[order(datep$days.to.50)])
ggplot(datep, aes(days.to.50, var.ord, color=var.ord)) + 
    geom_point()
    
##10% flowering
datep$var.ord <- factor(datep$Var_corr, levels = datep$Var_corr[order(datep$days.to.10)])
ggplot(datep, aes(days.to.10, var.ord, color=var.ord)) + 
    geom_point()

rmi$var.ord <- factor(rmi$variety, levels = rmi$variety[order(rmi$doy.2015)])
ggplot(rmi, aes(doy.2015, var.ord, color=var.ord)) +
    geom_point()

varsum <-
  ddply(datep, c("Var_corr", "days.to.50"), summarise)


##budburst

ggplot(datep, aes(days.to.bb, Var_corr, color=Var_corr)) + 
  geom_point()

datep$var.ord <- factor(datep$Var_corr, levels = datep$Var_corr[order(datep$days.to.bb)])
ggplot(datep, aes(days.to.bb, var.ord, color=var.ord)) + 
  geom_point()

##leafout

ggplot(datep, aes(days.to.lo, Var_corr, color=Var_corr)) + 
  geom_point()

datep$var.ord <- factor(datep$Var_corr, levels = datep$Var_corr[order(datep$days.to.lo)])
ggplot(datep, aes(days.to.lo, var.ord, color=var.ord)) + 
  geom_point()
  
## correlations with diameter
#50% flowering
ggplot(datnd, aes(spurdiam_mean, flowering23yn, color=Var_corr)) + geom_point()

#inflor developed
ggplot(datnd, aes(spurdiam_mean, flowering12yn, color=Var_corr)) + geom_point()

#EL7
ggplot(datnd, aes(spurdiam_mean, flowering7yn, color=Var_corr)) + geom_point()

#EL4
ggplot(datnd, aes(spurdiam_mean, flowering4yn, color=Var_corr)) + geom_point()

##correlations with node size
#50 flowering
ggplot(datnd, aes(nodesize_mean, flowering23yn, color=Var_corr)) + geom_point()

#inflor developed
ggplot(datnd, aes(nodesize_mean, flowering12yn, color=Var_corr)) + geom_point()

#EL7
ggplot(datnd, aes(nodesize_mean, flowering7yn, color=Var_corr)) + geom_point()

#EL4
ggplot(datnd, aes(nodesize_mean, flowering4yn, color=Var_corr)) + geom_point()

#######
##GLM##
#######

##diameter
#50% flowering
mod.23diam <- glm(flowering23yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.23diam)

#inflor developed
mod.12diam <- glm(flowering12yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.12diam)

#EL7
mod.7diam <- glm(flowering7yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.7diam)

#EL4
mod.4diam <- glm(flowering4yn~spurdiam_mean, data=datnd, family = binomial(link="logit"))

summary(mod.4diam)


##node size
#50% flowering
mod.23node <- glm(flowering23yn~nodesize_mean, data=datnd, family = binomial(link="logit"))

summary(mod.23node)

#inflor developed
mod.12node <- glm(flowering12yn~nodesize_mean, data=datnd, family = binomial(link="logit"))

summary(mod.12node)

#EL7
mod.7node <- glm(flowering7yn~nodesize_mean, data=datnd, family = binomial(link="logit"))

summary(mod.7node)

#EL4
mod.4node <- glm(flowering4yn~nodesize_mean, data=datnd, family = binomial(link="logit"))

summary(mod.4node)

#######################################
## Plotting RMI and GH data together ##
######################################

# Step 1: Get the datep into mean and SE per var (did just for BB and LO, could add flo)
ghsum <-
      ddply(datep, c("Var_corr"), summarise,
      mean.bb = mean(days.to.bb),
      sd.bb = sd(days.to.bb),
      sem.bb = sd(days.to.bb)/sqrt(length(days.to.bb)),
      mean.lo = mean(days.to.lo),
      sd.lo = sd(days.to.lo),
      sem.lo = sd(days.to.lo)/sqrt(length(days.to.lo)))

rmimean <-
      ddply(rmi.indbb, c("variety"), summarise,
      mean.bb = mean(doy, na.rm=TRUE))

dat1 <- ghsum[order(ghsum$mean.bb),]
dat2 <- rmi
dat2 <- dat2[order(match(dat2$variety, dat1$Var_corr)),]
dat3 <- rmi.indbb
dat4 <- rmimean[order(match(rmimean$variety, dat1$Var_corr)),]

# While we're here make a list of vars with the order from each added ...
rmiorder <- dat4[order(dat4$mean.bb),]
rmiorder$rmiorder <- c(1:nrow(rmiorder))
rmiorder <- subset(rmiorder, select=c("variety", "rmiorder"))
ghorder <- merge(rmiorder, dat1, by.x="variety", by.y="Var_corr")
ghorder <- ghorder[order(ghorder$mean.bb),]
vartxt <- paste(ghorder$variety, " (", c(1:nrow(ghorder)), ", ", ghorder$rmiorder, ")", sep="")
    
##
## Step 2-4: First option: Plot on same plot
##

# Step 2: set up some plot parameters
# need to fix the outer margins, see: https://www.r-bloggers.com/mastering-r-plot-part-3-outer-margins/ and http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm


y <-c(1:nrow(dat1))
ytxt <- c(-37) # push the text way over left
wtpch <- c(16, 18, 1, 5)
yrangeusemod <- c(1, nrow(dat1))
prettycol <- colorRampPalette(brewer.pal(9,"YlOrRd")[2:9])(nrow(dat1))

# Step 3: open a blank plot
quartz("Quartz", width=4, height=8, pointsize=12)
par(mar=c(8.5, 9, 1.1, 2.1))
par(mfrow=c(1,1), cex=0.7, xpd=TRUE, yaxt="n")
plot(c(5,80), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
text(ytxt, c(1:nrow(dat1)), vartxt, adj=0, cex=1, outer=TRUE)
leg.txt<- c("greenhouse", "vineyard")
# legend(-8, (nrow(dat1)+2), leg.txt, pch=wtpch, bty="n")

# Step 4: Now plot each set of data
# plot the greenhouse data 
x<-as.vector(dat1$mean.bb)
xsem<-as.vector(dat1$sem.bb)
arrows(x-xsem,y,x+xsem,y,code=3,angle=90,length=0.0)
points(x,y,pch=wtpch[1], bg='white', col=alpha(prettycol, 0.75), cex=1.2)

# plot the RMI data 
y1 <- c(1:nrow(dat2))
x1 <-as.vector(dat2$doy.bb)
xsem1 <-as.vector(dat2$se.bb)
arrows(x1-xsem1,y1,x1+xsem1,y1,code=3,angle=90,length=0.0)
points(x1,y1,pch=wtpch[2], bg='white', col=alpha(prettycol, 0.75), cex=1.2)


##
## Step 2-4: Alternative version of above plot
## Note: I think this one may be better as it highlights the two different data sources 
##

y <-c(1:nrow(dat1))
ytxt <- c(-30) # push the text way over left
wtpch <- c(16, 18)
yrangeusemod <- c(1, nrow(dat1))
prettycol <- colorRampPalette(brewer.pal(9,"YlOrRd")[2:9])(nrow(dat1))

# Step 3: open a blank plot
quartz("Quartz", width=4, height=8, pointsize=12)
par(oma=c(8.5, 9, 1.1, 0))
par(mar=c(0, 2, 0, 1))
par(mfrow=c(1,3), cex=0.7, xpd=NA, yaxt="n")
plot(c(5,25), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
# mtext(ytxt, side=2, 0, outer=TRUE)
text(ytxt, c(1:nrow(dat1)), as.vector(dat1$Var_corr), adj=0, cex=1)
# leg.txt<- c("greenhouse", "vineyard")
# legend(-8, (nrow(dat1)+2), leg.txt, pch=wtpch, bty="n")

# Step 4: Now plot each set of data
# plot the greenhouse data 
x<-as.vector(dat1$mean.bb)
xsem<-as.vector(dat1$sem.bb)
arrows(x-xsem,y,x+xsem,y,code=3,angle=90,length=0.0)
points(x,y,pch=wtpch[1], bg='white', col=alpha(prettycol, 0.75), cex=1.2)

# plot the RMI data
par(mar=c(0, 1, 0, 2))
plot(c(40,80), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
y1 <- c(1:nrow(dat2))
x1 <-as.vector(dat2$doy.bb)
xsem1 <-as.vector(dat2$se.bb)
arrows(x1-xsem1,y1,x1+xsem1,y1,code=3,angle=90,length=0.0)
points(x1,y1,pch=wtpch[2], bg='white', col=alpha(prettycol, 0.75), cex=1.2)


## START alternative versions ##

## Alternative versions of Steps 3-4 to plot ALL datapoints with means on top

ytxt <- -115
# Step 3 (alterative): open a blank plot
quartz("Quartz", width=4, height=8, pointsize=12)
par(oma=c(8.5, 9, 1.1, 0))
par(mar=c(0, 2, 0, 1))
par(mfrow=c(1,3), cex=0.7, xpd=NA, yaxt="n")
plot(c(-5,30), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
text(ytxt, c(1:nrow(dat1)), vartxt, adj=0, cex=1)
# leg.txt<- c("greenhouse", "vineyard")
# legend(-8, (nrow(dat1)+2), leg.txt, pch=wtpch, bty="n")

# Step 4 (alterative): Now plot each set of data
# plot the greenhouse data

for (i in c(1:length(dat1$Var_corr))){
    subby <- subset(datep, Var_corr==dat1$Var_corr[i])
    x<-as.vector(subby$days.to.bb)
    points(x,rep(y[i], length(x)),pch=wtpch[1], bg='white',
        col=alpha(prettycol[i], 0.75), cex=1.2)
}
x<-as.vector(dat1$mean.bb)
points(x,y, pch=wtpch[3], bg='white', col="black", cex=1.2)

# plot the RMI data (alterative)
par(mar=c(0, 1, 0, 2))
plot(c(40,90), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
y1 <- c(1:nrow(dat2))

for (i in c(1:length(dat1$Var_corr))){
    subby <- subset(dat3, variety==dat1$Var_corr[i])
    x<-as.vector(subby$doy)
    points(x,rep(y[i], length(x)),pch=wtpch[2], bg='white',
        col=alpha(prettycol[i], 0.75), cex=1.2)
}
x1<-as.vector(dat4$mean.bb)
points(x1,y1, pch=wtpch[4], bg='white', col="black", cex=1)

## END alternative versions ##
