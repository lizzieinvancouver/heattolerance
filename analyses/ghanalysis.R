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

setdiff(unique(gendat$Var_corr),unique(rmi$variety))

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

dat1 <- rmi[order(rmi$doy.bb),]
dat2 <- datep
dat2 <- dat2[order(match(dat2$Var, dat1$variety)),]

# set up some plot parameters
# need to fix the outer margins, see: https://www.r-bloggers.com/mastering-r-plot-part-3-outer-margins/ and http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm
y <-c(1:nrow(dat1))
ytxt <- c(-30) # need to move further negative once we fix margins
wtpch <- c(16, 18)
yrangeusemod <- c(1,nrow(dat1))
prettycol <- colorRampPalette(brewer.pal(9,"YlOrRd")[2:9])(nrow(dat1))

# open a blank plot
quartz("Quartz", width=4, height=8, pointsize=12)
par(mfrow=c(1,1), cex=0.7, xpd=TRUE, yaxt="n")
plot(c(-10,85), yrangeusemod, type="n", # we may eventually want to zoom so you can see the SE
        xlab="day of event",
        ylab="")
text(ytxt, c(1:nrow(dat1)), as.vector(dat1$variety), adj=0, cex=1)
# leg.txt<- c("greenhouse", "vineyard")
# legend(0.75, 8, leg.txt,pch=wtpch, pt.bg=c("black","white"), bty="n")

# plot the RMI data 
x<-as.vector(dat1$doy.bb)
xsem<-as.vector(dat1$se.bb)
points(x,y,pch=wtpch[1], bg='white', col=prettycol, cex=1.2)
arrows(x-xsem,y,x+xsem,y,code=3,angle=90,length=0.0)

# plot the greenhouse data 
y1 <- c(1:nrow(dat2))
x1 <-as.vector(dat2$days.to.bb)
# xsem<-as.vector(dat2$se.bb)
points(x1,y1,pch=wtpch[2], bg='white', col=prettycol, cex=1.2)
# arrows(x-xsem,y,x+xsem,y,code=3,angle=90,length=0.0)
