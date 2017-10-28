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
rmilo <- read.csv(file="input/leafout_est_2015.csv", header=TRUE)

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

rmilo$variety[which(rmilo$variety=="Ungni blanc/Trebbiano")] <- "Ugni blanc/Trebbiano"
rmi.indlo <- rmilo[which(rmilo$variety %in% c(unique(gendat$Var_corr), "Valdepenas")),]
rmi.indlo$variety[which(rmi.indlo$variety=="Valdepenas")] <- "Tempranillo"

setdiff(unique(gendat$Var_corr),unique(rmi$variety))
setdiff(unique(datep$Var_corr),unique(rmi$variety))
setdiff(unique(datep$Var_corr),unique(rmi.indbb$variety))
setdiff(unique(datep$Var_corr),unique(rmi.indlo$variety))


####################
## Making a Table ##
####################

expttable1 <- 
  ddply(datnd, c("Var_corr"), summarise,
        n = length(Var_corr),
        numflow = sum(flowering12yn, na.rm = TRUE),
        diamean = mean(spurdiam_mean, na.rm = TRUE))

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

##Plotting logistic

quartz(title="diam v. flowering") # creates a quartz window with title

plot(datnd$spurdiam_mean,datnd$flowering23yn,xlab="Spur diameter",ylab="Chance of flowering") 

curve(predict(mod.23diam,data.frame(spurdiam_mean=x),type="resp"),add=TRUE)


#######################################
## Plotting RMI and GH data together ##
######################################


# Step 1: Get the data into mean and SE per var (did just for BB and LO, could add flo)
ghsum <-
      ddply(datep, c("Var_corr"), summarise,
      mean.bb = mean(days.to.bb),
      sd.bb = sd(days.to.bb),
      sem.bb = sd(days.to.bb)/sqrt(length(days.to.bb)),
      mean.lo = mean(days.to.lo),
      sd.lo = sd(days.to.lo),
      sem.lo = sd(days.to.lo)/sqrt(length(days.to.lo)))

rmimean.bb <-
      ddply(rmi.indbb, c("variety"), summarise,
      mean.bb = mean(doy, na.rm=TRUE),
      sd.bb = sd(doy),
      sem.bb = sd(doy)/sqrt(length(doy)))

rmimean.lo <-
      ddply(rmi.indlo, c("variety"), summarise,
      mean.lo = mean(doy, na.rm=TRUE),
      sd.lo = sd(doy),
      sem.lo = sd(doy)/sqrt(length(doy)))

# Step 2: Merge the data for ease
rmimeans <- merge(rmimean.bb, rmimean.lo, by="variety")
varmeans <- merge(ghsum, rmimeans, by.x="Var_corr", by.y="variety",
    suffixes=c(".gh", "rmi"))

varmeans <- varmeans[order(varmeans$mean.bb.gh),]

# Step 3: Plotting parameters
prettycol <- colorRampPalette(brewer.pal(9,"YlOrRd")[2:9])(nrow(varmeans))
pch=16
arrowalpha <- 0.25
cextext <- 0.6

# Step 4: Plot!
pdf(file.path("graphs/ghrmi_vars.pdf"), width = 8, height = 14)
par(mfrow=c(2,1))

# burburst
plot(mean.bbrmi~mean.bb.gh, data=varmeans, col=prettycol, pch=pch,
    xlim=c(10, 19), ylim=c(58, 76),
    xlab="Day of budburst in greenhouse (day since forcing)",
    ylab="Day of budburst in field (day of year)")
# Add error arrows?
x1 <-as.vector(varmeans$mean.bb.gh)
xsem1 <-as.vector(varmeans$sem.bb.gh)
y1 <- as.vector(varmeans$mean.bbrmi)
ysem1 <-as.vector(varmeans$sem.bbrmi)
arrows(x1,y1-ysem1,x1,y1+ysem1,code=3,angle=90,length=0.0, col=alpha(prettycol, arrowalpha))
arrows(x1-xsem1,y1,x1+xsem1,y1,code=3,angle=90,length=0.0, col=alpha(prettycol, arrowalpha))
# add linear fit
abline(lm(mean.bbrmi~mean.bb.gh, data=varmeans))
# label varieties
text(varmeans$mean.bb.gh, varmeans$mean.bbrmi, 
    varmeans$Var_corr,
    cex = cextext, 
    pos = 3)
legend(21, 76, legend=varmeans$Var_corr, pch=16, col=alpha(prettycol, arrowalpha))

# leafout
plot(mean.lormi~mean.lo.gh, data=varmeans, col=prettycol, pch=pch,
    xlim=c(14, 24), ylim=c(63, 84),
    xlab="Day of leafout in greenhouse (day since forcing)",
    ylab="Day of leafout in field (day of year)")
# Add error arrows?
x1 <-as.vector(varmeans$mean.lo.gh)
xsem1 <-as.vector(varmeans$sem.lo.gh)
y1 <- as.vector(varmeans$mean.lormi)
ysem1 <-as.vector(varmeans$sem.lormi)
arrows(x1,y1-ysem1,x1,y1+ysem1,code=3,angle=90,length=0.0, col=alpha(prettycol, arrowalpha))
arrows(x1-xsem1,y1,x1+xsem1,y1,code=3,angle=90,length=0.0, col=alpha(prettycol, arrowalpha))
# add linear fit
abline(lm(mean.lormi~mean.lo.gh, data=varmeans))
# label varieties
text(varmeans$mean.lo.gh, varmeans$mean.lormi, 
    varmeans$Var_corr,
    cex = cextext, 
    pos = 3)

dev.off()

summary(lm(mean.lormi~mean.lo.gh, data=varmeans))
summary(lm(mean.bbrmi~mean.bb.gh, data=varmeans))

## Alternative Step 4: Put it all on one plot
pdf(file.path("graphs/ghrmi_vars_oneplot.pdf"), width = 8, height = 7)
par(xpd=FALSE)
par(mar=c(5.1, 4.1, 4.1, 6.5))
plot(mean.lormi~mean.lo.gh, data=varmeans, col=prettycol, pch=17,
    xlim=c(10, 24), ylim=c(58, 84),
    xlab="Day of event in greenhouse (day since forcing)",
    ylab="Day of event in field (day of year)")
points(mean.bbrmi~mean.bb.gh, data=varmeans, col=prettycol, pch=16)
abline(lm(c(varmeans$mean.bbrmi, varmeans$mean.lormi)~c(varmeans$mean.bb.gh,
    varmeans$mean.lo.gh)))
par(xpd=TRUE)
legend("topleft", legend=c("budburst", "beafout"), bty="n",  pch=c(16, 17))
legend(24.75, 86, varmeans$Var_corr, col=prettycol, bty="n", pch=16, cex=0.6)
dev.off()
