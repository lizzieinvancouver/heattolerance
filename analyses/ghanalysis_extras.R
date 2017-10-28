## Started 28 October 2017 (with code from Sept-Oct) ##
## By Lizzie and Nicole ##

## File requires running data read in and clean up from ghanalysis.R to run ##
## Less great versions of ways to plot GH and RMI bb and LO data ##
## But such nice code that I wanted to hold onto it ##

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

y <-c(1:nrow(dat1))
ytxt <- c(-37) # push the text way over left
wtpch <- c(16, 18, 1, 5)
yrangeusemod <- c(1, nrow(dat1))
prettycol <- colorRampPalette(brewer.pal(9,"YlOrRd")[2:9])(nrow(dat1))

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
