## Started 14 February 2017 ##
## By Lizzie and Nicole! ##

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/vinmisc/heattolerance/analyses") 
} else
  setwd("/Users/Nicole/GitHub/heattolerance/analyses/")

# Get packages
library(ggplot2)

dat <- read.csv ("output/clchambdata.csv", header=TRUE)
dat50 <- read.csv ("output/chamb50fl.csv", header=TRUE)

##subset
chds <- subset(dat, select=c(
  "Date",
  "RowNumNumRep", 
  "Var", 
  "days",
  "sm_cat",
  "length_mean",
  "lfnum_mean",
  "pf_mean",
  "bfall_mean",
  "capfall_mean",
  "vFcount_mean",
  "vFest_mean",
  "vFper_mean",
  "EL_mean",
  "stemlenchange",
  "lfchange",
  "Treat"))

##super basic plot
ggplot(chds, aes(days, length_mean, color=Var, group=RowNumNumRep)) + 
  geom_point(aes(size=sm_cat), shape=1) + 
  geom_line() + 
  labs(x = "Time (days)", y = "Stem Length")

##plots stem length by days separated by variety
ggplot(chds, aes(days, length_mean, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

## ALT: Alternative version of above plot that shows each rep, plots stem length by days separated by variety
ggplot(chds, aes(days, length_mean, color=RowNumNumRep, shape=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots change in stem length by days separated by variety
ggplot(chds, aes(days, stemlenchange, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length Change (mm)") ##not sure about this unit

##plot change in stem length by treatment
ggplot(chds, aes(Treat, stemlenchange, color=Var)) +
  geom_point() +
  labs(x = "Treatment", y = "Stem Length Change (mm)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##boxplot of stem length change by treatment
ggplot(chds, aes(Treat, stemlenchange)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Stem Length Change (mm)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plots length by days separated by treat
ggplot(chds, aes(days, length_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots leafnum by days
ggplot(chds, aes(days, lfnum_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Leaf Number")

##plots change leafnum by days
ggplot(chds, aes(days, lfchange, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Leaf Number Change")

##plots change in leaf num by days separated by variety
ggplot(chds, aes(days, lfchange, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Leaf Number Change")

##plots change in leaf num by treatment
ggplot(chds, aes(Treat, lfchange, color=Var)) +
  geom_point() +
  labs(x = "Treatment", y = "Leaf Number Change") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##boxplot of change in leaf num by treatment
ggplot(chds, aes(Treat, lfchange)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Leaf Number Change") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plots perflow by days
ggplot(chds, aes(days, pf_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Percent Flowering")

##plots pflowr (from loop) by day - not much change
ggplot(chds, aes(days, pflowr_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Percent Flowering")

##boxplot percent flowering by variety
ggplot(chds, aes(Var, pf_mean, color=Var)) +
  geom_boxplot() +
  facet_wrap(~Treat) +
  labs(x = "Variety", y = "Percent Flowering") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot percent flowering by treatment
ggplot(chds, aes(Treat, pf_mean, color=Var)) +
  geom_point() +
  labs(x = "Treatment", y = "Percent Flowering") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot vfcounts by day
ggplot(chds, aes(days, vFcount_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Counts")

##plot vFcounts by Variety
ggplot(chds, aes(Var, vFcount_mean, color=Treat)) +
  geom_point() +
  labs(x = "Variety", y = "vitisFlower app Flower Counts") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot vFper from loop by day
ggplot(chds, aes(days, vFper_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Counts")

##plot vfestimates by variety
ggplot(chds, aes(Var, vFest_mean, color=Treat)) +
  geom_point() +
  labs(x = "Variety", y = "vitisFlower app Flower Estimates") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


##plots budfall by day 
ggplot(chds, aes(days, bfall_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Number Buds Fallen")

##boxplot budfall by variety
ggplot(chds, aes(Treat, bfall_mean)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Buds Fallen into Bag") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot budfall by treatment
ggplot(chds, aes(Treat, bfall_mean, color=Var)) +
  geom_point() +
  labs(x = "Treatment", y = "Buds Fallen into Bag") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot capfall by day
ggplot(chds, aes(days, capfall_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Caps Fallen in Bag")

##boxplot capfall by variety
ggplot(chds, aes(Var, capfall_mean, color=Var)) +
  geom_boxplot() +
  facet_wrap(~Treat) +
  labs(x = "Variety", y = "Caps Fallen into Bag") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##boxplot capfall by treatment, see wiki (a)
ggplot(chds, aes(Treat, capfall_mean, color=Var)) +
  geom_point() +
  labs(x = "Variety", y = "Caps Fallen into Bag") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##plot 50% flowering estimate phen by individ
ggplot(dat50, aes(RowNumNumRep, days, color=Var)) +
  geom_point() +
  labs(x = "Time (days)", y = "Plant ID") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## plots 50% flowering estiamtephen by variety
ggplot(dat50, aes(Var, days)) +
  geom_boxplot() +
  labs(x = "Time (days)", y = "Variety") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##
## Trying to figure out how to show treatment, variety, time and INDIVIDUAL ... a couple ideas
## plots stem length by days separated by variety with shape for RowNumNumRep
ggplot(chds, aes(days, pflowr_mean, color=Treat, shape=RowNumNumRep)) + 
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots stem length by days separated by variety with linetype, this meh... 
ggplot(chds, aes(days, pflowr_mean, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line(aes(linetype=RowNumNumRep)) + labs(x = "Time (days)", y = "Stem Length")
## Hmm, I think I will work on writing a longer graping script from base graphics for this. More to come!
## End of 'Trying to figure out how to show treatment, variety, time and INDIVIDUAL' section


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
