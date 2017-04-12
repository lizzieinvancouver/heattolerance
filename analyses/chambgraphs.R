## Started 14 February 2017 ##
## By Lizzie ##

dat <- read.csv ("input/chamberobservations_grapes2016.csv", header=TRUE)

##subset
chds <- subset(dats, select=c(
  "Date",
  "RowNumNumRep", 
  "Var", 
  "days",
  "sm_cat",
  "length_mean",
  "lfnum_mean",
  "pf_mean",
  "bfall_mean",
  "vFcount_mean",
  "vFest_mean",
  "Treat"))

##super basic plot
ggplot(chds, aes(days, length_mean, color=Var, group=RowNumNumRep)) + 
  geom_point(aes(size=sm_cat), shape=1) + 
  geom_line() + 
  labs(x = "Time (days)", y = "Stem Length")

##plots by var
ggplot(chds, aes(days, length_mean, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots length by treat
ggplot(chds, aes(days, length_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length")

##plots leafnum by treat
ggplot(chds, aes(days, lfnum_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Leaf Number")

##plots perflow by treat
ggplot(chds, aes(days, pf_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Percent Flowering")

##plots budfall by treat
ggplot(chds, aes(days, bfall_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Number Buds Fallen")

##plot vfcounts as well
ggplot(chds, aes(days, vFcount_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Counts")

##plot vfextimates
ggplot(chds, aes(days, vFest_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Estimates")


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
