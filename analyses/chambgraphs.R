## Started 14 February 2017 ##
## By Lizzie ##

dat <- read.csv ("output/clchambdata.csv", header=TRUE)

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
  "pflowr_mean",
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

##plots change in stem length by days separated by variety
ggplot(chds, aes(days, stemlenchange, color=Treat)) +
  geom_point() +
  facet_wrap(~Var) +
  geom_line() + labs(x = "Time (days)", y = "Stem Length Change (mm)") ##not sure about this unit

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

##plots budfall by day 
ggplot(chds, aes(days, bfall_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Number Buds Fallen")

##plot capfall by day
ggplot(chds, aes(days, capfall_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "Caps Fallen in Bag")

##plot vfcounts by day
ggplot(chds, aes(days, vFcount_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Counts")

##plot vFper from loop by day
ggplot(chds, aes(days, vFper_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Counts")

##plot vfextimates by day
ggplot(chds, aes(days, vFest_mean, color=Var)) +
  geom_point() +
  facet_wrap(~Treat) +
  geom_line() + labs(x = "Time (days)", y = "vitisFlower app Flower Estimates")

##boxplot percent flowering by variety
ggplot(chds, aes(Var, pf_mean, color=Var)) +
  geom_boxplot() +
  facet_wrap(~Treat) +
  labs(x = "Variety", y = "Percent Flowering") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##boxplot budfall by variety
ggplot(chds, aes(Var, bfall_mean, color=Var)) +
  geom_boxplot() +
  facet_wrap(~Treat) +
  labs(x = "Variety", y = "Buds Fallen into Bag") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

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
