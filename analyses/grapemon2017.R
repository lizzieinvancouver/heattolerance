## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/Lizzie/Documents/git/projects/vinmisc/heattolerance/analyses/input")

goo <- read.csv("grapemon2017.csv", header=TRUE) # see data/greenhousemonitoring_2017/grapeMonitoringAsa2017_Updated.xlsx
goo$X <- NULL
goo$X.1 <- NULL
goo$X.2 <- NULL
goo$RowNumNumRep <- paste(goo$RowNum, goo$Num, goo$Rep, sep=".")

flowbuds <- subset(goo, Flowbuds=="Y")
flow <- subset(goo, Flowers=="Y")
fruit <- subset(goo, Notes=="GRAPES!")

sort(unique(goo$RowNumNumRep)) # 68
sort(unique(flowbuds$RowNumNumRep)) # 11
sort(unique(flow$RowNumNumRep)) # 6
sort(unique(fruit$RowNumNumRep)) # 6

# buds but no flowers?
unique(flowbuds$RowNumNumRep)[which(!unique(flowbuds$RowNumNumRep) %in% unique(flow$RowNumNumRep))]

# was flowering with Nicole:
# 19.9.R3
# 38.7.R2
# 38.7.R6

# check
# 22.9.R2 - stalled at stage 11 (12 is when inflorescence is clear)
# 35.3.R3 - stalled at stage 11
# 38.7.R1 - stalled at stage 11
