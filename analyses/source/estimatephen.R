## Started 14 Feb 2017 ##
## By Lizzie ##
## Adapted from davis_estimatephen.R ##

## Updated on 14 April 2017 ## 

## This file works to estimate a single day each individual plant reached a phenological stage ##
## NOTE: that the code is set up to delete leading zeros (0) and ones (1) only. This makes some sense. If a plant is stuck mid-development, we probably should not erase that data. The original code deleted only leading 0s but I can see many plants starting at EL 1 (or 1%) and deleting that being okay. So I adjusted the code. ##

## Since our data may often skip a key stage (e.g., we're interested in EL 5, but have data for EL 4 then EL 7) #
# We need a method to estimate what day each stage happened at. ##

## This code subsets the data to each individual (RowNumNumRep) then remove any duplicate values at the start or end #
# So, if you have 1, 1, 1, 5, 6, 10, 10, 10, 10 it should simplify to 1, 5, 6, 10 (though I think the start bit needs work) #
# then it calculates a linear regression and uses that regression to predict the day the stage you ask about happened #
# It also gives you the model info and the total number of observations you used to create the linear model ##

### f(x) to get estimate of 50% flowering and veraison and any EL stage ###
## requires a data frame with the following column names:
# RowNumNumRep (plant ID)
# Var (variety)
# EL_mean (mean for individual plant) ##change chamber data column names for perc flowering
# days (days since start of experiment)

## You need to provide:
# dat: the dataframe
# stage: a name for the stage (you really can write anything but it should be "budburst" or "flowering" or such
# percent: what percent you want it to estimate OR what EL stage
# EL: EL stage, only needed usually for estimating flowering

get_pheno_est <- function(dat, stage, percent, EL){
    RowNumNumRep <- unique(dat$RowNumNumRep) # RowNumNumRep must be row.plantnumber
    dat_final <- data.frame(matrix(data=NA,nrow=0,ncol=7))
    colnames(dat_final) <- c("RowNumNumRep","Var","stage","days","m","intercept","n")
        for(i in 1:length(RowNumNumRep)){ # i <- 1
            print(paste(i,RowNumNumRep[i],sep="_"))
            ## subsetting by RowNumNumRep
            temp <- dat[which(dat$RowNumNumRep==RowNumNumRep[i]),]
            ## removing NA values from the data
            temp <- temp[which(!is.na(temp$EL_mean)),] 
            if(nrow(temp)==0)
                {dat_final <-
                rbind(dat_final,data.frame(RowNumNumRep=RowNumNumRep[i],
                Var=dat$Var[which(dat$RowNumNumRep==RowNumNumRep[i])][1], stage=stage, days=NA, m=NA,
                intercept=NA,n=NA))}
            else{
		avg <- temp$EL_mean
		### clean up code here! shouldn't have any with single date point or same first and last
		if(nrow(temp)<=1 | temp$EL_mean[1]==temp$EL_mean[nrow(temp)]){days_50==temp[1,'days']}
                else{
		## removing duplicate values for avgvud at the end or beginning of data collection;
		num <- length(avg)
		count_f <- 1 # start a counter
                # remember: avg is the phenostage data as a vector
                # first the script tries to delete the repeating entries at the start    
		while(avg[count_f]==avg[count_f+1]){count_f <- count_f+1} # while this entry is the same as the next entry, step forward one on the count (add 1 to count_f) and ...
                # ask if the if-statement below if true, if so, delete 1:through to that count from temp
                # Note that the first clause of the if statement means that ONLY leading ones or zeroes are deleted (more above).
		if(count_f>1 & avg[1]==0|count_f>1 & avg[1]==1) {del_f <- c(1:(count_f-1)); temp <- temp[-del_f,]}
                # Now we need to reset based on any new dimensions (added by Lizzie)
                avg <- temp$EL_mean
                num <- length(avg)
                count_b <- num # get max to count to
                # next the script tries to delete the repeating entries at the end (this looks to work okay)
		while(avg[count_b]==avg[count_b-1]){count_b <- count_b-1} # while the count_b is the same as the count before count_b, delete count_b by 1 and ...
                 # ask if count_b is less than the num 
		if(count_b<num){del_b <- c((count_b+1):num);temp <- temp[-del_b,]}
		###clean up code here! shouldn't have any with single date point or same first and last
		if(nrow(temp)<=1){days_50==temp[1,'days']}else{
			mod <- lm(days~EL_mean,data=temp)
			days_50 <- percent*mod$coef["EL_mean"] + mod$coef["(Intercept)"]
		}
		dat_final <- rbind(dat_final ,data.frame(RowNumNumRep=temp[1,'RowNumNumRep'],
                   Var=temp[1,'Var'], stage=stage,days=days_50, m=mod$coef["EL_mean"],
                   intercept=mod$coef["(Intercept)"], n=nrow(temp)))
	       }
	} 
   }
   return(dat_final)
}
