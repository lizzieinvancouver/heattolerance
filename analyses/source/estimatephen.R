## Started 14 Feb 2017 ##
## By Lizzie ##
## Adapted from davis_estimatephen.R ##

## This file works to estimate a single day each individual plant reached a phenological stage ##

## Since our data may often skip a key stage (e.g., we're interested in EL 5, but have data for EL 4 then EL 7) #
# We need a method to estimate what day each stage happened at ##

## This code subsets the data to each individual (RowNumNumRep) then remove any duplicate values at the start or end #
# So, if you have 1, 1, 1, 5, 6, 10, 10, 10, 10 it should simplify to 1, 5, 6, 10 (though I think the start bit needs work) #
# then it calculates a linear regression and uses that regression to predict the day the stage you ask about happened #
# It also gives you the model info and the total number of observations you used to create the linear model ##

### f(x) to get estimate of 50% flowering and veraison and any EL stage ###
## requires a data frame with the following column names:
# RowNumNumRep (plant ID)
# Var (variety)
# EL_mean (mean for individual plant)
# days (days since start of experiment)
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
		count_b <- num # get max to count to
                # remember: avg is the stage data as a vector
                # first the script tries to delete the repeating entries at the start    
		while(avg[count_f]==avg[count_f+1]){count_f <- count_f+1} # while this entry is the same as the next entry ...
                # ask if the if-statement below if true, if so, delete 1:through to that count from temp
                    # the deletion of rows seems to be working, but the count_f>1&avg[1]==0 doesn't make sense to me
                    # specifically the avg[1]==0 seems wrong, we should be really asking if the row before and after are the same.... 
		if(count_f>1&avg[1]==0) {del_f <- c(1:(count_f-1)); temp <- temp[-del_f,]}
                # next the script tries to delete the repeating entries at the end (this looks to work okay)
		while(avg[count_b]==avg[count_b-1]){count_b <- count_b-1} # while the count_b is the same as the count before count_b
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
