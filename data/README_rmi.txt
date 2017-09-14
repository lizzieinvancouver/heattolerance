Started 14 Sep 2017
By Lizzie

Some notes on the Davis data
Data (analyses/input/budburst_est_2015.csv and leafout_est_2015.csv) created in davisphen_clean.R (vin repo). 

Budburst was considered EL stage 4
Leafout was considered EL stage 7

Each stage was calculated at the level of each individual plant (usually two,  but sometimes more and a few times just one). 

For example, for CabSauv, we just had one plant (I went back to the raw data and checked).

Here’s the code that takes the average of each variety (by plant):

avgerageplants <- function(datafile, varcolname){
    dat.var <- ddply(datafile, c(varcolname), summarise,
    n = sum(n),
    sd = sd(doy),                   
    doy = mean(doy, na.rm=TRUE))
    dat.var$se <- dat.var$sd/sqrt(dat.var$n)
    return(dat.var)
}

Because Valdepenas and Tempranillo are the same thing I built new R code (rmidat.R) to create output/bblo_2015syn.csv. I also updated the averageplants f(x) to count the number of plants (nplants) in addition to the total dates of observation (n). 