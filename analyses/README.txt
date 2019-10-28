READ ME

Started by Nicole on 22 October 2019

Effects of high temperatures on winegrape flowering phenology

R code details

In order for code to work successfully, the files must be read in the following order: cleaning.R, rmidat.R, ghcleaning.R, ghanalysis.R, chambercleaning.R, chambanalysis.R.  estimatephen.R and ELgraphs.R are not directly tied to any of the other files.

cleaning.R began the process of cleaning up the data by editing some header names, removing non-numerics, and changing the date and days column so that it was formatted as the day of the year for easier analysis.  nodespurdater and id.sm (baseline measurements taken at the start of the experiment and information for matching variety codes with variety names, respectively) were joined.

rmidat.R cleans up the data form the RMI 2015 growing season so it can be compared to our greenhouse data.

ghcleaning.R does general cleaning of the data from the greenhouse.  It also contains code to figure out how far each plant developed and the min/max/mean budburst and leafout dates.  Finally, it marks if each plant made it to specific stages (EL4, EL7, EL12, EL23).  

ghanalysis.R first corrects variety name errors from previous files.  It then creates a table to summarize the varieties involved in the study, their mean starting diameter, and the mean budburst date. There are some early attempts at plotting the data, and then the logistic curve comparing baseline diameter with reaching the flowering stage.  Finally, this file also has the plot comparing the greenhouse development with that of the UC Davis RMI Vineyard data.

chambercleaning.R cleans the data taken in the growth chambers so that headers work with the code, corrects non-numerics and some fields (like the day of the year), removes some unnecessary data, and converts chamber and soil moisture to better values/phrasing for plotting. It also calculates the change in leaf number and stem length.

chambanalysis.R finalizes cleaning of the data and creates plots of the chamber results.

estimatephen.R is a function to estimate the day each plant reached a specific EL stage.

ELgraphs.R takes the greenhouse data and runs it through estimatephen.R and then plots each plant's development through time.