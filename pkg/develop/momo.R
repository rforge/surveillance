######################################################################
# Importing data from a csv file. As well as population.
######################################################################
momo.ts <- read.csv("~/Surveillance/EuroMoMo/Data/mortality-dk.csv",header=TRUE,check.names=FALSE)
#Fill week slot with Monday of each week , starting from 3rd Jan 1994
dates <- as.Date("1994-01-03") + 7 * 0:(nrow(momo.ts)-1)
#Create sts object
momo <- new("sts",epoch=as.numeric(dates), start=c(1994,1), freq=52,
           observed=momo.ts, epochAsDate=TRUE)
population(momo) <- as.matrix(read.csv("~/Surveillance/EuroMoMo/Data/population-dk.csv",check.names=FALSE))
save(file="~/Surveillance/surveillance/pkg/data/momo.RData",list=c("momo"))
