######################################################################
# Author: Michael Höhle <http://www.stat.uni-muenchen.de/~hoehle>
# Date:   18 Nov 2008
# Description:
# R file for the talk "The R package surveillance"
# at the workshop on
# Computer supported outbreak detection and signal management
# Robert Koch Institute, Berlin, Germany, 18 Nov 2008
#
# Comments: This file was generated using Sweave
######################################################################

options(width=70,prompt="R> ")



######################################################################
# Load the package and the hepatitis A data. Plot the data
######################################################################
library("surveillance")
data("ha")
plot(aggregate(ha),main="Hepatitis A in Berlin 2001-2006")


######################################################################
# Importing data from a csv file. 
######################################################################
ha.counts <- as.matrix(read.csv("ha.csv")) 
ha <- new("sts",week=1:nrow(ha.counts), start=c(2001,1), freq=52,
           observed=ha.counts,
           state=matrix(0,nrow(ha.counts),ncol(ha.counts)))


#Read ESRI shapefile for the hepatitis A data and set the
#map slot. This was not shown in the presentation.
#Alternatively, one could have set the "map" slot when calling
#new. Important is that the IDvar in the shapefile matches the
#column names in the "observed" slot.
shp <- system.file("shapes/berlin.shp",package="surveillance")
ha@map <- readShapePoly(shp,IDvar="SNAME")


## #Show contents of an ha object  
## print(ha)


#Show contents of an ha object  
print(ha)


######################################################################
#Examples of using matrix access functions on an sts object
######################################################################
dim(ha)


######################################################################
#Examples of using the aggregate function
######################################################################
dim(aggregate(ha, by="unit"))
dim(aggregate(ha, by="time"))


######################################################################
#Access a slot, either using "@" or head(slot(ha),n=1)
######################################################################
head(ha@observed,n=1)


#Aggregate 4 weeks - this results in a frequency of 13 observations per
#year (so these are not really months)
ha4 <- aggregate(ha[,c("pank","mitt","frkr","scho","chwi","neuk")],nfreq=13)
dim(ha4)


## ######################################################################
## # Illustration of the plot function
## ######################################################################
## 
## #Plot using type = observed ~ time, i.e. data are aggregated by unit
## #and are then plotted. This is equivalent to 
## #plot(aggregate(ha4,by="unit"),type= observed ~ time | unit)
## plot(ha4,type=observed ~ time)
## 


#Actually a slightly different call is used to get a different legend
#as the standard plot
plot(ha4,type=observed ~ time, legend.opts=list(x="topleft",legend=c("Infected")))


## #Plot using type = observed ~ time | unit, i.e. we plot the multivariate
## #time series
## plot(ha4,type=observed ~ time | unit)


#Again we use a slightly different call to tune the legend
plot(ha4,type=observed ~ time | unit, par.list=list(mar=c(4,3,1,1)))


## #Plot using type = observed ~ 1 | unit, i.e. we aggregate data by "time"
## #and plot it. Result is equivalent to 
## #plot(aggregate(ha4,by="time"),type=observed ~ 1 | unit)
## plot(ha4,type=observed ~ 1 | unit)


par(mar=c(0,0,0,0))
#Plot using type = observed ~ 1 | unit, i.e. we aggregate data by "time"
#and plot it. Result is equivalent to 
#plot(aggregate(ha4,by="time"),type=observed ~ 1 | unit)
plot(ha4,type=observed ~ 1 | unit)


## #This was not shown, but type = observed~1|time*unit gives a spatio
## #temporal illustration (whether that is useful is another issue)
## #Use Cntrl-C Cntrl-C to interupt the visualization
## plot(ha,type= observed~1|time*unit)


######################################################################
#Example of how to create customized graphics.
#Another option is to inject code into plot using too hook
#function
######################################################################

ha41 <- aggregate(ha4,by="unit")
plot(ha41,type=observed ~ time | unit,legend.opts=list(x="top",legend=c("Total","Pankow"),fill=c("lightblue","blue"),horiz=TRUE,lty=c(0,0)),col="lightblue")
plot(ha4[,"pank"],type=observed ~ time | unit,legend.opts=NULL,col="blue",add=TRUE)
text(58,10,"Outbreak\n in Pankow\n and Mitte")
arrows(65,10,71,10,length=0.1)
polygon(c(19,19,29,29),c(0,7,7,0),col=gray(0.9),border=NA)
text(24,3.5,"No data", srt=90, pos=1)


######################################################################
# Illustrate how the Farrington algorithm works
######################################################################
par(mar=c(2, 4, 4, 2) + 0.1)
#Setup Farrington control object
cntrlFar <- list(range=53:73,w=2,b=3,alpha=0.01)
#Adopt control object so exactly one computation is shown and 
#setting the argument "plot" a picture is shown
one.t <- cntrlFar ; one.t$range <- min(one.t$range) ; one.t$plot <- TRUE
#Perform surveillance for one time point and save the graphics
onet <- farrington(ha41,control=one.t)


######################################################################
#Surveillance using farrington
######################################################################

#specify control options and call the farrington function
cntrlFar <- list(range=53:73,w=2,b=3,alpha=0.01)
survha <- farrington(ha41,control=cntrlFar)


#Show the results of the surveillance
plot(survha,legend.opts=list(x="topleft",horiz=TRUE),xlab="time (months)")


#Show the effect of the limit54 argument in control 
cntrlFar$limit54 <- c(0,4)
survha <- farrington(ha41,control=cntrlFar)


plot(survha,legend.opts=list(x="topleft",horiz=TRUE),xlab="time (months)")


######################################################################
# This block shows the effect of the powertrans control argument
######################################################################
cntrlFar$powertrans <- "2/3"
upper.pt23 <- farrington(ha41,control=cntrlFar)@upperbound
cntrlFar$powertrans <- "1/2"
upper.pt12 <- farrington(ha41,control=cntrlFar)@upperbound
cntrlFar$powertrans <- "none"
upper.ptnone <-farrington(ha41,control=cntrlFar)@upperbound


#Reset alarm part - so the information is ignored
survha@alarm <- 0*survha@alarm
#Show results and plot legend manually
plot(survha,legend.opts=NULL,xlab="time (months)",ylim=c(0, max(upper.pt23,upper.ptnone,upper.pt12,survha@observed)),lwd=c(1,1,2),lty=c(1,1,2),main="")
#lines(c(1:nrow(survha),nrow(survha)+0.5),c(upper.pt23,upper.pt23[nrow(survha)]),type="s",col="magenta",lwd=2,lty=2)
lines(c(1:nrow(survha),nrow(survha)+0.5),c(upper.pt12,upper.pt12[nrow(survha)]),type="s",col="magenta",lwd=2,lty=2)
lines(c(1:nrow(survha),nrow(survha)+0.5),c(upper.ptnone,upper.ptnone[nrow(survha)]),type="s",col="lightblue",lwd=2,lty=2)
legend(x="topleft",c("2/3","1/2","none"),col=c("blue","magenta","lightblue"),lwd=2,lty=1,horiz=TRUE)


######################################################################
# Illustrate Rossi CUSUM approach implemented in function "cusum"
######################################################################
kh <- find.kh(ARLa=500,ARLr=7)
cntrlRossi <- list(range=209:290,k=kh$k,h=kh$h,trans="rossi",m=NULL)
ha.cs <- cusum(aggregate(ha,by="unit"),control=cntrlRossi)


#Plot results
plot(ha.cs,legend.opts=list(x=nrow(ha.cs)/2,y=6,xjust=0.5,legend = c("Infected", "Threshold",expression(m[t]),"Alarm"),lty=c(1,2,1,NA)),outbreak.symbol=list(pch=NA,col="green"))
#Show the estimated in-control mean estimated from data in range 1:208
lines(c(1-0.5,nrow(ha.cs)+0.5),rep(ha.cs@control$m,2),col="green",lwd=1)



######################################################################
# Towards multivariate surveillance
######################################################################

#perform farrington on each series in ha4 using the following
#control argument
control <- list(b=3,w=2,range=53:73,alpha=0.01,limit54=c(0,1))
ha4.surv <- farrington(ha4,control=control)


#Show results
plot(ha4.surv,par.list=list(mar=c(4,1,1,1)))


## ######################################################################
## # Towards multivariate surveillance
## ######################################################################
## 
## #perform farrington on each series in ha4 using the following
## #control argument
## control <- list(b=3,w=2,range=53:73,alpha=0.01,limit54=c(0,1))
## ha4.surv <- farrington(ha4,control=control)


######################################################################
# This block shows how to extract results from the sts object
# created by the repeated univariate surveillance
######################################################################
options(digits=3)


## #Extract important slots of sts object for the last week
## sapply(c("observed","upperbound","alarm"),
##        function(str) { slot(ha4.surv,str)[nrow(ha4.surv),]}
## )


#Extract important slots of sts object for the last week
sapply(c("observed","upperbound","alarm"),
       function(str) { slot(ha4.surv,str)[nrow(ha4.surv),]}
)


#Illustrate results using a multivariate time series
plot(ha4.surv, type= alarm ~ time | unit)


#Illustrate results of surveillance using a map
plot(ha4.surv[nrow(ha4.surv),], type= observed ~ 1 | unit)
title("August 2006")


