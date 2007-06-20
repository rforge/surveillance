library(surveillance)

#source("../R/AllClass.R")
#source("../R/AllGeneric.R")
#source("../R/sts.R")
#This is no good
#source("../develop/class.R")

##S3 testers
data(ha)
plot(ha)
plot(aggregate(ha))
plot(aggregate(ha),legend.opts=list(horiz=TRUE))


#Load data
data(ha)
shpFile <- system.file("shapes/berlin.shp",package="surveillance")
ha <- disProg2sts(ha, map=readShapePoly(shp,IDvar="SNAME"))
ha4 <- aggregate(ha,nfreq=13)
ha4.abyunit <- aggregate(ha4,by="unit")

validObject(ha4)
validObject(ha4.abyunit)

#This yields an error (the shape info doesn match anymore, colnames == NULL)
plot(ha4.abyunit, type = observed ~ 1 | unit)

#Show some plots for weekly data
plot(ha) # corresponds to observed ~ time | unit
plot(ha,type=observed ~ time )
plot(ha,type=observed ~ time | unit)
plot(ha,type=observed ~ time | unit, xaxis.years=FALSE)

#Show some plots for monthly data
plot(ha4,type=observed ~ time )
plot(ha4,type=observed ~ time , xaxis.years= FALSE)
plot(ha4,type=observed ~ time | unit)
plot(ha4,type=observed ~ 1 | unit)
plot(ha4,type=observed ~ 1 | unit,axes=FALSE)#, main = "Hepatitis A in Berlin 2001-2006" )

#Movies
plot(ha4, type = observed ~ 1 | unit * time,wait.ms=250)

#dev.printer <- list(device=png,extension=".png",width=640,height=480,name="/tmp/animation/berlin")
dev.printer <- list(device=png,name="/tmp/animation/berlin")
plot(ha4, type = observed ~ 1 | unit * time, dev.printer=dev.printer)

system(paste("convert -delay 50 ",dev.printer$name,"*.png /tmp/animation/animated.gif",sep=""))


##Apply the Bayes algorithm
ha4.b62 <- bayes(ha4,control=list(range=52:73,b=2,w=6,alpha=0.001))
plot(ha4.b62,type=observed ~ time | unit, xaxis.years=FALSE)
plot(ha4.b62,type=observed ~ time | unit)
plot(ha4.b62,type=observed ~ 1 | unit * time)

#This should give an error message about a missing map..??!
plot(sts, type = observed ~ 1 | unit, xaxis.years=FALSE)

#Do it on the ha4 object
ha4.f62 <- farrington(ha4,control=list(range=52:73,b=2,w=6,alpha=0.001))
plot(ha4.f62, type = observed ~ time | unit )

#############################################################################################
##Try all surveillance methods
#############################################################################################
ha4.rki <- rki(ha4,control=list(range=52:73,b=2,w=6))
plot(ha4.rki, type = observed ~ time | unit )

#Cusum/Rossi
kh <- find.kh(ARLa=500,ARLr=7)
ha4.rossi <- cusum(ha4,control=list(range=52:73,k=kh$k, h=kh$h, trans="rossi"))
plot(ha4.rossi)

ha4.rossi <- cusum(ha4.abyunit,control=list(range=52:73,k=kh$k, h=kh$h, trans="rossi"))
plot(ha4.rossi,ylegpos=0.9)

plot(ha4.rossi,ylegpos=0.9,outbreak.symbol=list(pch=3,col=3,cex=1.5), ,alarm.symbol=list(pch=24, col=2, cex=1.5))

#GLR pois
ha4.glrpois <- glrpois(ha4,control = list(range=52:73,c.ARL=5, S=1,beta=NULL, Mtilde=1, M=-1, change="intercept",theta=NULL))
plot(ha4.glrpois)
ha4.glrpois <- glrpois(ha4.abyunit,control = list(range=52:73,c.ARL=5, S=1,beta=NULL, Mtilde=1, M=-1, change="intercept",theta=NULL))
plot(ha4.glrpois)


##Test accesss
nrow(ha)
nrow(ha[50:100,1:2])
ncol(ha[50:100,1:2])

plot(ha4[2:20,1:10],type = observed ~ time |unit)

colnames(ha)
plot(ha4[,c("chwi","span")])


#Try the measels data instead -- no map here!
data(measels.weser)
measels.weser <- new("sts",measels.weser,map=NULL)
plot(measels.weser, type = observed ~ time | unit)
control=list(range=80:104, b=1, w=3, reweight=TRUE, verbose=FALSE,alpha=0.01)
measels.far <- farrington(measels.weser,control)

plot(measels.far, type = observed ~ time | unit)
plot(measels.far, type = observed ~ time | unit,xaxis.years=FALSE)

