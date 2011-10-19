######################################################################
# This block is for testing only
######################################################################

source("backproj.R")

######################################################################
# Hook function to use for back-projection
######################################################################

plotIt <- function(Y,lambda,...) {
  par(las=2)
  T <- length(Y)
  plot(1:T,Y,type="h",ylab="Cases",lwd=2,xaxt="n",xlab="",...)
  lines(1:T+0.3,lambda,type="h",col="gray",lty=1,lwd=2)  
  axis(1,at=1:T,label=1:T,cex.axis=0.7)
  legend(x="topleft",c(expression(Y[t]),expression(lambda[t])),col=c(1,"gray"),lty=c(1,1),lwd=2)

  invisible()
}

#T <- max(times) #lets say the last days are not good yet
#Y <- table(factor(times,levels=1:max(times)))[1:T]
#T <- length(Y)

######################################################################
#Simulated outbreak
######################################################################

#Incubation time distribution vector (support starts at zero!)
inc.pmf <- c(0,pgamma(1:25,15,1.4) - pgamma(0:24,15,1.4))
inc.cdf <- cumsum(inc.pmf)

#Wrap above array within PMF and CDF functions with discrete support.
dincu <- function(x) {
  notInSupport <- x<0 | x>=length(inc.pmf)
  #Give index -1 to invalid queries
  x[notInSupport] <- -1
  return(c(0,inc.pmf)[x+2])
}

pincu <- function(x) {
  x[x<0] <- -1
  x[x>=length(inc.cdf)] <- length(inc.cdf)-1
  return(c(0,inc.cdf)[x+2])
}

####
rincu <- function(n) {
  sample(0:25, size=n, replace=TRUE, prob=inc.pmf)
}


barplot(inc.pmf,names.arg=0:25)

######################################################################
#Simulate outbreak starting at time t0 of length l
######################################################################
t0 <- 23
l <- 10

#Sample time of exposure and length of incubation time
n <- 1e3
exposureTimes <- t0 + sample(x=0:(l-1),size=n,replace=TRUE)
symptomTimes <- exposureTimes + rincu(n)

X <- table( factor(exposureTimes,levels=1:(max(symptomTimes)+25)))
Y <- table( factor(symptomTimes,levels=1:(max(symptomTimes)+25)))

plot(1:length(Y),as.numeric(Y),type="h",xlab="",lwd=2,ylim=c(0,max(X,Y)))
lines(1:length(Y)+0.2,X,col="gray",type="h")

#Create sts object
library("surveillance")
sts <- new("sts", epoch=1:length(Y),observed=matrix(Y,ncol=1), alarm=0*matrix(Y,ncol=1))
plot(sts,xaxis.years=FALSE,legend=NULL)
  
#Call non-parametric back projection function
sts.bp <- backprojNP(sts, k=0,incu.pmf.vec=inc.pmf,eps=0.005,hookFun=plotIt,ylim=c(0,max(X,Y)))
  
plot(sts.bp,xaxis.years=FALSE,legend=NULL,las=1)
lines(1:length(Y),X,col=2,type="h")

#Test bootstrap version
#debug("backprojNPBoot")
#sts.bp2 <- backprojNP.ci(sts, k=c(0,2),incu.pmf.vec=inc.pmf,eps=c(0.005,0.01),B=100,hookFun=plotIt,ylim=c(0,max(X,Y)))
sts.bp0 <- backprojNP.ci(sts, k=0,incu.pmf.vec=inc.pmf,eps=c(0.005,0.005),B=100,hookFun=NULL)
sts.bp2 <- backprojNP.ci(sts, k=2,incu.pmf.vec=inc.pmf,eps=c(0.005,0.005),B=100,hookFun=NULL,ylim=c(0,max(X,Y)))


#Plot type 1
plot(sts.bp2,legend=NULL,xaxis.years=FALSE,dx.upperbound=0)
segments(1:nrow(sts.bp2),sts.bp2@control$ci[,1],1:nrow(sts.bp2),sts.bp2@control$ci[,2],col="green")


#Plot type 2
plot(sts.bp2,legend=NULL,xaxis.years=FALSE,dx.upperbound=0,,main="",ylim=c(0,max(sts.bp2@control$ci)))
plot(upperbound(sts.bp2),type="l")
for (i in 1:nrow(sts.bp2)) {
  polygon( x=c(i,i,i+1,i+1), y=c(sts.bp2@control$ci[i,1],
                                 sts.bp2@control$ci[i,2],
                                 sts.bp2@control$ci[i,2],
                                 sts.bp2@control$ci[i,1]),col="lightgray")
}

#Plot 3 -- no seperate plot function exists for these objects
#          do it manually.
plot(upperbound(sts.bp2),type="n",ylim=c(0,max(sts.bp2@control$ci)),ylab="Cases",xlab="time")
polygon( c(1:nrow(sts.bp2),rev(1:nrow(sts.bp2))),
         c(sts.bp2@control$ci[,2],rev(sts.bp2@control$ci[,1])),col="lightgray")
lines(upperbound(sts.bp2),type="l",lwd=2)
#legend(x="topright",c(expression(lambda[t]),"95% CI"),lty=c(1,NA),col=c(1,NA),fill=c(NA,"lightgray"),border=c(NA,1),lwd=c(2,NA))
legend(x="topright",c(expression(lambda[t])),lty=c(1),col=c(1),fill=c(NA),border=c(NA),lwd=c(2))
#Add truth for comparison
lines(1:length(Y),X,col=2,type="h")








######################################################################
#Do the EM looping
######################################################################
bp.k0 <- backprojNP(Y=Y,k=2,dincu=dincu,pincu=pincu,eps=1e-5,hookFun=plotIt,ylim=c(0,max(X,Y)))
lines(1:length(Y),X,col=2,type="h")

incu.sample <- rincu(1e5)
delta.star <- quantile(incu.sample,p=(1:length(symptomTimes))/length(symptomTimes),type=3)
exposureTime.hypothetical <- symptomTimes -  delta.star

plot(1:length(Y),Y,type="h")
other <- table(factor(exposureTime.hypothetical,levels=1:max(symptomTimes)))
lines(1:length(Y)+0.2,X,col=2,type="h")
lines(1:length(Y)+0.4,other,type="h",col="green")
}
