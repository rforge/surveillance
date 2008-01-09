library(surveillance)
library(mgcv)

data(shadar)

train <- 1:(4*52) #1:(3*52)
test <-  (4*52+1):length(shadar$week) #  <- (3*52+1):length(x)
shadar.data <- data.frame(x=as.vector(shadar$observed)[1:length(train)],t=train)

#Fit the model
m.hadar.nb.spline <- gam( x ~ 1 + t + s(t %% 52,bs="cc",fx=FALSE), family=negative.binomial(theta=1/0.2), data=shadar.data)
m.hadar <- m.hadar.nb.spline
alpha.hat <- 1/m.hadar$family$Theta



#Compute GLR statistic for every possible observation
one <- function (mu0,t,control) {
  if (length(mu0) != length(t)) stop("Error: Length of x and y are different!")
  
  #Function arguments
  control$c.ARL <- 1e99
  dir <- 1
  ret <- 1
  i <<- i+1
  if (i %% 10 == 0) cat(i,"\n")
  
  #Simulate data
  x <- rpois(length(t), lambda = mu0)
  
  res <- .C("glr_nb_window",x=as.integer(x),mu0=as.double(mu0),alpha=as.double(control$alpha),lx=length(x),Mtilde=as.integer(control$Mtilde),M=as.integer(control$M),c.ARL=as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),dir=as.integer(dir),PACKAGE="surveillance")
  
  return(res$val)
}

#mu model
mu0.hadar <- predict(m.hadar, newdata=data.frame(t=test),type="response")
#control param
cntrl = list(range=test,mu0=mu0.hadar, alpha=alpha.hat, c.ARL=1e99, Mtilde=1, M=-1,change="intercept")

i <- 0
#This might take a while!
glr.val <- replicate(10000,one(mu0=mu0.hadar,t=test,control=cntrl))
monitor <- algo.glrnb(shadar, cntrl)
p <- c(seq(0,0.99,length=100),0.999,0.9999,0.99999, 1)
qs <- apply(glr.val, MARGIN=1, quantile, p=p)

matplot(test,t(qs),type="n",xlab="time",ylab="GLR(n)")
ramp <- colorRamp(c("green", "yellow", "red"),bias=0.01)#, bias=0.1)
col <-  rgb( ramp(p), max=255)

for (i in 2:nrow(qs)) {
  polygon(c(test,rev(test)),c(qs[i-1,],rev(qs[i,])),col=col[i-1],border=FALSE)
}
matlines(test,t(apply(glr.val, MARGIN=1, quantile, p=c(0.9,0.99,0.999))),type="l",xlab="time",col=1)
lines(test, monitor$upperbound,lwd=3)
legend(x="topleft",paste("<",sprintf("%.2f",c(0.9,0.99,0.999)*100),"%",sep=""),col=1,horiz=TRUE,lty=1:3,bg="white")

######################################################################
#P value plot
######################################################################
a <- matrix(monitor$upperbound, nrow=nrow(glr.val), ncol=ncol(glr.val),byrow=FALSE)
pval <- apply(glr.val > a, MARGIN=1, mean)
matplot(test,t(qs),type="n",xlab="time",ylab="p-value",ylim=c(0,1))
lines(test, pval)


