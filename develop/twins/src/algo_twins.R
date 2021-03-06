######################################################################
# Experimental version -- integrating the twins program into
# the surveillance package
######################################################################

algo.twins <- function(disProgObj, control=list(burnin=1000, filter=10, sampleSize=2500, alpha_xi=10, beta_xi=10, psiRWSigma=0.25, alpha_psi=1, beta_psi=0.1, logFile="twins.log")) {

  T <- as.integer(disProgObj$freq)
  nfreq <- as.integer(1)

  #Convert sts objects
  if (class(disProgObj) == "sts") disProgObj <- sts2disProg(disProgObj)

  #set default values (if not provided in control)
  if(is.null(control[["burnin",exact=TRUE]]))
    control$burnin <- 1000
  if(is.null(control[["filter",exact=TRUE]]))
    control$filter <- 10
  if(is.null(control[["sampleSize",exact=TRUE]]))
    control$sampleSize <- 2500
  if(is.null(control[["alpha_xi",exact=TRUE]]))
    control$alpha_xi <- 10
  if(is.null(control[["beta_xi",exact=TRUE]]))
    control$beta_xi <- 10
  if(is.null(control[["psiRWSigma",exact=TRUE]]))
    control$psiRWSigma <- 0.25
  if(is.null(control[["alpha_psi",exact=TRUE]]))
    control$alpha_psi <- 1
  if(is.null(control[["beta_psi",exact=TRUE]]))
    control$beta_psi <- 0.1
  if(is.null(control[["logFile",exact=TRUE]]))
    control$logFile <- "twins.log"

  control$logFile2 <- paste(control$logFile,"2",sep="")

  #Call the C code
  x <- disProgObj$observed
  n <- as.integer(dim(x)[1])
  I <- as.integer(dim(x)[2])

#  res <- with(control,
  with(control,
  res <- .C("twins",
            x=as.integer(x), n=n, I=I,
            logFile=logFile, logFile2=logFile2,
            burnin=as.integer(burnin), 
            filter=as.integer(filter),sampleSize=as.integer(sampleSize),
            alpha_xi=as.double(alpha_xi), beta_xi=as.double(beta_xi),
            T=T, nfreq=nfreq,
            psiRWSigma=as.double(0.25),
            alpha_psi=as.double(alpha_psi), beta_psi=as.double(beta_psi)))

  #Log files
  results <- read.table(control$logFile,header=T,na.strings=c("NaN","-NaN"))
  results2 <- read.table(control$logFile2,header=T,na.strings=c("NaN","-NaN"))
  acc <- read.table(paste(control$logFile,".acc",sep=""),col.names=c("name","RWSigma","acc"))
  rownames(acc) <- acc[,1]
  acc <- acc[,-1]
  
  #Nothing is returned by the function - result is not a
  #standard survObj
  result <- list(control=control,disProgObj=disProgObj,logFile=results,logFile2=results2)

  
  class(result) <- "atwins"

  return(result)
}




######################################################################
# Adapted the functions form figures.R
######################################################################

# Helper functions to make list of Z and the means of X,Y and omega
make.pois <- function(obj) {
  n <- nrow(obj$disProgObj$observed)
  m<-list()
  m$n <- n
  m$Z <- obj$disProgObj$observed
  m$X <- numeric(n)
  m$Y <- numeric(n)
  m$omega <- numeric(n)
  # Read means at each time instance
  Vars <- c("X","Y","omega") 
    for (t in 1:n) {
      for (v in Vars) {
        m[[v]][t] <- obj$logFile2[,paste(v,".",t,".",sep="")]
      }
    }
  return(m)
}

pois.plot <- function(m.results,xlab="") {
  plotorder <- c(expression(Z),expression(X),expression(Y))
  plotcols <- c(1,"blue","red")
  lwd <- c(1,3,3)
  ymax <- 5/4*max(m.results[[paste(plotorder[1])]])
  plot(0:(m.results$n-1),m.results[[paste(plotorder[1])]],type="s",col=plotcols[1],
       ylim=c(0,ymax),xlab=xlab,ylab="No. of cases",lwd=lwd[1])

  for (i in 2:length(plotorder)) {
    lines(1:(m.results$n-1),m.results[[paste(plotorder[i])]][2:m.results$n],type="s",col=plotcols[i],lwd=lwd[i])
  }
  legend(0,ymax,paste(plotorder),lwd=lwd,col=plotcols,horiz=T,y.intersp=0)
}

# makes list of gamma, zeta and nu
make.nu <- function(obj) {
  n <- nrow(obj$disProgObj$observed)
  samplesize <- obj$control$sampleSize
  frequencies <- 1
  season <- obj$disProgObj$freq
  
  m<-list()
  basefrequency<-2*pi/season
  for (j in 0:(2*frequencies)) {
        m$gamma[[j+1]] <- numeric(samplesize)
        m[["gamma"]][[j+1]] <- obj$logFile[,paste("gamma",".",j,".",sep="")]
  }
  m$zeta<-list()
  for (t in 1:n) {
    m$zeta[[t]]<-m$gamma[[1]]
    for(j in 1:frequencies){
      m$zeta[[t]] <- m$zeta[[t]] + m$gamma[[2*j]]*sin(basefrequency*j*(t-1)) + m$gamma[[2*j+1]]*cos(basefrequency*j*(t-1)) 
    }
  }
  m$nu<-list()
  for (t in 1:n) {
    m$nu[[t]]<-exp(m$zeta[[t]])
  }
  m$frequencies <- frequencies
  return(m)
}

#Function to plot median, and quantiles over time for m.par (m.par is list of n vectors, x is time)
tms.plot <-function(x,m.par,xlab="",ylab="",ylim=F,...){
  m<-list()
  n<-length(m.par)
  m$median<-numeric(n)
  for (t in 1:n) {
   m$median[t]<- median(m.par[[t]])
   m$q025[t]<- quantile(m.par[[t]],0.025)
   m$q975[t]<- quantile(m.par[[t]],0.975)
  }
  if(!ylim){
  ymin<-min(m$q025)
  ymax<-max(m$q975)
  ylim=c(ymin,ymax)
  }

  plot(x-1,m$q975[x],type="l",col="red",main="",xlab=xlab,ylab=ylab,ylim=ylim,...) 
  lines(x-1,m$median[x],type="l")
  lines(x-1,m$q025[x],type="l",col="red")
}


plot.atwins <- function(obj, which=c(1,4,6,7),ask=TRUE) {
  #Make list of X,Y,Z,omega means of results2
  m.results <-make.pois(obj)
  #Make list of results of  gamma, zeta and nu
  nu<-make.nu(obj)

  
  #Plots
  show <- rep(FALSE,7)
  show[which] <- TRUE
  par(ask=ask)
  
  if (show[1]) {
    par(mfcol=c(1,1))
    pois.plot(m.results,xlab="time")
  }

  if (show[2]) {
    par(mfcol=c(2,nu$frequencies+1))
    for(j in 0:2*nu$frequencies) {
      plot(nu$gamma[[j+1]],type="l",ylab=paste("gamma",j,sep=""))
    }
  }

  if (show[3]) {
    par(mfcol=c(1,1))
    plot(obj$logFile$K,type="l",ylab=expression(K))
    plot(obj$logFile$xilambda,type="l",ylab=expression(xi))
    plot(obj$logFile$psi,type="l",ylab=expression(psi))
  }

  if (show[4]) {
    par(mfcol=c(1,2))
    acf(obj$logFile$K,lag.max = 500,main="",xlab=expression(K))
    acf(obj$logFile$psi,lag.max = 500,main="",xlab=expression(psi))
  }

  if (show[5]) {
    par(mfcol=c(1,1))
    tms.plot(2:m.results$n,nu$nu,xlab="time")
  }

  if (show[6]) {
    par(mfcol=c(1,2))
    hist(obj$logFile$K,main="",xlab=expression(K),prob=T,breaks=seq(-0.5,max(obj$logFile$K)+0.5,1))
    hist(obj$logFile$psi,main="",xlab=expression(psi),prob=T,nclass=50)
  }

  if (show[7]) {
    par(mfcol=c(1,1))
    hist(obj$logFile$Znp1,main="",xlab=expression(Z[n+1]),prob=T,breaks=seq(-0.5,max(obj$logFile$Znp1)+0.5,1))
  }
}

