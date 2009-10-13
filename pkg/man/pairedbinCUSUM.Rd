\name{pairedbinCUSUM}
\alias{pairedbinCUSUM}
\alias{pairedbinCUSUM.runlength}
\alias{pairedbinCUSUM.workhorse}
\encoding{latin1}

\title{Paired binary CUSUM and its run-length computation}
\description{
  CUSUM for paired binary data.
}
\usage{
pairedbinCUSUM(stsObj, control = list(range=NULL,theta0,theta1,h1,h2,h11,h22))
pairedbinCUSUM.runlength(p,w1,w2,h1,h2,h11,h22, sparse=FALSE)
}
\arguments{
  \item{stsObj}{Object of class \code{sts} containing the paired
    responses for each of the, say n, patients. The observed slot is
    thus a \eqn{n \times 2} matrix.}
  \item{control}{}
  \item{theta0}{}
  \item{theta1}{}
  \item{h1}{}
  \item{h2}{}
  \item{h11}{}
  \item{h22}{}
}
\details{
}
%\seealso{\code{\link{pairedbinCUSUM}}}
\value{An \code{sts} object with \code{observed}, \code{alarm},
  etc. slots trimmed to the \code{control$range} indices.
}
\references{
  Steiner, S. H., Cook, R. J., and Farewell, V. T. (1999), Monitoring
  paired binary surgical outcomes using cumulative sum charts,
  Statistics in Medicine, 18, pp. 69--86.
}
\examples{
#Set in-control and out-of-control parameters as in paper
theta0 <- c(-2.3,-4.5,2.5)
theta1 <- c(-1.7,-2.9,2.5)

#Small helper function to compute the paired-binary likelihood
#of the length two vector yz when the true parameters
#are theta.
dPBin <- function(yz,theta) {
    exp(dbinom(yz[1],size=1,prob=plogis(theta[1]),log=TRUE) + dbinom(yz[2],size=1,prob=plogis(theta[2]+theta[3]*yz[1]),log=TRUE))
}

#Likelihood ratio for all four possible configurations
p <- c(dPBin(c(0,0), theta=theta0), dPBin(c(0,1), theta=theta0),
       dPBin(c(1,0), theta=theta0), dPBin(c(1,1), theta=theta0))

#Compute ARL using non-sparse matrix operations
pairedbinCUSUM.runlength(p,c(-1,37,-9,29),c(-1,7),h1=70,h2=32,h11=38,h22=17)

#Sparse computations don't work on all machines (e.g. the next line
#might lead to an error. If it works this call is considerably! faster
#than the non-sparse call.
\dontrun{
pairedbinCUSUM.runlength(p,c(-1,37,-9,29),c(-1,7),h1=70,h2=32,h11=38,h22=17,sparse=TRUE)
}

#Use paired binary CUSUM
data("deleval")

S <- pairedbinCUSUM(x=observed(deleval)[,c(2,1)],theta0=theta0,
    theta1=theta1,h1=70,h2=32,h11=38,h22=17)

######################################################################
#Log LR for conditional specification of the paired model
######################################################################
LLR.pairedbin <- function(yz,theta0, theta1) {
    #In control
    alphay0 <- theta0[1] ; alphaz0 <- theta0[2] ; beta0 <- theta0[3]
    #Out of control
    alphay1 <- theta1[1] ; alphaz1 <- theta1[2] ; beta1 <- theta1[3]
    #Likelihood ratios        
    llr0 <- dbinom(yz[1],size=1,prob=plogis(alphay0),log=TRUE) + dbinom(yz[2],size=1,prob=plogis(alphaz0+beta0*yz[1]),log=TRUE)
    llr1 <- dbinom(yz[1],size=1,prob=plogis(alphay1),log=TRUE) + dbinom(yz[2],size=1,prob=plogis(alphaz1+beta1*yz[1]),log=TRUE)
    llry <- (alphay1-alphay0)*yz[1]+log(1+exp(alphay0))-log(1+exp(alphay1))
    llrz <- (alphaz1-alphaz0)*yz[2]+log(1+exp(alphaz0+beta0*yz[1]))-log(1+exp(alphaz1+beta1*yz[1]))
    return(c(llry,llrz,llr1-llr0))
}


#Scale steiner charts
val <- expand.grid(0:1,0:1)
table3 <- t(apply(val,1, LLR.pairedbin, theta0=theta0, theta1=theta1))
w1 <- min(abs(table3[,1]))
w2 <- min(abs(table3[,2]))
S2 <- S / cbind(rep(w1,nrow(S)),w2)

#Show results
par(mfcol=c(2,1))
plot(0:nrow(deleval),S2[,1],type="l",main="Near Miss",xlab="Patient #",ylab="CUSUM")
lines(c(0,1e99), c(32,32),lty=2,col=2)
lines(c(0,1e99), c(17,17),lty=2,col=3)
    
plot(0:nrow(deleval),S2[,2],type="l",main="Death",xlab="Patient #",ylab="CUSUM")
    lines(c(0,1e99), c(70,70),lty=2,col=2)
    lines(c(0,1e99), c(38,38),lty=2,col=3)

}

\author{S. Steiner and M. \enc{Höhle}{Hoehle}}
\keyword{regression}

%deleval <- read.table("~/MCHealth/Varizellen/R/leval.txt",header=TRUE)
%deleval <- new("sts", epoch=1:nrow(deleval), observed=deleval,state=0*deleval,freq=104)
%save(file="~/Surveillance/surveillance/pkg/data/deleval.RData",list=c("deleval"))