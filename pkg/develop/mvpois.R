theta <- c(10,12,5)

######################################################################
# Draw from the multivariate poisson distribution
#
# Params:
#  n     - size of the sample
#  theta - vector of parameters
######################################################################
rmvpois <- function(n,theta) {
  #generate the variables
  lambda <- matrix(rep(theta,each=n),length(theta),n,byrow=TRUE)
  x <- matrix(rpois(n*length(theta),lambda),length(theta),n)

  #do the additions
  val <- matrix(x[1,],nrow=(length(theta)-1),n,byrow=TRUE)  + x[2:3,]
  return(val)
}


#Try
theta <- c(100,12,5)
x <- rmvpois(1e5,theta=theta)

c(theta[1]+theta[2],theta[1]+theta[3])
apply(x,MARGIN=1,mean)

theta[1]
cov(x[1,],x[2,])

theta[1]/sqrt(prod(c(theta[1]+theta[2],theta[1]+theta[3])))
cor(x[1,],x[2,])

#Use surveillance
library(surveillance)

n <- 100
x <- new("sts",week=100,start=c(2001,1),observed=t(rmvpois(n,theta=theta)),
         state=matrix(0,n,length(theta)-1))

plot(x,xaxis.years=FALSE)
plot(x,xaxis.years=FALSE,type = observed ~ time)


##My own experiments


W <- matrix(NA,4,4)
W[1,] <- c(0,1,1,0)
W[2,] <- c(1,0,1,1)
W[3,] <- c(1,1,0,1)
W[4,] <- c(0,1,1,0)


#Sample n instances of the multivariate poisson distribution
rmvpois <- function(n,theta,A) {
  one <- function() {
    y <- matrix(rpois(length(theta),theta),ncol=1);
    x <- A %*% y
  }
                   
  return(replicate(n,one()))
}

lambda <- 1
phi    <- 0.5
A <- lambda*diag(rep(1,4)) + phi*W

x <- rmvpois(1e5,theta=rep(10,4),A=A)

cov(t(x))

A %*% diag(theta) %*% t(A)

cov2cor(A %*% diag(theta) %*% t(A))

