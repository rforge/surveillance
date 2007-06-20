#R stuff accompanying the Statistics in Medicine paper (2004)

#The independent scenario
p <- 0.00319
mu <- p*1e4
var <- 1e4*p*(1-p)
Lambda <- diag( rep(var,10))
#Lambda <- diag( rep(1,10))

rho <- 0#0.2
x <- matrix(1,nrow=10,ncol=1)
alpha <- rho * Lambda[1,1]
#diag(rep(1-alpha),10) + alpha * x %*% t(x)
Kappa <- diag(rep(0-alpha),10) + alpha * x %*% t(x)


##Compute correlation from covariance matrix
cov2cor(Lambda)
cov2cor(Kappa+Lambda)


#Action
#Sigma <- Lambda
Sigma <- Kappa + Lambda

mu1 <- matrix(c(rep(sqrt(Sigma[1,1]),1),rep(0,9)),10,1)
mu2 <- matrix(c(rep(sqrt(Sigma[1,1]),3),rep(0,7)),10,1)
mu3 <- matrix(c(sqrt(1/3)*rep(sqrt(Sigma[1,1]),3),rep(0,7)),10,1)

mvdist <- function(mu) {
  sqrt(t(mu) %*% solve(Sigma) %*% mu)
}

mvdist(mu1)/2
mvdist(mu2)/2
mvdist(mu3)/2


#SAR Model
source("n4.R")
dimx <- 4
dimy <- 3
#W <- matrix(NA,dimx,dimy)
#for (x in 1:4) {
#  for (y in 1:3) {
#    W[x,y] <- pix2i(matrix(c(x,y),1,2))
#  }
#}
#W

W <- matrix(0,dimx*dimy,dimx*dimy)
for (i in 1:12) {
  if ((i != 8) & (i!= 12)) {
    pix <- i2pix(i)
    N4 <- pix2i(n4(pix[,1,drop=FALSE],pix[,2,drop=FALSE]))
    N4 <- N4[N4 != 8 & N4 != 12]
    print(N4)
    for (j in N4) W[i,j] <- 1
  }
}

#Thin W, now equal to
# 1 5 8
# 2 6 9
# 3 7 10
# 4
W <- W[-c(8,12),-c(8,12)]

#According to Shabenberger & Gotway p.336: (I-\rho W) has to be non-singular
#means 1/vmin < rho < 1/vmax, where vmin, vmax har the smallest and largest
#eigenvalue of W
range(eigen(W)$values)
1/range(eigen(W)$values)

#standardize W such that row sums equal 1
V <- W / apply(W,MARGIN=1,sum)
apply(V,MARGIN=1,sum)
1/range(eigen(V)$values)

W <- V
rho <- 0.7
I <- diag(rep(1,nrow((W))))
Sigma.e <- diag(rep(1,nrow(W)))
Sigma <- solve(t(I-rho*W) %*% solve(Sigma.e) %*% (I-rho*W))

#mu1 <- matrix(c(rep(sqrt(Sigma[1,1]),1),rep(0,nrow(W)-1)),nrow(W),1)
#mu2 <- matrix(c(rep(sqrt(Sigma[1,1]),3),rep(0,nrow(W)-3)),nrow(W),1)
#mu3 <- matrix(c(sqrt(1/3)*rep(sqrt(Sigma[1,1]),3),rep(0,nrow(W)-3)),nrow(W),1)

mu1 <- matrix(c(sqrt(Sigma[1,1]),rep(0,nrow(W)-1)),nrow(W),1)
mu2 <- matrix(c(sqrt(diag(Sigma[1:3,1:3])),rep(0,nrow(W)-3)),nrow(W),1)
mu3 <- matrix(c(sqrt(1/3)*sqrt(diag(Sigma[1:3,1:3])),rep(0,nrow(W)-3)),nrow(W),1)

mvdist <- function(mu) {
  sqrt(t(mu) %*% solve(Sigma) %*% mu)
}

mvdist(mu1)/2
mvdist(mu2)/2
mvdist(mu3)/2


#######################################################################
# Poisson simultaneous experiments
######################################################################

rho <- 0.1
theta <- c(10,20)
lambda <- c(theta[1] + rho*theta[2],theta[2] + rho*theta[1])

n <- 1e6
x <- matrix(rpois(2*n,lambda),ncol=2,byrow=TRUE)

lambda
apply(x,MARGIN=2,mean)

mean(x[,1] * x[,2])

prod(lambda)

#almost zero!
cov(x[,1],x[,2])


#Different
theta <- c(10,12,13)
