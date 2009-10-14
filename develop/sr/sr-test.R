######################################################################
# Shiryaev-Roberts based spatio-temporal cluster detection
# Based on the work in Correa & Assuncao (2009)
#
# Parameters:
#   x - vector containing spatial x coordinate of the events
#   y - vector containing spatial y coordinate of the events
#   t - vector containing the time points of the events
#   radius - is the radius of the cluster 
#   epsilon - is the relative change of event-intensity within the cluster
#       to detect
#   A - threshold limit for the alarm and should be equal to the desired ARL
######################################################################
sr<- function(x, y,t,radius,epsilon,threshold) {
  #Sort data by time
  n<-length (x)

  desordem <- cbind(x,y,t)
  ordena <-sort(desordem[ ,3], partial = NULL, na.last = NA, decreasing = FALSE,index.return=TRUE)
  ordem <- ordena$ix
  Dados <- matrix (0,n,3)


  for (i in 1:n)  {
    Dados[i, ]<- desordem[ordem[i], ]
  }

  x <- Dados[,1]
  y <- Dados[,2]
  t <- Dados[,3]

  #Startup
  epsilon1 <- epsilon+1
  R <- rep(0, n)    #R[n] is the sum of R(i), i=1..n
  RMax <- rep(0, n)
  RMaxInd <- rep(0, n)
  ParcelaR <- matrix(0, n, n)

  #Spatial distances matrix 
  Dados_espaAo <- matrix (0,n,2)
  Dados_espaAo[ ,1]<- x
  Dados_espaAo[ ,2]<- y
  mdist2 <- dist(Dados_espaAo, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  len <- length (mdist2)

  #1 for events that are close in space and 0 for events that aren't close in space  
  for (i in 1:len)
  {
   if (mdist2[i]<=radius) mdist2[i] <- 1 
     else mdist2[i] <- 0
  }

  mdist <- as.matrix(mdist2)
  for (i in 1:n)
  {
   mdist[i,i] <- 1 
  }


  #Calculation of test statistic
  T1 <- matrix(0,n,n)
  T2 <- matrix(0,n,n)
  for (i1 in 1:n)   #i1 is n
  {
    for (i2 in 1:i1)  #i2 is Tau=begining of the cluster
    {
      nC <- (sum(mdist[i2,i2:i1]))
      T1 <- epsilon1^nC
      MiC <- (sum(mdist[i2,1:i1]) * (i1-i2+1))/i1
      T2 <- exp((-epsilon)*MiC)
      ParcelaR[i2,i1] <- T1 * T2
      R[i1] <- R[i1] + ParcelaR[i2,i1]
      if (ParcelaR[i2,i1] > RMax[i1])
      {
        RMax[i1] <- ParcelaR[i2,i1]
        RMaxInd[i1] <- i2
      }
    }
  }

  alarme <- -1
  min <- n
  for (i in 1:n)  
    {
      if (R[i] > threshold) 
        {
          if (i <= min) 
            {
              alarme <- i
              min <- i
            }
        }
    }

  #Where and when did the cluster begin?
  RMax<-0
  Lambda<-c(0,alarme)

  for (i2 in 1:alarme)   #i2 È Tau
  {
      nC <- (sum(mdist[i2,i2:alarme]))
      T11 <- epsilon1^nC
      MiC <- (sum(mdist[i2,1:alarme]) * (alarme-i2+1))/alarme
      T21 <- exp((-epsilon)*MiC)
      Lambda[i2] <- T11 * T21   

      if (Lambda[i2] > RMax)
      {
        RMax <- Lambda[i2]
        inicio <- i2
       }
 }

  #Whicht events belong to the cluster?
  distcluster<-mdist[inicio:alarme, inicio]
  as.vector(distcluster)

  b<-alarme-inicio+1
  vet<-matrix(0,b,2)
  vet[ ,1]<-seq(inicio,alarme,1)
  vet[ ,2]<-distcluster

  num<-sum(vet[ ,2])
  clus<-c(rep(0,num))
  j<-1


  for (i in 1:b){
    if(vet[i,2]==1){
      clus[j]<-vet[i,1] 
       j<-j+1
     }  
  }

  c <- length (clus)
  clus.pos<-matrix(0,c,3)

  for (i in 1:c)
    {
      clus.pos[i, ] <- c(x[clus[i]],y[clus[i]],t[clus[i]])
    }


  #output
  output <- list(alarme.event=alarme,alarm.x=x[alarme],alarm.y=y[alarme],
                 alarm.t=t[alarme],cluster.event=inicio,cluster.x=x[inicio],
                 cluster.y=y[inicio],cluster.t=t[inicio],
                 withincluster.event=clus,withincluster.xyt=clus.pos)
}

sr.fast<- function(x, y,t,radius,epsilon,threshold) {
  #check that x,y,t are of the same length.
  n <- length(x)
  if ((length(y) != n) | (length(t) != n)) {
    stop("Vectors x,y,t not of same size.")
  }

  res <- .C("SRspacetime", x=as.double(x), y=as.double(y), t=as.double(t), n=as.integer(n), radius=as.double(radius), epsilon=as.double(epsilon), threshold=as.double(threshold),R=as.double(numeric(n)),idxFA=as.integer(-1),idxCC=as.integer(-1))

  #Missing: compute which indices are part of the cluster.
  #Thais
  
  return(list(R=res$R,idxFA=res$idxFA+1,idxCC=res$idxCC+1))
}

testIt <- function() {
  source("sr-test.R")
  #Read data
  library("splancs")
  data(burkitt)

  #Parameters
  #x <- burkitt$x; y <- burkitt$y; t <- burkitt$t
  epsilon <- 0.5 # relative change within the cluster
  radius <- 20 # radius
  threshold <- 161 # threshold limit
#  threshold <- 2000 # threshold limit

  #Load the library
  dyn.load("sr-spacetime.so")
  #Call sr.fast
  res <- sr.fast(burkitt$x,burkitt$y,burkitt$t,radius,epsilon,threshold)

  #Index of the event
  which.max(res$R >= threshold)

  
  res <- sr(burkitt$x,burkitt$y,burkitt$t,radius,epsilon,threshold)


 }
