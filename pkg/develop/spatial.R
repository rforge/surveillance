library(surveillance)
data(measels.weser)
hep <- as.matrix(read.table("hepatitisAberlin.txt",header=TRUE,skip=1))
hepMale <- as.matrix(read.table("hepAmale.txt",header=TRUE,skip=1))
hepFemale <- as.matrix(read.table("hepAfemale.txt",header=TRUE,skip=1))

#Remove week, nicht ermittelbar und nicht zuordnbar
#obj$observed <- hep[,-c(1,2,3)]
#obj$observed <- hepMale[,-c(1,2,3)] + hepFemale[,-c(1,2,3)]
#obj$observed <- hep[,-c(1,2,3)] - hepMale[,-c(1,2,3)] - hepFemale[,-c(1,2,3)]
#obj$state <-  matrix(0,dim(hep)[1],dim(hep)[2])

#Create disProg object
obj <- create.disProg(week=hepMale[,1],observed=hepMale[,-c(1,2,3)],
                      state=matrix(0,dim(hepMale)[1],dim(hepMale)[2]-3))

#Define time labels
noWeeks <- 53
y2001 <- 1:noWeeks
y2002 <- (1*noWeeks+1):(2*noWeeks)
y2003 <- (2*noWeeks+1):(3*noWeeks)
y2004 <- (3*noWeeks+1):(4*noWeeks)
y2005 <- (4*noWeeks+1):(5*noWeeks)
y2006 <- (5*noWeeks+1):dim(obj$observed)[1]


year <- c(as.numeric(sapply(2001:2005,function(y) rep(y,53))),rep(2006,30))
by(obj$observed,year,FUN=sum)
x <- aggregateDisProg(obj)
plot(x)
obj1 <- x
plot(obj,as.one=FALSE)

plot(algo.rki(x,control=list(range=c(y2005,y2006),alpha=0.1)))

#plot(algo.bayes(x,control=list(range=c(y2005,y2006),b=3,w=3,alpha=0.01)))
ab <- algo.bayes(obj,control=list(range=c(y2005,y2006),b=3,w=3,alpha=0.001))

plot(ab)


y <- algo.cusum(x,control=list(range=c(y2005,y2006),k=1,h=3,trans="standard"))
y <- algo.cusum(x,control=list(range=c(y2005,y2006),k=1,h=3,trans="rossi"))
y <- algo.cusum(x,control=list(range=c(y2005,y2006),k=1,h=3,trans="none"))
plot(y)
lines(c(1,length(c(y2005,y2006))),rep(y$control$m,2))


plot(algo.cusum(x,control=list(range=c(y2002,y2003,y2004,y2005,y2006),k=1,h=2)))
plot(algo.farrington(x,control=list(range=c(y2005,y2006),b=3,w=3)))

