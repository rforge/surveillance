setwd("P:\\Daten\\Surveillance\\pkg\\R")
source("../R/farringtonFlexible.r")
library("RUnit")
library(testthat)
library('surveillance')
data("salmonella.agona")
# sts object
lala <- paste(salmonella.agona$start[1],salmonella.agona$start[2],"1",sep=" ")
firstMonday <- as.POSIXlt(lala, format = "%Y %W %u")
salm.ts <- salmonella.agona$observed
dates <- as.Date(firstMonday) + 7 * 0:(length(salm.ts) - 1)
start=c(salmonella.agona$start[1],salmonella.agona$start[2])
salm <- new("sts",epoch = as.numeric(dates), start = start, freq = 52,
observed = salm.ts, epochAsDate = TRUE)

################################################################################
# MONTHLY DATA
################################################################################
#time series 
# timeseries <- c(493L, 516L, 561L, 522L, 481L, 718L, 461L, 480L, 551L, 497L, 
                   # 493L, 457L, 393L, 476L, 582L, 438L, 471L, 501L, 418L, 487L, 445L, 
                   # 441L, 476L, 326L, 372L, 417L, 543L, 411L, 506L, 459L, 435L, 517L, 
                   # 427L, 387L, 494L, 335L, 430L, 441L, 365L, 380L, 462L, 355L, 445L, 
                   # 450L, 408L, 445L, 433L, 437L, 448L, 453L, 478L, 388L, 373L, 440L, 
                   # 456L, 382L, 362L, 448L, 395L, 392L, 397L, 414L, 465L, 404L, 393L, 
                   # 445L, 494L, 447L, 408L, 417L, 480L, 382L, 409L, 4L)
  # freq <- 12
  # firstDayTimeseries <- "2014-02-20"
  # dateRanges <- as.Date(seq(as.Date(firstDayTimeseries, 
										# origin="1970-01-01"),
										# length=length(timeseries), by="-1 month"))
  # control <- list(
    # noPeriods=10,populationBool=FALSE,
    # fitFun="algo.farrington.fitGLM.flexible",
    # b=4,w=3,weightsThreshold=2.58,
    # pastWeeksNotIncluded=26,
    # pThresholdTrend=1,trend=TRUE,verbose = FALSE,
    # thresholdMethod="nbPlugin",alpha=0.01
  # )
  

  
  #create the sts object
  # seriesSTSObject <- new("sts",epoch = as.numeric(dateRanges), 
                         # freq = freq,observed = timeseries, 
                         # epochAsDate = TRUE)
  
 # control parameters
  # algoControl <- control


# expect_that(farringtonFlexible(seriesSTSObject, control = algoControl), gives_warning("Monthly data are to be"))





################################################################################
# END OF MONTHLY DATA TESTS
################################################################################
################################################################################
# WEIGHTS FUNCTION
################################################################################

# gamma = 1 if everything below the threshold
s <- rep(0,10)
weightsThreshold <- 0
weights <- algo.farrington.assign.weights(s,weightsThreshold)
checkEquals(weights,rep(1,10))

# A case that was checked by hand
s <- rep(2,10)
s[1:5] <- 0
weightsThreshold <- 0
weights <- algo.farrington.assign.weights(s,weightsThreshold)
checkEquals(weights[1:5],rep(1.6,5))
checkEquals(weights[6:10],rep(0.4,5))

################################################################################
# END OF WEIGHTS FUNCTION TESTS
################################################################################

################################################################################
# RESIDUALS FUNCTION
################################################################################

# residuals should be zero
x <- rpois(10,1)
y <- exp(x)
model <- glm(y~x,family = quasipoisson(link="log"))
phi <- max(summary(model)$dispersion,1)
s <- anscombe.residuals(model,phi)
checkEquals(as.numeric(s),rep(0,10))

# residuals should not be zero
x <- rpois(1000,1)
y <- exp(x)+runif(1)
model <- glm(y~x,family = quasipoisson(link="log"))
phi <- max(summary(model)$dispersion,1)
s <- anscombe.residuals(model,phi)
checkTrue(mean(s)>0)

################################################################################
# END OF RESIDUALS FUNCTION TESTS
################################################################################

################################################################################
# FORMULA FUNCTION
################################################################################

checkEquals(formulaGLM(populationOffset=FALSE,timeBool=TRUE,factorsBool=FALSE),"response ~ 1+wtime")
checkEquals(formulaGLM(populationOffset=FALSE,timeBool=FALSE,factorsBool=FALSE),"response ~ 1")
checkEquals(formulaGLM(populationOffset=TRUE,timeBool=TRUE,factorsBool=FALSE),"response ~ 1+wtime+offset(log(population))")
checkEquals(formulaGLM(populationOffset=TRUE,timeBool=TRUE,factorsBool=TRUE),"response ~ 1+wtime+offset(log(population))+seasgroups")
################################################################################
# END OF FORMULA FUNCTION TESTS
################################################################################

################################################################################
# REFERENCE TIME POINTS FUNCTION
################################################################################

# Case with weekly data with dates
dayToConsider <- as.Date("2013-06-06")
b <- 3
freq <- 52
epochAsDate <- TRUE
epochStr <- "week"
lala <- algo.farrington.referencetimepoints(dayToConsider,b=b,freq=freq,epochAsDate,epochStr)
# Do we get the same day as dayToConsider?
checkEquals(as.numeric(format(lala, "%w")),rep(4,4))
# Actually for this example I know the dates one should get
checkEquals(sort(lala),sort(c(as.Date("2010-06-03"),as.Date("2013-06-06"),as.Date("2012-06-07"),as.Date("2011-06-09"))))

# Case with monthly data
dayToConsider <- 48
b <- 3
freq <- 12
epochAsDate <- FALSE
epochStr <- "month"
lala <- algo.farrington.referencetimepoints(dayToConsider,b=b,freq=freq,epochAsDate,epochStr)
checkEquals(lala,c(48,36,24,12))
expect_that(algo.farrington.referencetimepoints(dayToConsider,b=8,freq=freq,epochAsDate,epochStr), gives_warning("Some reference"))

# Check that one gets a warning with epochAsDate==TRUE if too many years back

# apply code
 control1 <-  list(range=250,
                                     noPeriods=10,populationOffset=FALSE,
                                     fitFun="algo.farrington.fitGLM.flexible",
                                     b=40,w=3,weightsThreshold=2.58,
                                     pastWeeksNotIncluded=26,
                                     pThresholdTrend=1,trend=TRUE,
                                     thresholdMethod="muan",alpha=0.05,glmWarnings=FALSE)
expect_that(farringtonFlexible(salm,control=control1), gives_warning("Some reference"))

################################################################################
# END OF REFERENCE TIME POINTS FUNCTION TESTS
################################################################################

################################################################################
# FIT GLM FUNCTION TESTS
################################################################################

# Case with convergence
control<-  list(range=250,noPeriods=10,populationOffset=TRUE,
                fitFun="algo.farrington.fitGLM.flexible",
                b=40,w=3,weightsThreshold=2.58,
                pastWeeksNotIncluded=26,
                pThresholdTrend=1,trend=TRUE,
                thresholdMethod="muan",alpha=0.05,glmWarnings=FALSE)
 response=salm@observed[1:120]     
dataGLM <- data.frame(response=response,wtime=1:120,
	                  population=runif(120)*100,
                      seasgroups=as.factor(rep(1:12,10)))
     
arguments <- list(dataGLM=dataGLM,
                   timeTrend=TRUE,
                   populationOffset=TRUE,
                   factorsBool=TRUE,reweight=TRUE,
                   weightsThreshold=0.5,glmWarnings=control$glmWarnings,
				   control=control)
model <- do.call(algo.farrington.fitGLM.flexible, args=arguments)

# Right class of output?
checkEquals(class(model),c("glm","lm"))

# As many coefficients as expected?
checkEquals(dim(summary(model)$coefficients)[1],length(levels(dataGLM$seasgroups))-1+1+1)


# Were wtime, response, phi and weights added to the model?
checkTrue(is.null(model$phi)==FALSE)
checkTrue(is.null(model$wtime)==FALSE)
checkTrue(is.null(model$response)==FALSE)
checkTrue(is.null(model$population)==FALSE)
checkTrue(is.null(model$weights)==FALSE)

# Was reweighting done?
checkTrue(sum(model$weights!=rep(1,length(model$weights)))==length(model$weights))

# No weights if very high threshold?
arguments$reweight <- TRUE
arguments$weightsThreshold <- 100000
model <- do.call(algo.farrington.fitGLM.flexible, args=arguments)
checkTrue(sum(model$weights==rep(1,length(model$weights)))==length(model$weights))

# Not a too small overdispersion?
checkTrue(model$phi>=1)

# arguments$timeTrend <- FALSE
# model <- do.call(algo.farrington.fitGLM.flexible, args=arguments)
################################################################################
# END OF FIT GLM FUNCTION TESTS
################################################################################

################################################################################
# BLOCKS FUNCTION TESTS
################################################################################

referenceTimePoints <- c(as.Date("2010-06-03"),as.Date("2013-06-06"),as.Date("2012-06-07"),as.Date("2011-06-09"))
firstDay <- as.Date("1990-06-07")
vectorOfDates <- dates <- as.Date(firstDay) + 7 * 0:1300
freq <- 52
dayToConsider <- as.Date("2013-06-06")
b <- 3
w <- 3
epochAsDate <- TRUE

# p=1
p <- 1
lala <- blocks(referenceTimePoints,vectorOfDates,freq,dayToConsider,b,w,p,
epochAsDate)
# reference windows
checkEquals(length(vectorOfDates[is.na(lala)==FALSE&lala==p]),w+1+b*(2*w+1))

# p>1
p <- 8
lala <- blocks(referenceTimePoints,vectorOfDates,freq,dayToConsider,b,w,p,
epochAsDate)
# reference windows
checkEquals(length(vectorOfDates[is.na(lala)==FALSE&lala==p]),w+1+b*(2*w+1))

lili <- as.factor(lala[is.na(lala)==FALSE])
# as many levels as expected?
checkEquals(length(levels(lili)),p)
lolo <- lili[lili!=p]

# periods of roughly the same length each year?
checkEquals(as.numeric(abs(diff(table(lolo))[1:(p-2)])<=b),rep(1,(p-2)))

################################################################################
# END OF BLOCKS FUNCTION TESTS
################################################################################

################################################################################
# THRESHOLD FUNCTION FARRINGTON TESTS
################################################################################

predFit <- 5
predSeFit <- 0.2
wtime <- 380
skewness.transform <- "2/88"
alpha <- 0.05
y <- 8
method <- "delta"
phi <- 1

expect_that(algo.farrington.threshold.farrington(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method),throws_error("proper exponent"))

skewness.transform <- "none"
lala <- algo.farrington.threshold.farrington(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)												  
# Should always be ok
lala <- as.numeric(lala)
checkTrue(lala[3]<=1&lala[1]>=0)	
checkTrue(lala[2]>lala[1])
checkTrue(lala[1]>=0)			

# Here we know the results								  
checkEquals(abs(as.numeric(lala)-c(1.3073128, 8.6926872, 0.0907246, 0.8124165))<rep(1e-7,4),rep(TRUE,4))

skewness.transform <- "1/2"
lala <- algo.farrington.threshold.farrington(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)												  
											  
checkEquals(abs(as.numeric(lala)-c( 1.9891097, 9.3744842, 0.0000000, 0.6857951))<rep(1e-7,4),rep(TRUE,4))

skewness.transform <- "2/3"
lala <- algo.farrington.threshold.farrington(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)												  
											  
checkEquals(abs(as.numeric(lala)-c( 1.808448e+00,  9.115482e+00, 1.596176e-112,  7.289546e-01))<rep(1e-6,4),rep(TRUE,4))


################################################################################
# END OF THRESHOLD FUNCTION FARRINGTON TESTS
################################################################################


################################################################################
# THRESHOLD FUNCTION NOUFAILY TESTS
################################################################################
											  
predFit <- log(5)
predSeFit <- log(2)
wtime <- 380
skewness.transform <- "none"
alpha <- 0.05
y <- 11
phi <- 1.5
method <- "muan"
lala <- algo.farrington.threshold.noufaily(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)
# Should always be ok
lala <- as.numeric(lala)
checkTrue(lala[3]<=1&lala[1]>=0)	
checkTrue(lala[2]>lala[1])
checkTrue(lala[1]>=0)	

# Here we calculated some examples
checkEquals(abs(as.numeric(lala)-c(7.0000000, 26.0000000,  0.8597797,  0.3850080))<rep(1e-6,4),rep(TRUE,4))
phi <- 1.0
method <- "muan"
lala <- algo.farrington.threshold.noufaily(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)
checkEquals(abs(as.numeric(lala)-c(8.0000000, 24.0000000,  0.9093099 , 0.4193982))<rep(1e-6,4),rep(TRUE,4))

phi <- 1.5
method <- "nbPlugin"
lala <- algo.farrington.threshold.noufaily(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)
checkEquals(abs(as.numeric(lala)-c(1.00000000, 11.00000000,  0.03763657,  1.00000000))<rep(1e-6,4),rep(TRUE,4))
												  
phi <- 1.0
method <- "nbPlugin"
lala <- algo.farrington.threshold.noufaily(predFit,predSeFit,phi,
                                                 skewness.transform,
												  alpha,y,method)										
checkEquals(abs(as.numeric(lala)-c( 1.00000000, 10.00000000,  0.01369527,  1.11918153))<rep(1e-6,4),rep(TRUE,4))

################################################################################
# END OF THRESHOLD FUNCTION NOUFAILY TESTS
################################################################################

################################################################################
# DATA GLM FUNCTION TESTS
################################################################################
b <- 3
freq <- 52
dayToConsider <- as.Date("2013-05-30")
epochAsDate <- TRUE
epochStr <- "week"
firstDay <- as.Date("1990-06-07")
vectorOfDates <- dates <- as.Date(firstDay) + 7 * 0:1300
w <- 3
noPeriods <- 10
observed <- rnorm(1301)+runif(1301)+30
population <- rnorm(1301)+10
verbose <- FALSE
pastWeeksNotIncluded <- w
k <- 1200

lala <- algo.farrington.data.glm(dayToConsider, b, freq, 
                                     epochAsDate,epochStr,
									 vectorOfDates,w,noPeriods,
									 observed,population,
									 verbose,pastWeeksNotIncluded,k)
checkEquals(names(lala)==c( "response", "wtime","population","seasgroups","vectorOfDates"),rep(TRUE,5))
checkTrue(class(lala)=="data.frame")
checkEquals(diff(lala$wtime)==rep(1,length(lala$wtime)-1),rep(TRUE,length(lala$wtime)-1))
checkTrue(length(levels(lala$seasgroups))==noPeriods)

################################################################################
# END OF DATA GLM FUNCTION TESTS
################################################################################

################################################################################
# GLM FUNCTION TESTS
################################################################################

dataGLM <- lala
timeTrend <- TRUE
populationOffset <- TRUE
factorsBool <- TRUE
reweight <- TRUE
weightsThreshold <- 1
pThresholdTrend <- 1
b <- 3
noPeriods <- 10
typePred <- "link"
fitFun <- "algo.farrington.fitGLM.flexible"
glmWarnings <- FALSE
epochAsDate <- TRUE
dayToConsider <- as.Date("2013-05-30")
diffDates <- 7
populationNow <- 10

finalModel <- algo.farrington.glm(dataGLM,timeTrend,populationOffset,factorsBool,
                                reweight,weightsThreshold,pThresholdTrend,b,
								noPeriods,typePred,fitFun,glmWarnings,epochAsDate,
								dayToConsider,diffDates,populationNow,verbose=FALSE)
checkEquals(names(finalModel)==c("pred","doTrend","coeffTime","phi"),rep(TRUE,4))
pThresholdTrend <- 1
b <- 2
finalModel <- algo.farrington.glm(dataGLM,timeTrend,populationOffset,factorsBool,
                                reweight,weightsThreshold,pThresholdTrend,b,
								noPeriods,typePred,fitFun,glmWarnings,epochAsDate,
								dayToConsider,diffDates,populationNow,verbose=FALSE)
checkEquals(finalModel$doTrend,FALSE)
################################################################################
# END OF GLM FUNCTION TESTS
################################################################################





















