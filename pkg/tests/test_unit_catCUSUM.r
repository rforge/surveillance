setwd("P:\\Daten\\Surveillance\\pkg\\R")
source("../R/catCUSUM.r")
library("testthat")
library('surveillance')
library('gamlss')
  ###########################################################################
# Load data
data("salmHospitalized")
# Define reference data and data under monitoring
phase1 <- which(isoWeekYear(epoch(salmHospitalized))$ISOYear<2013)
phase2 <- which(isoWeekYear(epoch(salmHospitalized))$ISOYear>=2013)
# Prepare data for fitting the model
weekNumber <-  isoWeekYear(epoch(salmHospitalized))$ISOWeek
salmHospitalized.df <- cbind(as.data.frame(salmHospitalized),weekNumber)
colnames(salmHospitalized.df) <- c("y","t","state","alarm","n","freq",
                                   "epochInPeriod","weekNumber")

# Fit beta-binomial model using GAMLSS
vars <- c("y","n","t","epochInPeriod","weekNumber")
m.bbin <- gamlss( cbind(y,n-y) ~ 1 + t
+ sin(2 * pi * epochInPeriod) + cos(2 * pi * epochInPeriod) 
+ sin(4 * pi * epochInPeriod) + cos(4 * pi * epochInPeriod)
+ I(weekNumber==1) + I(weekNumber==2), 
sigma.formula=~1,
family=BB(sigma.link="log"),
data=salmHospitalized.df[phase1,vars])
# CUSUM parameters
R <- 2 #detect a doubling of the odds for a salmHospitalized being positive
h <- 2 #threshold of the cusum - set also later how it was chosen
# Compute in-control and out of control mean
pi0 <- predict(m.bbin,newdata=salmHospitalized.df[phase2,vars],
               type="response")
pi1 <- plogis(qlogis(pi0)+log(R))
# Create matrix with in control and out of control proportions.
# Categories are D=1 and D=0, where the latter is the reference category
pi0m <- rbind(pi0, 1-pi0)
pi1m <- rbind(pi1, 1-pi1)

# Creation of the sts-object with the counts for the 2 categories
salmHospitalized.multi <- new("sts",freq=52,start=c(2004,1),
                              epoch = as.numeric(epoch(salmHospitalized)),
						      epochAsDate=TRUE,
                              observed=cbind(observed(salmHospitalized),
                              population(salmHospitalized) 
					          -observed(salmHospitalized)),
                              populationFrac=cbind(population(salmHospitalized),
                              population(salmHospitalized)),
                              state=matrix(0,nrow=nrow(salmHospitalized),
					          ncol=2), 
							  multinomialTS=TRUE)
# Function to use as dfun in the categoricalCUSUM 
mydBB.cusum <- function(y, mu, sigma, size, log = FALSE) 
{  return(dBB( if (is.matrix(y)) y[1,] else y, if (is.matrix(y)) mu[1,] else mu,
 sigma = sigma, bd = size, log = log)) 
}

# Monitoring
control <- list(range=phase2,h=2,pi0=pi0m, pi1=pi1m, ret="cases",
dfun=mydBB.cusum)
salmHospitalizedCat <- categoricalCUSUM(salmHospitalized.multi, 
                                        control=control,
                                        sigma=exp(m.bbin$sigma.coef))		
										
expect_that(salmHospitalizedCat@epoch==salmHospitalized.multi@epoch[phase2], equals(rep(TRUE,length(phase2))))	
expect_that(salmHospitalizedCat@start==c(isoWeekYear(epoch(salmHospitalizedCat)[1])$ISOYear,isoWeekYear(epoch(salmHospitalizedCat)[1])$ISOWeek), equals(rep(TRUE,2)))	


									