% hhh4
\name{hhh4}
\alias{hhh4}
\alias{plot.ah4}
\alias{oneStepAhead}
\alias{predict.ah4}
\alias{coef.ah4}
\alias{logLik.ah4}
\alias{print.ah4}
\alias{interpretControl}
\alias{checkFormula}
\alias{ri}
\alias{fe}
\alias{isInModel}
\alias{setControl}
\alias{addSeason2formula}
\alias{newtonRaphson}
\alias{fitHHH}
\alias{updateVariance}
\alias{updateRegression}
\alias{d2Sigma3}
\alias{dSigma3}
\alias{d2Sigma1}
\alias{d2Sigma2}
\alias{marFisher}
\alias{marScore}
\alias{marLogLik}
\alias{getSigma}
\alias{getSigmaInv}
\alias{getSigmaiInv}
\alias{getSigmai}
\alias{sqrtOf1pr2}
\alias{penFisher}
\alias{penScore}
\alias{penLogLik}
\alias{meanHHH}
\alias{splitParams}
\alias{weightedSumNE}
\alias{getCov}
\alias{getSdCorr}
\alias{ranef}
\alias{fixef}

\encoding{latin1}
\title{Random effects HHH model fit as described in Paul and Held (2011)} 
\description{
  Fits a Poisson or negative binomial model with mean
  \eqn{\mu_{it}}{\mu_it} containing endemic and epidemic components
  to a multivariate time series of counts. The linear predictors may contain
  random effects. 
}
\usage{
hhh4 <- function(stsObj, 
   control = list(
               ar = list(f = ~ -1,        
                         lag = 1,         
                         weights = NULL,  
                         initial = NULL   
                         ),
               ne = list(f = ~ -1,        
                         lag = 1,         
                         weights = NULL,  
                         initial = NULL   
                         ),
               end = list(f = ~ 1,         
                          offset = NULL,   
                          initial = NULL   
                          ),
               family = c("Poisson","NegBin1","NegBinM")[1],
               subset = 2:nrow(stsObj),             
               optimizer = list(tech = "nlminb",    
                                stop.tol = 1e-5,
                                stop.niter = 100),
               verbose = FALSE,
               start=list(fixed=NULL,random=NULL,sd.corr=NULL),
               data=data.frame(t=epoch(sts)-1)   
               )
   )
}
\arguments{
  \item{stsObj}{object of class \code{sts} containing the multivariate
    count data time series}
  \item{control}{control object, which is a list containing several
    components:
    \describe{
      \item{\code{ar}}{\describe{
	  \item{f=list(f = ~ -1)}{a formula for \eqn{\exp(x'\lamba)\cdot y_{t-1}}{exp(x'lamba)*y_t-1}}
	  \item{lag = 1}{autoregression on \eqn{y_{i,t-lag}}{y_i,t-lag}
	    (currently not used)}
	  \item{weights = NULL}{optional weights, only used if model is
	    a contact matrix (currently not used)}
	  \item{initial = NULL}{vector with initial values for
	    parameters if pred is a matrix or if pred = ~1 (not really
	    used ATM)}
	}
      }
      \item{\code{ar}}{A list with the following elements:
	\describe{
          \item{f = ~ -1}{a formula \eqn{\exp(x'\lamba)\cdot y_{t-1}}{exp(x'lamba)*y_t-1}}
	  \item{lag = 1}{autoregression on y_i,t-lag (currently not
	    used)}
	  \item{weights = NULL}{optional weights, only used if model
	    is a contact matrix (currently not used)}
	  \item{initial = NULL}{vector with initial values for
	    parameters if pred is a matrix or if pred = ~1 (not really
	    used ATM)}
	}
      }
      \item{\code{ne}}{A list with the following components:
	\describe{
	  \item{f = ~ -1}{a formula \eqn{\exp(x'phi) \cdot \sum_j {w_ji \cdot
		y_{j,t-1}}}{exp(x'phi) * \sum_j {w_ji *
		y_j,t-1}}}
	  \item{lag = 1}{autoregression on y_j,t-lag  (currently not
	    used)}
	  \item{weights = NULL}{weights w_ji, if NULL neighbourhood
	    matrix of stsObj is used}
	  \item{initial = NULL}{vector with initial values for
	    parameter if pred = ~1 (not really used ATM)}
	}
      }
      \item{\code{end}}{Model for the endemic component given as list
	with the following components
	\describe{
	  \item{f = ~ 1}{a formula \eqn{exp(x'nu) * n_it}}
	  \item{offset = NULL}{optional offset n_it}
	  \item{initial = NULL}{vector with initial values for
	    parameter if pred = ~1 (not really used ATM)}
	}
      }
      \item{\code{family}}{Distributional family -- either Poisson,
	or one of two parametrizations of the negative Binomial
	distribution
      }
      \item{\code{subset}}{typically 2:nrow(obs) if model contains
	autoregression}
      \item{optimizer}{ details for used optimizer (nlminb is default, optim may also be used)}
      \item{verbose}{Logical if additional information is to be printed
	during the computations}
      \item{\code{start}}{list with initials, overrides any initial
	values in formulas}
      \item{\code{data}}{ data.frame or named list with covariates that
	are specified in the formulas for the 3 components}
    }
  }
}

\value{Returns an object of class \code{ah4} with elements
  \item{coefficients}{named vector with estimated parameters of the model}
  \item{se}{estimated standard errors}
  \item{cov}{covariance matrix}
  \item{Sigma}{  }
  \item{Sigma.orig}{  }
  \item{Sigma.cov}{  }
  \item{call}{  }
  \item{dim}{  }
  \item{loglikelihood}{loglikelihood avaluated at the MLE}
  \item{margll}{  }
  \item{convergence}{  }
  \item{fitted.values}{fitted mean values \eqn{\mu_{i,t}}{\mu_it}}
  \item{control}{control object of the fit}
  \item{terms}{  }
  \item{stsObj}{  }
  \item{lag}{  }
  \item{nObs}{number of observations used for fitting the model}
  \item{nTime}{  }
  \item{nUnit}{  }
}

\details{

  Note that for the time being this function performs only modelling of
  the multivariate time series. No surveillance/monitoring functionality
  is available.

}
\seealso{}
\author{M. Paul and L. Held}
\examples{
######################################################################
# Fit the models from the Paul & Held (2011) paper for the influenza data
# from Bavaria and Baden-Wuerttemberg. For further documentation see
# also the vignette
######################################################################

data("flu-BYBW") 


###############################################################
## generate formula for temporal and seasonal trends
f.end <- addSeason2formula(f = ~ -1 + ri(type="iid", corr="all") + I((t-208)/100), S=3, period=52)

##########################
## models 
# A0
cntrl_A0 <- list(ar = list(f = ~ -1),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
(res_A0 <- hhh4(sts.flu,cntrl_A0))

\dontrun{
# B0
cntrl_B0 <- list(ar = list(f = ~ 1),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_B0 <- hhh4(sts.flu,cntrl_B0)               
 

# C0
cntrl_C0 <- list(ar = list(f = ~ -1 + ri(type="iid", corr="all")),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_C0 <- hhh4(sts.flu,cntrl_C0)               


#A1
cntrl_A1 <- list(ar = list(f = ~ -1),
	      ne = list(f = ~ 1, weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_A1 <- hhh4(sts.flu,cntrl_A1)               


# B1
cntrl_B1 <- list(ar = list(f = ~ 1),
	      ne = list(f = ~ 1, weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_B1 <- hhh4(sts.flu,cntrl_B1)               


# C1
cntrl_C1 <- list(ar = list(f = ~ -1 + ri(type="iid", corr="all")),
	      ne = list(f = ~ 1, weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_C1 <- hhh4(sts.flu,cntrl_C1)               


#A2
cntrl_A2 <- list(ar = list(f = ~ -1),
	      ne = list(f = ~ -1 + ri(type="iid", corr="all"), weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_A2 <- hhh4(sts.flu,cntrl_A2)               


# B2
cntrl_B2 <- list(ar = list(f = ~ 1),
	      ne = list(f = ~ -1 + ri(type="iid", corr="all"), weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_B2 <- hhh4(sts.flu,cntrl_B2)               

# C2
cntrl_C2 <- list(ar = list(f = ~ -1 + ri(type="iid", corr="all")),
	      ne = list(f = ~ -1 + ri(type="iid", corr="all"), weights = wji),
              end = list(f =f.end, offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1),
              start=list(fixed=fixef(res_B0),random=c(rep(0,140),ranef(res_B0)), sd.corr=c(-.5,getSdCorr(res_B0),0)))
res_C2 <- hhh4(sts.flu,cntrl_C2)               


# D
cntrl_D <- list(ar = list(f = ~ 1),
	      ne = list(f = ~ -1 + ri(type="iid"), weights = wji),
              end = list(f =addSeason2formula(f = ~ -1 + ri(type="car") + I((t-208)/100), S=3, period=52), offset = population(sts.flu)),
	      family="NegBin1",optimizer = list(tech = "nlminb",stop.tol = 1e-5,stop.niter = 200),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
res_D <- hhh4(sts.flu,cntrl_D)               


#####################################################################
## do 1-step ahead predictions
#####################################################################
tp <- nrow(sts.flu)-2*52

val_A0 <- oneStepAhead(res_A0,tp=tp) 
val_B0 <- oneStepAhead(res_B0,tp=tp) 
val_C0 <- oneStepAhead(res_C0,tp=tp) 

val_A1 <- oneStepAhead(res_A1,tp=tp) 
val_B1 <- oneStepAhead(res_B1,tp=tp) 
val_C1 <- oneStepAhead(res_C1,tp=tp) 

val_A2 <- oneStepAhead(res_A2,tp=tp) 
val_B2 <- oneStepAhead(res_B2,tp=tp) 
val_C2 <- oneStepAhead(res_C2,tp=tp) 

val_D <- oneStepAhead(res_D,tp=tp) 


##################################
## compute scores
################################

#scores
vals <- ls(pattern="val_")

nam <- substring(vals,first=5,last=6)

uni <- NULL
indiv <- TRUE

scores_i <- list()
meanScores <- NULL
for(i in seq(along.with=vals)){
  sc <- scores(get(vals[i]),unit=uni, individual=indiv)
  scores_i[[i]] <- sc
  meanScores <- rbind(meanScores,colMeans(sc))
}

names(scores_i) <- nam
rownames(meanScores) <- nam

##comparison with best model B2 

compareWithBest <- function(best, whichModels, whichScores=1:3, nPermut=9999, seed=1234){
  set.seed(seed)
  pVals <- NULL
  for(score in whichScores){
    p <- c()
    for(model in whichModels){
      if(model==best) p <- c(p,NA)
      else p <- c(p,permutationTest(scores_i[[model]][,score],scores_i[[best]][,score],plot=T,nPermutation=nPermut)$pVal.permut)
    }  
    pVals <- cbind(pVals,p)
  }
  return(pVals)
}


pVals_flu <- compareWithBest(best=6, whichModels=1:10,seed=2059710987)
rownames(pVals_flu) <- nam

}
}
\keyword{ts}
\keyword{regression}
\references{
  Held, L., \enc{Höhle}{Hoehle}, M., Hofmann, M. (2005) A statistical framework 
    for the analysis of multivariate infectious disease surveillance counts, 
    Statistical Modelling, \bold{5}, 187--199.
    
  Paul, M., Held, L. and Toschke, A. M. (2008) Multivariate modelling of 
    infectious disease surveillance data, Statistics in Medicine, \bold{27}, 
    6250--6267.    

  Paul, M. and Held, L. (2011) Predictive assessment of a non-linear
    random  effects model for multivariate time series of infectious
    disease counts. Statistics in Medicine, DOI: 10.1002/sim.4177.
}




