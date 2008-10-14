\name{algo.twins}
\alias{algo.twins}

\encoding{latin1}
\title{Model fit based on a two-component epidemic model} 
\description{
        Fits a Poisson/negative binomial model with mean \eqn{\mu_{it}}{\mu_it} 
        (as described in Held/Höhle/Hofmann, 2005) to a multivariate 
        time series of counts.  
}
\usage{
algo.twins(disProgObj, control=list(burnin=1000, filter=10,
   sampleSize=2500, alpha_xi=10, beta_xi=10, psiRWSigma=0.25, alpha_psi=1,
   beta_psi=0.1, logFile="twins.log"))
}
\arguments{
  \item{disProgObj}{object of class \code{disProg}}
  \item{control}{control object:
        \describe{
        \item{\code{lambda}}{If \code{TRUE} an autoregressive 
              parameter \eqn{\lambda} is included, if \code{lambda} is a vector of logicals,
              unit-specific parameters \eqn{\lambda_i} are included. By default, observations \eqn{y_{t-lag}}{y_t-lag}
              at the previous time points, i.e. \eqn{lag=1}, are used for the autoregression. Other lags can be
              used by specifying \code{lambda} as a vector of integers, see Examples and \code{\link{meanResponse}} for details.}
        \item{\code{neighbours}}{If \code{TRUE} an autoregressive parameter for 
              adjacent units \eqn{\phi} is included, if \code{neighbours} is a vector of logicals,
              unit-specific parameters \eqn{\phi_i} are included. By default, observations \eqn{y_{t-lag}}{y_t-lag}
              at the previous time points, i.e. \eqn{lag=1}, are used for the autoregression. Other lags can be
              used by specifying \code{neighbours} as a vector of integers.}
        \item{\code{linear}}{a \code{logical} (or a vector of logicals) indicating wether a linear 
              trend \eqn{\beta} (or a linear trend \eqn{\beta_i} for each unit)
              is included}
        \item{\code{nseason}}{Integer number of Fourier frequencies; if \code{nseason} is a vector
              of integers, each unit \eqn{i} gets its own seasonal parameters }
        \item{\code{negbin}}{if \code{"single"} negative binomial rather than poisson is used,
              if \code{"multiple"} unit-specific overdispersion parameters are used.}
        \item{\code{proportion}}{see details in \code{\link{meanResponse}} }
        \item{\code{lag.range}}{determines which observations are used to fit the model }
        }}        
%  \item{thetastart}{vector with starting values for all parameters specified
%      in the control object (for \code{optim}).  } 
%  \item{verbose}{if \code{true} information about convergence is printed}

}
\value{Returns an object of class \code{atwins} with elements 
  \itemize{
    \item{control}{specified control object}
    \item{disProgObj}{specified \code{disProg}-object}
    \item{logFile}{MISSING!!}
    \item{logFile2}{MISSING!!}
  }
}
\details{Note that for the time being this function is not a
  surveillance algorithm, but only a modelling approach as described in
  the Held et. al (2006) paper.

  Note also that the function writes three logfiles in your current
  working directory (i.e. getwd()): twins.log, twins.log.acc and twins.log2
  Thus you need to have write permissions in the current getwd() directory.
}
\author{M. Hofmann and M. Höhle}
\examples{

#Load the data used in the Held et al. (2006) paper
data("hepatitisA")

#Fix seed - this is used for the MCMC samplers in twins
set.seed(123)

#Call algorithm and save result
otwins <- algo.twins(hepatitisA)

#This shows the entire output
plot(otwins)

#Direct access to MCMC output
hist(otwins$logFile$psi,xlab=expression(psi),main="")
require("coda")
print(summary(mcmc(otwins$logFile[,c("psi","xipsi","K")])))

}
\keyword{ts}
\keyword{regression}
\source{L. Held, M. Hofmann, M. Höhle and V. Schmid, A two-component
  model for counts of infectious diseases (2006), Biostatistics, 7, pp.
  422–437.
}





