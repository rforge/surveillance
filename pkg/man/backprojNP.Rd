\encoding{latin1}
\name{backprojNP}
\alias{backprojNP}
%Internal functions
\alias{backprojNP.fit}
\alias{naninf2zero}
\alias{em.step.becker}

\title{
  Non-parametric back-projection of incidence cases to exposure cases
  using a known incubation time as in Becker et al (1991).
}

\description{
  The function is an implementation of the non-parametric
  back-projection of incidence cases to exposure cases described in
  Becker et al. (1991). The method back-projects exposure times
  from a univariate time series containing the number of symptom onsets per time
  unit. Here, the delay between exposure and symptom onset for an
  individual is seen as a realization of a random variable governed by a
  known probability mass function.
  The back-projection function calculates the expected number of exposures
  \eqn{\lambda_t}{lambda_t} for each time unit under the assumption of a
  Poisson distribution, but without any parametric assumption on how the
  \eqn{\lambda_t}{lambda_t} evolve in time.

  Furthermore, the function contains a boostrap based procedure, as
  given in Yip et al (2011), which allows an indication of uncertainty
  in the estimated \eqn{\lambda_t}{lambda_T}. The procedure is
  equivalent to the suggestion in Becker and Marschner (1993). However,
  the present implementation in \code{backprojNP} allows only a
  univariate time series, i.e. simulteneous age groups as in Becker and
  Marschner (1993) are not possible.

  The method in Becker et al. (1991) was originally developed for the
  back-projection of AIDS incidence, but it is equally useful for
  analysing the epidemic curve in outbreak situations of a disease
  with long incubation time, e.g. in order to qualitatively investigate
  the effect of intervention measures.
}

\usage{
backprojNP(sts, incu.pmf.vec,control=list(k=2,eps=rep(0.005,2),iter.max=rep(250,2),B=-1,alpha=0.05,verbose=FALSE,lambda0=NULL,hookFun=function(Y,lambda,...) {}),...)
}

\arguments{
  \item{sts}{
    an object of class \code{"\link{sts}"} (or one that can be coerced to
    that class): contains the observed number of symptom onsets as a
    time series. 
  }
  \item{incu.pmf.vec}{Probability mass function (PMF) of the incubation
    time. The PMF is specified as a vector with the value of the PMF
    evaluated at \eqn{0,...,d_max}{0,...,d_max}, i.e. note that the
    support includes zero. The value of \eqn{d_max}{d_max} is
    automatically calculated as \code{length(incu.pmf.vec)-1}.
  }
  \item{control}{A list with named arguments controlling the
    functionality of the non-parametric back-projection.
    \describe{
      \item{\code{k}}{An integer representing the smoothing parameter to use in the smoothing step
	of the EMS algorithm. Needs to be an even number.
      }
      \item{\code{eps}}{A vector of length two representing the
	convergence threshold \eqn{\epsilon}{epsilon} of the EMS algorithm, see Details for further
	information. The first value is the threshold to use in the
	\eqn{k=0}{k=0} loop, which forms the values for the parametric
	bootstrap. The second value is the threshold to use in the actual fit
	and bootstrap fitting using the specified \code{k}. If \code{k} is
	only of length one, then this number is replicated twice.
      }
      \item{\code{iter.max}}{
      }
      \item{\code{B}}{
      }
      \item{\code{alpha}}{
      }
      \item{\code{verbose}}{
      }
      \item{\code{lambda0}}{
      }
      \item{\code{hookFun}}{
      }
      \item{\dots}{
      }
    }
  }
}

\details{
  Becker et al. (1991) specify a non-parametric back-projection
  algorithm based on the Expectation-Maximization-Smoothing (EMS)
  algorithm.
  
  In the present implementation the algorithm iterates until
  \deqn{\frac{||\lambda^{(k+1)} - \lambda^{(k)}||}{||\lambda^{(k)}||} <
  \epsilon} This is a slight adaptation of the proposals in Becker et
  al. (1991).

  
  See the references for further information.
}

\value{
  \code{backprojNP} returns an object of \code{\link{stsBP}}.
}

\references{
  Becker NG, Watson LF and Carlin JB (1991), A method for
  non-parametric back-projection and its application to AIDS data,
  Statistics in Medicine, 10:1527-1542.
  
  Becker NG and Marschner IC (1993), A method for estimating the
  age-specific relative risk of HIV infection from AIDS incidence data,
  Biometrika, 80(1):165-178.

  Yip PSF, Lam KF, Xu Y, Chau PH, Xu J, Chang W, Peng Y, Liu Z, Xie X and
  Lau HY (2011), Reconstruction of the Infection Curve for SARS Epidemic
  in Beijing, China Using a Back-Projection Method, Communications in
  Statistics - Simulation and Computation, 37(2):425-433.
}

\author{
Michael \enc{Höhle}{Hoehle}
}

\note{
  The method is still experimental. A proper plot routine for
  \code{stsBP} objects is currently missing.
}

\seealso{
}

\examples{
#Generate an artificial outbreak of size n starting at time t0 and being of length
n <- 1e3 ; t0 <- 23 ; l <- 10

#PMF of the incubation time is an interval censored gamma distribution
#with mean 15 truncated at 25.
inc.pmf <- c(0,(pgamma(1:25,15,1.4) - pgamma(0:24,15,1.4))/pgamma(25,15,1.4))
#Function to sample from the incubation time
rincu <- function(n) {
  sample(0:25, size=n, replace=TRUE, prob=inc.pmf)
}
#Sample time of exposure and length of incubation time
set.seed(123)
exposureTimes <- t0 + sample(x=0:(l-1),size=n,replace=TRUE)
symptomTimes <- exposureTimes + rincu(n)

#Time series of exposured and symptoms. Convert Y to an sts object
X <- table( factor(exposureTimes,levels=1:(max(symptomTimes)+25)))
Y <- table( factor(symptomTimes,levels=1:(max(symptomTimes)+25)))
sts <- new("sts", epoch=1:length(Y),observed=matrix(Y,ncol=1))

#Plot the outbreak
plot(sts,xaxis.years=FALSE,legend=NULL)
#Add true number of exposures to the plot
lines(1:length(Y)+0.2,X,col="red",type="h",lty=2)


#Call non-parametric back-projection function with hook function but
#without bootstrapped confidence intervals
bpnp.control <- list(k=0,eps=rep(0.005,2), iter.max=rep(250,2),B=-1,hookFun=plotIt,verbose=TRUE)
sts.bp <- backprojNP(sts, incu.pmf.vec=inc.pmf, control=bpnp.control, ylim=c(0,max(X,Y)))

#Show result
plot(sts.bp,xaxis.years=FALSE,legend=NULL,lwd=c(1,1,2),lty=c(1,1,1))
lines(1:length(Y)+0.2,X,col="red",type="h",lty=2)

#Non-parametric back-projection including boostrap CIs. B=10 is here
#not really useful, but computation times stays within the reasonable.
#More realistic is B=100 or B=500.
bpnp.control2 <- modifyList(bpnp.control, list(hookFun=NULL,k=2,B=10))
\dontrun{
bpnp.control2 <- modifyList(bpnp.control, list(hookFun=NULL,k=2,B=100))
}
sts.bp2 <- backprojNP(sts, incu.pmf.vec=inc.pmf, control=bpnp.control2)

######################################################################
# Plot the result. This is currently a manul routine. Need to specify 
# a plot method for stsBP objects.
#
# Parameters:
#  stsBP - object of class stsBP which is to be plotted.
######################################################################

plot.stsBP <- function(stsBP) {
  maxy <- max(observed(stsBP),upperbound(stsBP),stsBP@ci,na.rm=TRUE)
  plot(upperbound(stsBP),type="n",ylim=c(0,maxy), ylab="Cases",xlab="time")
  if (!all(is.na(stsBP@ci))) {
    polygon( c(1:nrow(stsBP),rev(1:nrow(stsBP))),
             c(stsBP@ci[,2],rev(stsBP@ci[,1])),col="lightgray")
  }
  lines(upperbound(stsBP),type="l",lwd=2)
  legend(x="topright",c(expression(lambda[t])),lty=c(1),col=c(1),fill=c(NA),border=c(NA),lwd=c(2))

  invisible()
}

#Plot the result of k=0 and add truth for comparison. No CIs available
plot.stsBP(sts.bp)
lines(1:length(Y),X,col=2,type="h")
#Same for k=2
plot.stsBP(sts.bp2)
lines(1:length(Y),X,col=2,type="h")

}

\keyword{models}
\keyword{optimize}
