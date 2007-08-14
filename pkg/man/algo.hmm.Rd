\name{algo.hmm}
\alias{algo.hmm}
\title{Hidden Markov Model (HMM) method}
\description{ 
  %
  This function implements HMM detection of outbreaks as described in
  the LeStrat and Carret paper (see literature) by heavily exploiting
  functionality of the \code{msm} library. Using the function
  \code{msm} a specified HMM is estimated, the decoding problem,
  i.e. the most probable state configuration, is found by the Viterbi
  algorithm.

}
\usage{
  algo.hmm(disProgObj, control = list(range=range, noStates=2, trend=TRUE, 
           noHarmonics=1,covEffectEqual=FALSE))
}
\arguments{
        \item{disProgObj}{object of class disProg (including the observed and the state chain).}
        \item{control}{control object: 
            \describe{
            \item{\code{range}}{determines the desired time points
                which should be evaluated. Note that opposite to other
                surveillance methods an initial parameter estimation
                occurs in the HMM}
            \item{\code{noStates}}{number of hidden states in the HMM -- the typical choice is 2. The initial rates are set such that the \code{noState}'th state is the one having the highest rate. I.e. this state is considered the outbreak state.}
            \item{\code{trend}}{Boolean stating whether a linear time trend exists, i.e. if \code{TRUE} (default) then \eqn{beta_j != 0}.}
            \item{\code{noHarmonics}}{Number of harmonic waves to include in the linear predictor. Default is 1.}
            \item{\code{covEffectEqual}}{See details.}
            }
          }
}
\value{
\item{survRes}{
    \code{algo.hmm} gives a list of class \code{survRes} which includes the 
    vector of alarm values for every timepoint in \code{range}. No
    \code{upperbound} can be specified and is put equal to zero.
    
    The resulting object contains a slot \code{control$hmm}, which contains the \code{msm} object with the fitted HMM.
  }
}
\details{
A \code{noState}-State Hidden Markov Model (HMM) is used based on the Poisson distribution with linear predictor on the log-link scale. I.e. 
\deqn{Y_t | X_t = j \sim Po(\mu_t^j)}{Y_t|X_t = j ~ Po(\mu_t^j)}, where
\deqn{\log(\mu_t^j) = \alpha_j + \beta_j\cdot t + \sum_{i=1}^{nH} \gamma_j^i \cos(2i\pi/freq\cdot (t-1)) + \delta_j^i \sin(2i\pi/freq\cdot (t-1))}{%
log(mu_t^j) = alpha_j + beta_j t + \sum_{i=1}^{nH} gamma_j^i \cos(2*i*pi/freq * (t-1)) + delta_j^i sin(2*i*pi/freq * (t-1))
}
%
and \eqn{nH=noHarmonics} and \eqn{freq=12,52} depending on the
sampling frequency of the surveillance data. In the above \eqn{t-1} is
used, because the first week is always saved as \code{t=1}, i.e. we
want to ensure that the first observation corresponds to cos(0) and
sin(0).

If \code{covEffectEqual} then all covariate effects parameters are
equal for the states, i.e. \eqn{\beta_j=\beta, \gamma_j^i=\gamma^i,
  \delta_j^i=\delta^i} for all \eqn{j=1,...,noState}.

In case more complicated HMM models are to be fitted it is possible to
modify the \code{msm} code used in this function. Using
e.g. \code{AIC} one can select between different models (see the
\code{msm} package for further details).


}
\seealso{\code{\link[msm:msm]{msm}}}
\author{M. Höhle}
\examples{
set.seed(123)
#Simulate outbreak data from HMM
counts <- sim.pointSource(p = 0.99, r = 0.8, length = 52*8,
                              A = 1, alpha = 1, beta = 0, phi = 0,
                              frequency = 1, state = NULL, K = 1.5)

#Do surveillance using a two state HMM without trend component and
#the effect of the harmonics being the same in both states
surv <- algo.hmm(counts, control=list(range=1:length(counts$observed),noStates=2,trend=FALSE,covEffectsEqual=TRUE))
plot(surv)

#Look at the estimated HMM (see ?msm for details)
surv$control$hmm

#Extract transition matrix of HMM (see ?pmatrix.msm for details)
pmatrix.msm(surv$control$hmm)

}
\references{
  Y. Le Strat and F. Carrat, Monitoring Epidemiologic Surveillance Data using Hidden Markov Models (1999), Statistics in Medicine, 18, 3463--3478


  I.L. MacDonald and W. Zucchini, Hidden Markov and Other Models for Discrete-valued Time Series,  (1997), Chapman & Hall, Monographs on Statistics and applied Probability 70

}

\keyword{}

