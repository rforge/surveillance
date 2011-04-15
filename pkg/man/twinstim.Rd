%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rd file for function 'twinstim'
%%
%% Author: Michael Höhle based on the in-code documentation of
%%         Sebastian Meyer.
%% $Date: 2010-07-07 00:12:46 +0200 (Wed, 07 Jul 2010) $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\encoding{latin1}
\name{twinstim}
\alias{twinstim}
\alias{coef.twinstim}
\alias{vcov.twinstim}
\alias{logLik.twinstim}
\alias{print.twinstim}
\alias{summary.twinstim}
\alias{print.summary.twinstim}
\alias{toLatex.summary.twinstim}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fit a two-component spatio-temporal conditional intensity function model
}
\description{
  A twinstim model as described in Meyer et al. (2010) is fitted to
  marked spatio-temporal point process data. This constitutes a
  regression approach for conditional intensity function modelling.
}
\usage{
twinstim(endemic, epidemic, siaf, tiaf, qmatrix = data$qmatrix, data, subset, na.action = na.fail, optim.args, nCub, partial = FALSE, finetune = TRUE, t0 = data$stgrid$start[1], T = tail(data$stgrid$stop, 1), typeSpecificEndemicIntercept = FALSE, model = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{endemic}{
    Formula for the exponential (Cox-like multiplicative) endemic component. may contain offsets. If \code{~0} there will be no endemic component in the model.
}
  \item{epidemic}{
    Formula representing the epidemic model for the event-specific covariates determining infectivity. offsets are not implemented. If \code{~0} there will be no epidemic component in the model.
}
  \item{siaf}{
    Spatial interaction functions. May be \code{NULL} (or missing), a list
    (continous function) or numeric (knots of a step function).
}
\item{tiaf}{
    Temporal interaction functions. May be \code{NULL} (or missing), a list
    (continous function) or numeric (knots of a step function).
}
  \item{qmatrix}{
    Square indicator matrix (0/1 or \code{TRUE}/\code{FALSE}) for
    possible transmission between the event types. Will be internally
    converted to logical. Defaults to the Q matrix specified in data.
}
  \item{data}{
    An \code{epidataCS} object.
}
\item{subset}{
  Logical expression indicating events to keep: missing values are
  taken as false. The expression is evaluated in the context of the
  \code{data$events@data} \code{data.frame}
}
\item{na.action}{
  How to deal with missing values in '\code{data$events}'. Do not use
  'na.pass'. Missing values in the spatio-temporal grid '\code{data$stgrid}'
  are not accepted.
}
\item{optim.args}{
  \code{NULL} or an argument list passed to 'optim' containing at least
  the element 'par', the start values of the parameters in the order par
  = c(endemic, epidemic, siaf, tiaf).

  Exceptionally, the 'method' argument may also be "nlminb", in which
  case the 'nlminb' optimizer is used. This is also the default. In this case,
  the only control parameters available are \code{maxit}, \code{REPORT},
  \code{abstol}, and \code{reltol}, which are passed appropriately to
  \code{nlminb}.

  If 'optim.args' is \code{NULL} then no optimization will be performed but
  the necessary functions will be returned in a list (similar to
  '\code{model = TRUE}').
}
\item{nCub}{
  Determines the accuracy of the cubature of the 'siaf' function. If \code{siaf$Fcircle} is specified, nCub = effRange/eps, where eps is used as pixel width and height in the two-dimensional midpoint rule (see polyCub.midpoint). Thus nCub is the desired number of subdivions of effRange in both dimensions and eps = effRange/nCub. If \code{siaf$Fcircle} is missing, nCub equals the above mentioned eps.
}
\item{partial}{
  Logical indicating if the partial log-likelihood proposed by Diggle et
  al. (2009) should be used.
}
\item{finetune}{
  Logical indicating if a second maximisation should be performed with
  robust Nelder-Mead optim using as starting point the resulting
  parameters from the first maximisation. Default value is TRUE.
}
\item{t0}{
  Events having occured during (-Inf;t0] are regarded as part of the
  prehistory H_0 of the process. The time point 't0' must be an element
  of data$stgrid$start. By default it is the earliest time point of the
  spatio-temporal grid of 'data'.
}
\item{T}{
  Only use events that occured up to time T. The time point 'T' must be
  an element of data$stgrid$stop. By default it is the latest time point
  of the spatio-temporal grid of 'data'.
}
\item{typeSpecificEndemicIntercept}{
  Use type-specific endemic intercepts instead of the global endemic intercept?
}
  \item{model}{
    Logical. If \code{TRUE} the result contains an element \code{functions} with
    the log-likelihood function, and optionally the score function and
    the fisher information function. The environment of those functions
    equals the evaluation environment of the fitting function, i.e. it
    is kept in the workspace and the necessary model frames are still
    available when \code{twinstim} has finished.
}
}
\details{
  The function performs maximum likelihood inference
  for the additive-multiplicative spatio-temporal intensity model
  described in Meyer et al. (2010). It uses 'nlminb' as the default optimizer
  (Newton-algorithm) and returns an object of class \code{twinstim}.
  Such ojects have working \code{print},
  \code{plot} and \code{summary} functions. The output of can again be
  processed by the \code{toLatex} function. Furthermore, the usual model
  fit functions such as \code{coef}, \code{vcov} and \code{logLik} work.
  A specific add on is the use of the function \code{R0}.
}
\value{
  Returns an S3 object of class "twinstim".
%%  ~Describe the value returned
\describe{
  \item{coefficients}{Vector containing the MLE.}
  \item{loglik}{Value of the log-likelihood function at the MLE.}
  \item{partial}{If \code{FALSE}, the fit is based on the full likelihood.
    If \code{TRUE} then a partial likelihood approach was used.}
  \item{counts}{Number of log-likelihood and score evaluations during \code{optim}.}
  \item{coverged}{Logical.}
  \item{fisherinfo}{Expected Fisher information evaluated at the
    MLE. Only part of the output if \code{partial} is \code{FALSE} and if
    spatial and temporal interaction functions are provided with their
    derivatives.}
  \item{method}{Name of the optimizing function used to determine the MLE.}
  \item{fisherinfo.observed}{Observed Fisher information matrix
    evaluated at the value of the MLE. Obtained as the negative Hessian.
    This is only part of the result if \code{optim.args$method} is not
    \code{"nlminb"} (i.e. \code{optim} is used) and if it was requested by setting
    \code{hessian=TRUE} in \code{optim.args}.}
  \item{fitted}{Fitted values of the conditional intensity at the events.}
  \item{fittedComponents}{Matrix with columns \code{"h"} and \code{"e"} containing
    the fitted values of the endemic and epidemic components, respectively.}
  \item{tau}{Fitted cumulative intensities at the events, i.e.\ the transformed
    time points used in the residual analysis of point process models.}
  \item{npars}{Description of 'comp2'}
  \item{qmatrix}{Description of 'comp2'}
  \item{medianeps}{???}
  \item{formula}{A list containing four components
    \describe{
      \item{endemic}{Formula used to specify the endemic component.}
      \item{epidemic}{Formula used to specify the epidemic component.}
      \item{siaf}{Formula used to specify the spatial interaction
	function.}
      \item{tiaf}{Formula used to specify the temporal interaction function.}
    }
  }
  \item{functions}{If \code{model=TRUE} this is a \code{list} with
  components \code{ll}, \code{sc} and \code{fi} containing objects of
  type \code{function} containing functions for evaluating the
  likelihood, the score and the expected fisher information for a
  parameter vector \eqn{\theta}{theta}.
  }
  \item{call}{The matched call.}
  \item{runtime}{Contains the \code{proc.time} queried time taken to fit
    the model.}
}
}
\references{
  S. Meyer, J. Elias and M. Höhle (2010): A space-time conditional
  intensity model for infectious disease occurence, Department of
  Statistics: Technical Reports, No. 95, 27 pages, 2010.
  Available as http://epub.ub.uni-muenchen.de/11898/
  
  Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
  on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet}
  München. Available as http://epub.ub.uni-muenchen.de/11703/
}
\author{
  S. Meyer with documentation by M. \enc{Höhle}{Hoehle}.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{surveillance::twinSIR}}.
}
\examples{
#Load invasive meningococcal disease data
data("imdepi")

\dontrun{
#Indicator - we only take cases with all necessary covariates observed
allEpiCovNonNA <- !is.na(imdepi$events@data$agegrp) &
                     !is.na(imdepi$events@data$type)

#Define spatial interaction function to be a isotropic bivariate
#Gaussian with same parameters for both finetypes
siaf_log1 <- siaf.gaussian(1, logsd = TRUE, density = FALSE, effRangeMult = 6)
siaf_constant <- siaf.constant

#Start values (taken from the best model in fits5 of the Epi/mydata
#startvalues <-  c(-20.36516096, -0.04926598, 0.26183958, 0.26681968,
#-12.57458928, 0.64631559, -0.18675885, -0.84955802, 2.82866154)
#Crude intercept estimate -> if model only has an endemic component with
#a single population 
#popdensity.overall <- mean(subset(imdepi$stgrid, BLOCK == 1)$popdensity)
#h.intercept <- with(as(imdepi$events,"data.frame"),log(length(ID)/max(time)))/popdensity.overall

#Define start values
optim.args <- list(par = rep(0,8), method = "nlminb", control = list(fnscale = -10000,trace=4,REPORT=1))

#Fit a twinstim model (no space interaction)
imdepi.fit0 <- twinstim(endemic  = ~1 + offset(log(popdensity)) + I(start/365) + 
              sin(start * 2 * pi/365) + cos(start * 2 * pi/365), 
              epidemic = ~1 + agegrp + type, 
              data = imdepi, subset = allEpiCovNonNA, 
              optim.args = optim.args, finetune=FALSE, model=TRUE,
              nCub = 24, 
              typeSpecificEndemicIntercept = FALSE)

#Now with spatial interaction function
optim.args <- modifyList(optim.args,list(par=c(coef(imdepi.fit0),3)))
imdepi.fit1 <- twinstim(endemic  = ~1 + offset(log(popdensity)) + I(start/365) + 
              sin(start * 2 * pi/365) + cos(start * 2 * pi/365), 
              epidemic = ~1 + agegrp + type, 
              siaf = siaf_log1,
              data = imdepi, subset = allEpiCovNonNA, 
              optim.args = optim.args, finetune=FALSE, model=TRUE,
              nCub = 24, 
              typeSpecificEndemicIntercept = FALSE)


summary(imdepi.fit)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{optimize}
