\name{glrnbFarrington}
\alias{glrnbFarrington}
\encoding{latin1}

\title{Count data regression chart for the monitoring of surveillance time series with in-control values estimated from GLM inspired by the one used in farringtonFlexible.}
\description{
%
  
 This function implements the seasonal cound data chart based on
  generalized likelihood ratio (GLR) as described in the Hoehle and Paul
  (2008) paper. In-control values (expected values and overdispersion) are estimated with a GLM of the negative binomial family,
  with an intercept, an optional time trend, an optional sinusoidal seasonal component and an optional population offset.
%
}
\usage{
glrnbFarrington(sts, control = list(range = NULL, b = 3, w = 3,
                                      reweight = TRUE, 
                                      weightsThreshold = 2.58,
                                      glmWarnings = TRUE,
                                      trend = TRUE,
                                      pThresholdTrend=1,                                     
                                      fitFun="fitGLM.nb",
                                      populationOffset = FALSE, 
                                      pastWeeksNotIncluded = 26, 
									  sinCos = TRUE,
									  c.ARL = 5, Mtilde = 1,
									  M =-1, change="intercept",
									  theta=NULL,dir=c("inc","dec"),
									  ret=c("cases","value")))
}
\arguments{
\item{sts}{object of class sts (including the \code{observed} and the \code{state} time series)}
\item{control}{Control object given as a \code{list} containing the following components:
    \describe{
    \item{\code{range}}{Specifies the index of all timepoints which
        should be tested. If range is \code{NULL} all possible timepoints are used.}
    \item{\code{b}}{How many years back in time to include when
        forming the base counts.}
    \item{\code{w}}{Window's half-size, i.e. number of weeks to include
        before and after the current week in each year. Here it only increases the scope of 
		data used as baseline when estimating the in-control GLM: the furthest timepoint used is
		the furthest timepoint that would have been used in farringtonFlexible for the first point
		under surveillance (first timepoint in \code{range} - \code{pastWeeksNotIncluded}).}
    \item{\code{reweight}}{Boolean specifying whether to perform reweighting step.}
     \item{\code{weightsThreshold}}{Defines the threshold for reweighting past outbreaks  using the Anscombe residuals
(1 in the original method, 2.58 advised in the improved method).}
    \item{\code{glmWarnings}}{Boolean specifying whether to print warnings from the call to \code{glm}.}
    \item{\code{trend}}{Boolean indicating whether a trend should be included and kept in
        case the conditions in the Farrington et. al. paper are met
        (see the results). If \code{false} then NO trend is fit.}
     \item{\code{pThresholdTrend}}{Threshold for deciding whether to keep trend in the model
(0.05 in the original method, 1 advised in the improved method).}
    \item{\code{limit54}}{Vector containing two numbers: \code{cases} and \code{period}. To avoid alarms in cases where the time series only
        has about almost no cases in the specific week the algorithm uses the following heuristic
        criterion (see Section 3.8 of the Farrington paper) to protect
        against low counts: no alarm is sounded if fewer than
        \eqn{\code{cases}=5} reports were received in the past \eqn{\code{period}=4}
        weeks. \code{limit54=c(cases,period)} is a vector allowing the
        user to change these numbers. Note: As of version 0.9-7 of the package the
        term "last" period of weeks includes the current week -
        otherwise no alarm is sounded for horrible large numbers if
        the four weeks before that are too low.}
    \item{\code{fitFun}}{String containing the name of the fit
        function to be used for fitting the GLM. The only current option is 
        "fitGLM.nbe".}
    \item{\code{populationOffset}}{Boolean specifying whether to include 
                                 a population offset in the GLM. 
                                 The slot \code{sts@population} gives the population vector.}
     \item{\code{pastWeeksNotIncluded}}{How many time units before first timepoint in \code{range} should the detection begin.}
    \item{\code{c.ARL}}{threshold in the GLR test,
        i.e. \eqn{c_{\gamma}}{c_gamma}}
    \item{\code{Mtilde}}{number of observations needed before we
        have a full rank the typical setup for the
        "\code{intercept}" and "\code{epi}" charts is \code{Mtilde=1}}
    \item{\code{M}}{number of time instances back in time in the
        window-limited approach, i.e. the last value
        considered is \eqn{\max{1,n-M}}. To always look back
        until the first observation use \code{M=-1}.}
    \item{\code{change}}{a string specifying the type of the
        alternative. Currently the two choices are
        \code{intercept} and \code{epi}. See the SFB
        Discussion Paper 500 for details.}
    \item{\code{theta}}{if \code{NULL} then the GLR scheme is
        used. If not \code{NULL} the prespecified value for
        \eqn{\kappa} or \eqn{\lambda} is used in a recursive
        LR scheme, which is faster.  }
    \item{\code{dir}}{a string specifying the direction of testing in
        GLR scheme. With \code{"inc"} only increases in \eqn{x} are
        considered in the GLR-statistic, with \code{"dec"} decreases
        are regarded. }
    \item{\code{ret}}{a string specifying the type of
        \code{upperbound}-statistic that is returned. With
        \code{"cases"} the number of cases that would have been
        necessary to produce an alarm or with \code{"value"} the
        glr-statistic is computed (see below).}  
      }
    }
}
\details{
	The in-control model is initially estimated with these baseline data: last point is the last point before
	the first timepoint in \code{range} - \code{pastWeeksNotIncluded} and first point is the point
	located b years and w time units (weeks, months, days) before the first timepoint in \code{range} - \code{pastWeeksNotIncluded}. 
	After each alarm, baseline data are data between the timepoint of alarm and the point located b years and w time units before
	the first time point after alarm.
	
	The surveillance is done for all timepoints in \code{range} but starts \code{pastWeeksNotIncluded}
	timepoints before the first timepoint of \code{range} so that an alarm could be given for the first
	timepoint in \code{range}.

  The in-control model is estimated this way: following steps are perfomed as in farringtonFlexible (with a sinus-cosinus
  seasonal variable instead of a seasonal factor variable).
\enumerate{
\item Fit of the initial model with intercept, time trend if \code{trend} is \code{TRUE},
seasonal fsinusoidal variable inf \code{sinCos} is \code{TRUE}, and population offset if
\code{populationOffset} is \code{TRUE}. Initial estimation of mean and
  overdispersion.
\item Calculation of the weights omega (correction for past outbreaks) if \code{reweighting} is \code{TRUE}.
The threshold for reweighting is defined in \code{control}.
\item Refitting of the model
\item Revised estimation of overdispersion
\item Omission of the trend, if it is not significant
\item Repetition of the whole procedure
\item Calculation of the expected values and overdispersion for the range of timepoints to be put under surveillance.


}

Then, a moving-window generalized likelihood ratio
  detector is used like in algo.glrnb. After each alarm, the in-control model is re-estimated (with these baseline data: last point is the last point after
	the alarm timepoint and first point is the point
	located b years and w time units (weeks, months, days) before this timepoint), and the detector is used again, until the end of \code{range} is attained.
}
\value{
An object of class \code{sts} with the slots \code{upperbound} and \code{alarm} filled by appropriate output of the algorithm.
The slot \code{control} is the usual list but with more items all of length \code{length(range)}:
\describe{
\item{trend}{ is a vector of Booleans indicating whether a time trend was fitted for this time point.}
\item{trendVector}{ is a vector giving the coefficient of the time trend in the GLM for this time point. If no trend was fitted it is equal to NA.}
\item{expected}{is a vector giving the expectation of the predictive distribution for each timepoint. }
\item{phiVector}{ is a vector giving the overdispersion of the glm at each timepoint.}

}
}

\keyword{classif}

\examples{

### DATA I/O ###
#Read Salmonella Agona data
data("salmonella.agona")

# Create the corresponding sts object from the old disProg object
salm <- disProg2sts(salmonella.agona)
### RUN THE ALGORITHMS ###

testSalm <- glrnbFarrington(sts=salm,control = list( b = 3, w = 3,
                                      reweight = TRUE, 
                                      weightsThreshold = 2.58,
                                      glmWarnings = TRUE,
                                      trend = TRUE,
                                      pThresholdTrend=1, limit54=c(5,4),                                     
                                      fitFun="fitGLM.nb",
                                      populationOffset = FALSE, 
                                      pastWeeksNotIncluded = 26, 
                                      populationOffset = FALSE,
									  sinCos = TRUE,
									  c.ARL = 0.5, Mtilde = 1,
									  M =-1, change="intercept",
									  theta=log(2),dir="inc",
									  ret="cases",
									  referenceRange = NULL))
								  
 plot(testSalm)									 
lines(testSalm@control$expected,col='green')
}
\author{M. Salmon, M. \enc{Höhle}{Hoehle}}
\seealso{\code{\link{algo.glrnb}},\code{\link{farringtonFlexible}}}
\keyword{classif}
\references{A statistical algorithm for the early detection of outbreaks of infectious disease, Farrington, C.P., Andrews, N.J, Beale A.D. and Catchpole, M.A. (1996), J. R. Statist. Soc. A, 159, 547-563.


An improved algorithm for outbreak detection in multiple surveillance systems, Noufaily, A., Enki, D.G., Farrington, C.P., Garthwaite, P., Andrews, N.J., Charlett, A. (2012), Statistics in Medicine, published online.

Count data regression charts for the monitoring of surveillance time
series (2008), M. \enc{Höhle}{Hoehle} and M. Paul, Computational Statistics and
Data Analysis, 52(9), pp. 4357--4368.

Poisson regression charts for the monitoring of surveillance time
series (2006), \enc{Höhle}{Hoehle}, M., SFB386 Discussion Paper 500.
}


