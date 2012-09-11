\encoding{latin1}
\name{twinstim_iafplot}
\alias{iafplot}

\title{
  Plot the spatial or temporal interaction function of a twimstim
}

\description{
  The function plots the fitted temporal or (isotropic) spatial
  interaction function of a \code{twinstim} object.
}

\usage{
iafplot(object, which = c("siaf", "tiaf"),
    types = 1:nrow(object$qmatrix), scaled = FALSE,
    conf.type = if (length(pars) > 1) "bootstrap" else "parbounds",
    conf.level = 0.95, conf.B = 999,
    xgrid = 101, col.estimate = rainbow(length(types)), col.conf = col.estimate,
    alpha.B = 0.15, lwd = c(3,1), lty = c(1,2), xlim = NULL, ylim = NULL,
    add = FALSE, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{object}{
    object of class \code{"twinstim"} containing the fitted model.
  }
  \item{which}{
    argument indicating which of the two interaction functions to plot.
    Possible values are \code{"siaf"} (default) for the spatial interaction
    function and \code{"tiaf"} for the temporal interaction function.
  }
  \item{types}{
    integer vector indicating for which event \code{types} 
    the interaction function should be plotted in case of a marked twinstim.
  }
  \item{scaled}{
    logical indicating if the interaction function should be multiplied
    by the epidemic intercept \eqn{exp(\gamma_0)}. This is especially
    useful if different interaction functions are to be compared.
  }
  \item{conf.type}{
    type of confidence interval to produce. If \code{"bootstrap"},
    \code{conf.B} parameter values (vectors) are sampled from the
    asymptotic (multivariate) normal distribution of the ML estimate(s) of the
    interaction function parameters; the interaction function is then
    evaluated on the \code{xgrid} (i.e. temporal or
    spatial distances from the host) for each parameter realization to
    obtain a \code{conf.level} confidence interval at each point of the
    \code{xgrid}. 
    If \code{"parbounds"}, the \code{conf.level} Wald confidence
    intervals for the interaction function parameters are calculated and
    the interaction function is evaluated on the \code{xgrid}
    (distances from the host) for all combinations of the bounds
    of the parameters and the point-wise extremes of those functions are
    plotted. This type of confidence interval is actually only valid in
    the case of only one interaction function parameter, but could also
    be used as a rough indication if bootstrapping takes too long.
  }
  \item{conf.level}{
    the confidence level required. For \code{conf.type = "bootstrap"} it
    may also be specified as \code{NA}, in which case all \code{conf.B}
    bootstrapped functions will be plotted with transparency value given
    by \code{alpha.B}.
  }
  \item{conf.B}{
    number of samples for the \code{"bootstrap"} confidence interval.
  }
  \item{xgrid}{
    either a numeric vector of x-values (distances from the host) where
    to evaluate \code{which}, or a scalar representing the desired number of
    evaluation points in the interval \code{c(0,xlim[2])}.
  }
  \item{col.estimate}{
    vector of colours to use for the function point estimates of the different \code{types}.
  }
  \item{col.conf}{
    vector of colours to use for the confidence intervals of the different \code{types}.
  }
  \item{alpha.B}{
    alpha transparency value (as relative opacity) used for the \code{conf.B}
    bootstrapped interaction functions in case \code{conf.level = NA}
  }
  \item{lwd, lty}{
    numeric vectors of length two specifying the line width and type of point
    estimates (first element) and confidence limits (second element),
    respectively. 
  }
  \item{xlim, ylim}{
    vectors of length two containing the x- and y-axis limit of the
    plot. The default y-axis ranges from 0 to 1 (or to
    \eqn{exp(\gamma_0)}, if \code{scaled}).
    The default x-axis ranges from 0 to the length of the
    observation period (\code{which="tiaf"}) or the length of the
    diagonale of the bounding box of the observation region
    (\code{which="siaf"}), respectively, or to the maximum of
    \code{xgrid} if given as a vector of evaluation points.
  }
  \item{add}{
    add to an existing plot?
  }
  \item{xlab, ylab}{
    labels for the axes with \code{NULL} providing sensible defaults.
  }
  \item{\dots}{
    additional arguments passed to the default \code{plot} method.
  }
}

\value{
  A plot is created -- see e.g. Figure 3(b) in Meyer et al. (2012).
  \code{NULL} is returned (invisibly).
}

\references{
  Meyer, S., Elias, J. and H\enc{ö}{oe}hle, M. (2012):
  A space-time conditional intensity model for invasive meningococcal
  disease occurrence. \emph{Biometrics}, \bold{68}, 607-616.\cr
  DOI-Link: \url{http://dx.doi.org/10.1111/j.1541-0420.2011.01684.x}
}

\author{
  Sebastian Meyer
}

\seealso{
  \code{\link{plot.twinstim}}, which calls this function.
}

\examples{
data(imdepifit)
iafplot(imdepifit, "tiaf", types=1)   # tiaf.constant(), not very exciting
iafplot(imdepifit, "siaf", types=1,   # same for types=2
        xlim=c(0,200), col.estimate=1, lwd=c(2,1))

# bootstrap-CI (used if more than one parameter is affected)
iafplot(imdepifit, "siaf", types=1, scaled=TRUE, xlim=c(0,200), xgrid=500,
        conf.type="bootstrap", col.estimate=2)

\dontshow{
\dontrun{
## FIXME: fit model with 2-parameter Lomax kernel as siaf (this takes a
##        while and does not converge properly...)
data(imdepi)
imdepifit_lomax <- update(imdepifit,
    siaf = siaf.lomax(1),
    optim.args = list(par=c(coef(imdepifit), 0), fixed=1:4,
                      control=list(REPORT=1)),
    nCub = 3, nCub.adaptive = FALSE, model = FALSE, cumCIF = FALSE)
# false convergence
imdepifit_lomax2 <- update(imdepifit_lomax, optim.args=list(hessian=FALSE))
# false convergence
imdepifit_lomax_noepicovars <- update(imdepifit_lomax, epidemic = ~1,
optim.args=list(par=c(coef(imdepifit)[grep("(type|agegrp)",
names(coef(imdepifit)), invert=TRUE)], 0)))
# false convergence: loglik much greater (-6214), f approaches point mass in 0
# fit with step fun while decreasing delta, look at loglik
loglikdelta <- sapply(10^seq(0,-12, by=-2),
  #smaller eps cannot be modelled this way (radius for iR too small)
  function(delta) {
    logLik(update(imdepifit_lomax, data=update(imdepi, eps.s=delta),
    epidemic = ~1, siaf=siaf.constant(),
    optim.args=list(par=coef(imdepifit)[1:5], control=list(trace=0))))
  })
# full convergence: loglik increases with decreasing delta... WHY?
}
}
}

\keyword{hplot}
\keyword{aplot}
