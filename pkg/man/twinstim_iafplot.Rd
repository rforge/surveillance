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
iafplot(object, which = c("siaf", "tiaf"), types = NULL,
        scaled = FALSE, log="",
        conf.type = if (length(pars) > 1) "bootstrap" else "parbounds",
        conf.level = 0.95, conf.B = 999, xgrid = 101,
        col.estimate = rainbow(length(types)), col.conf = col.estimate,
        alpha.B = 0.15, lwd = c(3,1), lty = c(1,2),
        verticals = FALSE, do.points = FALSE,
        add = FALSE, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL,
        legend = !add && (length(types) > 1), ...)
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
    the interaction function should be plotted in case of a marked
    twinstim. The default \code{types=NULL} checks if the interaction
    function is type-specific: if so, \code{types=1:nrow(object$qmatrix)}
    is used, otherwise \code{types=1}.
  }
  \item{scaled}{
    logical indicating if the interaction function should be multiplied
    by the epidemic intercept \eqn{exp(\gamma_0)}. This is required
    for the comparison of estimated interaction functions from
    different models.
  }
  \item{log}{a character string passed to \code{\link{plot.default}}
    indicating which axes should be logarithmic.
    If \code{add=TRUE}, \code{log} is set according to
    \code{par("xlog")} and \code{par("ylog")}.}
  \item{conf.type}{
    type of confidence interval to produce.\cr
    If \code{conf.type="bootstrap"}, \code{conf.B} parameter values
    (vectors) are sampled from the asymptotic (multivariate) normal
    distribution of the ML estimate(s) of the interaction function
    parameters; the interaction function is then evaluated on the
    \code{xgrid} (i.e. temporal or spatial distances from the host) for
    each parameter realization to obtain a \code{conf.level} confidence
    interval at each point of the \code{xgrid} (or to plot the
    interaction functions of all bootstrap samples if
    \code{conf.level=NA}).\cr 
    If \code{conf.type="parbounds"}, the \code{conf.level} Wald confidence
    intervals for the interaction function parameters are calculated and
    the interaction function is evaluated on the \code{xgrid}
    (distances from the host) for all combinations of the bounds
    of the parameters and the point-wise extremes of those functions are
    plotted. This type of confidence interval is actually only valid in
    the case of only one interaction function parameter, but could also
    be used as a rough indication if bootstrapping takes too long.\cr
    If \code{conf.type="none"} or \code{NA} or \code{NULL}, no
    confidence interval will be calculated.
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
    evaluation points in the interval \code{c(0,xlim[2])}.\cr
    If the interaction function is a step function
    (\code{\link{siaf.step}} or \code{\link{tiaf.step}}), \code{xgrid}
    is ignored and internally set to \code{c(0, knots)}.
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
  \item{verticals,do.points}{graphical settings for step function
    kernels. These can be logical (as in \code{\link{plot.stepfun}}) or
    lists of graphical parameters.}
  \item{add}{
    add to an existing plot?
  }
  \item{xlim, ylim}{
    vectors of length two containing the x- and y-axis limit of the
    plot. The default y-axis range (\code{ylim=NULL}) is from 0 to 1 (or to
    \eqn{exp(\gamma_0)}, if \code{scaled}).
    The default x-axis (\code{xlim=NULL}) starts at 0, and the upper
    limit is determined as follows (in decreasing order of precedence):
    \itemize{
      \item If \code{xgrid} is a vector of evaluation points, \code{xlim[2]} is
      set to \code{max(xgrid)}.
      \item If the model environment is stored in \code{object}, it is set to
      the unique finite value of \code{eps.t}/\code{eps.s} if that exists.
      \item If the interaction function is a step function with
      \code{maxRange<Inf}, i.e. it drops to 0 at \code{maxRange},
      \code{xlim[2]} is set to \code{maxRange}.
      \item Otherwise, it is set to the length of the observation period
      (\code{which="tiaf"}) or the diagonale length of the bounding box of
      the observation region (\code{which="siaf"}), respectively.
    }
  }
  \item{xlab, ylab}{
    labels for the axes with \code{NULL} providing sensible defaults.
  }
  \item{legend}{
    logical indicating if a legend for the \code{types} should be added.
    It can also be a list of arguments passed to \code{\link{legend}}
    to tweak the default settings.
  }
  \item{\dots}{
    additional arguments passed to the default \code{plot} method.
  }
}

\value{
  A plot is created -- see e.g. Figure 3(b) in Meyer et al. (2012).
  
  The function invisibly returns a matrix of the plotted values of the
  interaction function (evaluated on \code{xgrid}, by type). The first
  column of the matrix contains the distance \eqn{x}, and the remaining
  \code{length(types)} columns contain the (scaled) function values for
  each type.
  
  The pointwise confidence intervals of the interaction functions are
  returned in similar matrices as attributes: if
  \code{length(types)==1}, there is a single attribute \code{"CI"},
  whereas for multiple types, the attributes are named
  \code{paste0("CI.",typeNames)} (where the \code{typeNames} are
  retrieved from \code{object$qmatrix}).
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
data("imdepifit")
iafplot(imdepifit, "tiaf", types=1)   # tiaf.constant(), not very exciting
iafplot(imdepifit, "siaf", types=1,   # same for types=2
        xlim=c(0,200), col.estimate=1, lwd=c(2,1))

# scaled version and bootstrap-CI (used if 'siaf' has more than one parameter)
iafplot(imdepifit, "siaf", types=1, scaled=TRUE, xlim=c(0,200),
        conf.type="bootstrap", col.estimate=2)
}

\keyword{hplot}
\keyword{aplot}
