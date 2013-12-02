\name{plot.ah4}
\alias{plot.ah4}
\alias{plotHHH4_fitted}
\alias{plotHHH4_fitted1}
\alias{plotHHH4_season}
\alias{plotHHH4_ri}

\title{Plot Method for Fitted \code{hhh4}-models}

\description{
  There are three \code{type}s of plots for fitted \code{\link{hhh4}} models:
  \itemize{
    \item Plot the \code{"fitted"} component means (of selected units)
    along time along with the observed counts.
    \item Plot the estimated \code{"season"}ality of the three
    components and the dominant eigenvalue of the epidemic components.
    \item If the units of the corresponding multivariate
    \code{"\linkS4class{sts}"} object represent different regions, a map
    of estimated region-specific intercepts (\code{"ri"}) of a selected
    model component can be produced.
  }
}

\usage{
\method{plot}{ah4}(x, type=c("fitted", "season", "ri"), ...)

plotHHH4_fitted(x, units = 1, names = NULL,
                col = c("orange", "blue", "grey85", "black"),
                pch = 19, pt.cex = 0.6,
                par.settings = list(),
                legend = TRUE, legend.args = list(),
                legend.observed = TRUE, ...)

plotHHH4_fitted1(x, unit = 1, main = NULL,
                 col = c("grey30", "grey60", "grey85", "grey0"),
                 pch = 19, pt.cex = 0.6, border = col, 
                 start = x$stsObj@start, end = NULL,
                 ylim = NULL, xlab = "", ylab = "No. infected",
                 hide0s = FALSE, meanHHH = NULL)

plotHHH4_season(...,
                components=c("ar", "ne", "end", "maxEV"),
                xlim=NULL, ylim=NULL,
                xlab=NULL, ylab=NULL, main=NULL,
                par.settings = list(), matplot.args = list(),
                legend = NULL, legend.args = list(),
                refline.args = list(), startseason = 0)

plotHHH4_ri(x, component, sp.layout = NULL,
            gpar.missing = list(col = "darkgrey", lty = 2, lwd = 2),
            ...)
}

\arguments{
  \item{x}{a fitted \code{\link{hhh4}} object.}
  \item{type}{type of plot: either \code{"fitted"} component means of
    selected \code{units} along time along with the observed counts, or
    \code{"season"}ality plots of the model components and the epidemic
    dominant eigenvalue, or a map of estimated region-specific random
    intercepts (\code{"ri"}) of a specific model \code{component}.
    The latter requires the model \code{x} to contain the selected
    random intercepts, and \code{x$stsObj} to contain a map.}
  \item{\dots}{for \code{plotHHH4_season}, one or more fitted
    \code{\link{hhh4}} objects.
    Otherwise further arguments passed on to other functions.\cr
    For the \code{plot}-method they go to the specific plot
    \code{type} function.\cr
    \code{plotHHH4_fitted} passes them to \code{plotHHH4_fitted1}, which is called
    sequentially for every unit in \code{units}.\cr
    \code{plotHHH4_ri} passes additional arguments to \code{\link{spplot}}.}
  \item{units,unit}{integer or character vector specifying the units to plot.
    It indexes \code{colnames(x$stsObj)}. The \code{unit} for
    \code{plotHHH4_fitted1} must be of length 1 only.}
  \item{names,main}{main title(s) for the selected
    \code{unit}(\code{s}) / \code{components}. If \code{NULL} (default),
    \code{plotHHH4_fitted1} will use the appropriate element of
    \code{colnames(x$stsObj)}, whereas \code{plotHHH4_season} uses
    default titles.}
  \item{col,border}{length 4 vector specifying the colors for the
    spatio-temporal, autoregressive, endemic, and observed elements in
    this order. For \code{plotHHH4_fitted}, a length 3 vector also
    works, in which case the default \code{"black"} will be used for the
    dots of observed counts.}
  \item{pch,pt.cex}{style specifications for the dots drawn to represent
    the observed counts.}
  \item{par.settings}{list of graphical parameters for
    \code{\link{par}}. Sensible defaults for \code{mfrow}, \code{mar} and
    \code{las} will be applied unless overridden or
    \code{!is.list(par.settings)}.}
  \item{legend}{Integer vector specifying in which of the
    \code{length(units)} frames the legend should be drawn. If a logical
    vector is supplied, \code{which(legend)} determines the frame
    selection, i.e., the default is to drawn the legend in the first
    (upper left) frame only, and \code{legend=FALSE} results in no
    legend being drawn.}
  \item{legend.args}{list of arguments for \code{\link{legend}}, e.g.,
    to modify the default positioning
    \code{list(x="topright", inset=0.02)}.}
  \item{legend.observed}{logical indicating if the legend should contain
    a line for the dots corresponding to observed counts.}
  \item{start,end}{time range to plot specified by vectors of length two
    in the form \code{c(year,number)}, see \code{"\linkS4class{sts}"}.}
  \item{xlim}{numeric vector of length 2 specifying the time-of-year
    range, default to \code{c(1,x$stsObj@freq)}.}
  \item{ylim}{y-axis range.
    For \code{type="fitted"}, this defaults to 
    \code{c(0,max(observed(x$stsObj)[,unit]))}.
    For \code{type="season"}, \code{ylim} must be a list of length
    \code{length(components)} specifying the range for every component
    plot, or a named list to customize only a subset of these. If only
    one \code{ylim} is specified, it will be recycled for all
    \code{components} plots.}
  \item{xlab,ylab}{axis labels. For \code{plotHHH4_season}, \code{ylab}
    specifies the y-axis labels for all \code{components} plots in a
    list (similar to \code{ylim}).}
  \item{hide0s}{logical indicating if dots for zero observed counts
    should be omitted. Especially useful if there are too many.}
  \item{meanHHH}{(internal) use different component means than those
    estimated and available from \code{x}.}
  \item{components}{vector of components for which to plot estimated
    seasonality. Additionally, a seasonality plot of the dominant
    eigenvalue of the epidemic components is available
    (\code{"maxEV"}).}
  \item{matplot.args}{list of line style specifications passed to
    \code{\link{matplot}}, e.g., \code{lty}, \code{lwd}, \code{col}.}
  \item{refline.args}{list of line style specifications passed to
    \code{\link{abline}} to draw the reference line in the plot of
    the dominant eigenvalue, e.g., \code{lty} and \code{col}.}
  \item{startseason}{the time point \eqn{t} of the start of a season,
    usually this is \code{$control$data$t[1] = 0} and represents
    \code{$stsObj@start}.}
  \item{component}{component for which to plot the estimated
    region-specific random intercepts. Must partially match one of
    \code{colnames(ranef(x, tomatrix=TRUE))}.}
  \item{sp.layout}{see \code{\link{spplot}}.}
  \item{gpar.missing}{list of graphical parameters applied to regions
    with missing random intercepts (i.e., not included in the model).
    Supplementary regions won't be plotted at all if
    \code{!is.list(gpar.missing)}.}
}

\value{
  \code{plotHHH4_fitted1} invisibly returns a matrix of the fitted
  component means for the selected \code{unit}, and \code{plotHHH4_fitted}
  returns these in a list for all \code{units}.\cr
  \code{plotHHH4_season} invisibly returns the plotted y-values, i.e. the
  multiplicative seasonality effect within each of \code{components}.
  Note that this will include the intercept, i.e. the point estimate of
  \eqn{exp(intercept + seasonality)} is plotted and returned.\cr
  \code{plotHHH4_ri} returns the generated \code{\link{spplot}}, which
  is a \pkg{lattice} plot of class \code{"\link[=trellis.object]{trellis}"}.
}

\author{
  Sebastian Meyer
}

\seealso{
  other methods for \code{hhh4} fits, e.g., \code{\link{summary.ah4}}.
}

\examples{
data("measlesWeserEms")

## fit a simple hhh4 model
measlesModel <- list(
    ar = list(f = ~ 1),
    end = list(f = addSeason2formula(~0 + ri(type="iid"), S=1, period=52),
               offset = population(measlesWeserEms)),
    family = "NegBin1"
    )
measlesFit <- hhh4(measlesWeserEms, measlesModel)

## fitted values for a single unit
plot(measlesFit, units=2)

## seasonality plot
plot(measlesFit, type="season")

## random intercepts of the endemic component
plot(measlesFit, type="ri", component="end")

## fitted values for the 6 regions with most cases and some customization
bigunits <- tail(names(sort(colSums(observed(measlesWeserEms)))), 6)
plot(measlesFit, units=bigunits,
     names=measlesWeserEms@map@data[bigunits,"GEN"],
     legend=5, legend.args=list(x="top"), xlab="Time (weekly)",
     hide0s=TRUE, ylim=c(0,max(observed(measlesWeserEms)[,bigunits])),
     start=c(2002,1), end=c(2002,26), par.settings=list(xaxs="i"))
}

\keyword{hplot}
