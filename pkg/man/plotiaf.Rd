\encoding{latin1}
\name{plotiaf}
\alias{plotiaf}
\title{
  Plot the spatio or temporal interaction function of a twimstim object.
}
\description{
  The function plots the fitted temporal or isotropic spatial
  interaction function of a \code{twinstim} object.
}
\usage{
plotiaf(twinstim, iaf = c("siaf", "tiaf"), types = 1:nrow(twinstim$qmatrix), xlim = c(0, eps), ylim = c(0, 1), cols = rainbow(length(types)), add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twinstim}{
    Object of class \code{twinstim} containing the fitted twinstim model.
}
  \item{iaf}{
    Argument indicating which of the two interaction functions to plot.
    Possible values are \code{"siaf"} for the spatial interaction
    function and \code{"tiaf"} for the temporal interaction function.
}
  \item{types}{
    If \code{twinstim} is a marked twinstim object, \code{types}
    indicates which interaction functions to plot.
}
  \item{xlim}{
    Vector of length two containing the x-axis limit of the plot.
}
\item{ylim}{
  Vector of length two containing the y-axis limit of the plot.
}
  \item{cols}{
    Color to use for each mark.
}
  \item{add}{
    Add to an existing plots
}
  \item{\dots}{
    Additional arguments sent to the \code{plot} command.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A plot is created -- see e.g. Fig. 5.3 in Meyer (2010). Nothing is returned.
}
\references{
 Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
 on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet}
 München. Available as http://epub.ub.uni-muenchen.de/11703/
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (twinstim, iaf = c("siaf", "tiaf"),
    types = 1:nrow(twinstim$qmatrix), xlim = c(0,eps), ylim = c(0,1),
    cols = rainbow(length(types)), add = FALSE, ...)
{
    iaf <- match.arg(iaf)
    eps <- twinstim$medianeps[if (iaf=="siaf") "spatial" else "temporal"]
    FUN <- twinstim$formula[[iaf]][[if (iaf=="siaf") "f" else "g"]]
    coefs <- coef(twinstim)
    idxpars <- grep(iaf,names(coefs))
    pars <- coefs[idxpars]
    cis <- confint(twinstim, idxpars)
    if (!add) plot(xlim, ylim, type="n", ...)
    for (i in seq_along(types)) {
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, pars, types[i]), add=TRUE, col=cols[i])
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, cis[,1], types[i]), add = TRUE, lty=2, col=cols[i])
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, cis[,2], types[i]), add = TRUE, lty=2, col=cols[i])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
