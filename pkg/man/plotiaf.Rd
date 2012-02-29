\encoding{latin1}
\name{plotiaf}
\alias{plotiaf}
\title{
  Plot the spatial or temporal interaction function of a twimstim object.
}
\description{
  The function plots the fitted temporal or isotropic spatial
  interaction function of a \code{twinstim} object.
}
\usage{
plotiaf(twinstim, iaf = c("siaf", "tiaf"), types = 1:nrow(twinstim$qmatrix), xlim = c(0, eps), ylim = c(0, 1), cols = rainbow(length(types)), lwd = c(2,1), add = FALSE, ...)
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
    Color to use for the \code{types}.
}
  \item{lwd}{
    Numeric vector of length two specifying the line width of point estimates (first element) and confidence limits (second element), respectively.
}
  \item{add}{
    Add to an existing plots
}
  \item{\dots}{
    Additional arguments sent to the \code{plot} command.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A plot is created -- see e.g. Fig. 5.3 in Meyer (2010). Nothing is returned.
}
\references{
 Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
 on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet}
 München. Available as http://epub.ub.uni-muenchen.de/11703/
}
%\author{
%  Sebastian Meyer
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

