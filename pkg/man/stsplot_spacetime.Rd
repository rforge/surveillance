\name{stsplot_spacetime}
\alias{stsplot_spacetime}
\title{
  Map of Disease Incidence
}

\description{
  For each period (row) or for the overall period of the
  \code{observed} matrix of the \code{"\linkS4class{sts}"} object, a map
  showing the counts by region is produced.
  It is possible to redirect the output into files, e.g. to generate an
  animated GIF.
}

\usage{
stsplot_spacetime(x, type, legend = NULL, opts.col = NULL, labels = TRUE,
                  wait.ms = 250, cex.lab = 0.7, verbose = FALSE,
                  dev.printer = NULL, ...)
}

\arguments{
  \item{x}{
    an object of class \code{"\linkS4class{sts}"}.
  }
  \item{type}{
    a formula (see \code{\link{stsplot}}). For a map aggregated over
    time (no animation), use \code{observed ~ 1 | unit}, otherwise
    \code{observed ~ 1 | unit * time}.
  }
%%   \item{legend}{
%% }
%%   \item{opts.col}{
%% }
%%   \item{labels}{
%% }
%%   \item{wait.ms}{
%% }
%%   \item{cex.lab}{
%% }
%%   \item{verbose}{
%% }
%%   \item{dev.printer}{
%% }
%%   \item{\dots}{
%% }
}

\author{
  Michael H\enc{ö}{oe}hle
}

\note{
  The \code{\link{animate.sts}} method provides a
  re-implementation and supersedes this function!
}

\seealso{
  Other \code{\link{stsplot}} types, and \code{\link{animate.sts}} for
  the new implementation.
}

\examples{
data("ha.sts")
print(ha.sts)

## map of total counts by district
plot(ha.sts, type=observed ~ 1 | unit)
## only show a sub-period total for two selected districts
plot(ha.sts[1:20,1:2], type=observed ~ 1 | unit)

\dontrun{
# space-time animation
plot(aggregate(ha.sts,nfreq=13), type= observed ~ 1 | unit * time)

#Configure a png device printer to save the frames
dev.printer <- list(device=png, extension=".png", width=640, height=480,
                    name=file.path(tempdir(),"berlin"))

#Do the animation (without extra sleeping time between frames)
plot(aggregate(ha.sts,nfreq=13), type = observed ~ 1 | unit * time,
     wait.ms=0, dev.printer=dev.printer)

#Use ImageMagick (you might have to adjust the path to 'convert')
system(paste("convert -delay 50 ",dev.printer$name,
           "*.png ", dev.printer$name, "-animated.gif",sep=""))
}
}

\keyword{hplot}
\keyword{dynamic}
\keyword{spatial}
