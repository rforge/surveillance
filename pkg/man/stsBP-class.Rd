\name{stsBP-class}
\Rdversion{1.1}
\docType{class}
\alias{stsBP-class}
\alias{coerce,sts,stsBP-method}

\title{Class \code{"stsBP"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("stsBP", epoch, start, freq, observed, state, map, neighbourhood, populationFrac, alarm, upperbound, control, epochAsDate, multinomialTS, ci, lambda)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ci}:}{Object of class \code{"array"} ~~ }
    \item{\code{lambda}:}{Object of class \code{"array"} ~~ }
    \item{\code{epoch}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{freq}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{start}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{observed}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{state}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{alarm}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{upperbound}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{neighbourhood}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{populationFrac}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{map}:}{Object of class \code{"SpatialPolygonsDataFrame"} ~~ }
    \item{\code{control}:}{Object of class \code{"list"} ~~ }
    \item{\code{epochAsDate}:}{Object of class \code{"logical"} ~~ }
    \item{\code{multinomialTS}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{sts}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "sts", to = "stsBP")}: ... }
	 }
}
%\references{
%%  ~~put references to the literature/web site here~~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("stsBP")
}
\keyword{classes}
