\encoding{latin1}
\name{twinstim_epidataCS}
\alias{as.epidataCS} 
\alias{epidataCS}
\alias{[.epidataCS}
\alias{print.epidataCS}
\alias{head.epidataCS}
\alias{tail.epidataCS}
\alias{marks.epidataCS}
\title{
  Class for Representing Continuous Space-Time Point Process Data
}

\description{
  Data structure for (c)ontinuous (s)patio-temporal event data, typically
  cases of an infectious disease. The data structure simultaneously
  contains a spatio-temporal grid of endemic covariates and a
  representation of the observation region.

  The \code{"epidataCS"} class is the basis for fitting 
  spatio-temporal epidemic intensity models with the function
  \code{\link{twinstim}}.
}
\usage{
as.epidataCS(events, stgrid, W, qmatrix = diag(nTypes),
             nCircle2Poly = 32L, T = NULL)

\method{print}{epidataCS}(x, n = 6L, digits = getOption("digits"), ...)
\method{head}{epidataCS}(x, n = 6L, ...)
\method{tail}{epidataCS}(x, n = 6L, ...)
\method{[}{epidataCS}(x, i, j)

\method{marks}{epidataCS}(x, ...)
}
\arguments{
  \item{events}{
    \code{SpatialPointsDataFrame} of cases with obligatory columns
    \describe{
      \item{time}{Time point of event.}
      \item{tile}{Reference to the spatial unit (tile) in \code{stgrid},
	where the event is located.}
      \item{type}{optional type of event in a marked \code{twinstim}
	model. Type will be converted to a factor variable.}
      \item{eps.t}{Maximal temporal influence radius (e.g. length of
	infectious period, time to culling, etc.), may be \code{Inf}.}
      \item{eps.s}{Maximal spatial influence radius (e.g. 100 [km]),
      may be \code{Inf}.
      }
    }
    The \code{data.frame} may contain columns with further marks of
    the event, e.g. sex, age of infected individuals (-> epidemic
    covariates).
    However, note that some auxiliary columns will be added at conversion,
    whose names \code{"ID"}, \code{".obsInfLength"}, \code{".bdist"},
    \code{".influenceRegion"}, and \code{".sources"} are reserved.
  }
  \item{stgrid}{
    \code{data.frame} with obligatory columns
    \describe{
      \item{tile}{ID of spatial unit (e.g. id of municipality)}
      \item{start, stop}{temporal interval. The \code{stop} column may
	be missing, in which case it will be auto-generated from the set
	of \code{start} values and \code{T}.}
      \item{area}{area of the spatial unit (\code{tile}).
	Be aware that the unit of this area (e.g., square km) must be consistent
	with the units of \code{W} and \code{events} as specified in
	their \code{\link{proj4string}}s.}
    }
    The remaining columns are endemic covariates.
    However, note that the column name \code{"BLOCK"} is reserved
    (a column which will be added automatically for indexing the time
    intervals of \code{stgrid}).  
  }
  \item{W}{
    An object of class \code{SpatialPolygons} indicating the observation
    region. Must have same \code{proj4string} as \code{events} and all
    events must be within W.  The function \code{\link{simplify.owin}} from the
    package \pkg{spatstat} may be useful if polygonal operations take too long
    or memory is limited.
  }
  \item{qmatrix}{
    square indicator matrix (0/1 or TRUE/FALSE) for possible
    transmission between the event types. The matrix will be internally
    converted to logical. Defaults to an independent spread of the event
    types, i.e. the identity matrix. 
  }
  \item{nCircle2Poly}{
    accuracy (number of edges) of the polygonal approximation of a circle
  }
  \item{T}{
    end of observation period (i.e. last \code{stop} time of
    \code{stgrid}). Must be specified if the start but not the stop
    times are supplied in \code{stgrid} (=> auto-generation of
    \code{stop} times).
  }
  \item{x}{an object of class \code{epidataCS}.}
  \item{n}{a single integer. If positive, the first (\code{head}, \code{print})
    / last (\code{tail}) \code{n} events are extracted. If negative,
    all but the \code{n} first/last events are extracted.
  }
  \item{digits}{minimum number of significant digits to be printed in
    values.}
  \item{\dots}{passed to the basic \code{print} method. Unused for
    \code{marks}.}
  \item{i,j}{
    arguments passed to the
    \code{\link[=[,SpatialPointsDataFrame-method]{[-method}} for
    \code{SpatialPointDataFrame}s for subsetting the \code{events}.
    Only epidemic covariates can be removed from \code{events} by the
    \code{j} index. The other components of \code{x} (\code{stgrid},
    \ldots) are retained.
  }
}
\details{
  The function \code{as.epidataCS} is used to generate objects of class
  \code{"epidataCS"}, which is the data structure that
  \code{\link{twinstim}} models work on.

  The \code{marks.epidataCS} method (of the generic function
  \code{marks} defined in package \pkg{spatstat}) returns a
  \code{data.frame} of the event marks (actually also including time
  and location of the events).
}
\value{
  An object of class \code{epidataCS} containing the following components
  \describe{
    \item{events}{See above description. The added auxiliary columns are:
    \describe{
      \item{ID}{unique chronological ID for the events.}
      \item{.obsInfLength}{observed length of the infectious period
	(being part [0,T])}  
      \item{.bdist}{minimal distance of the event locations to the boundary}
      \item{.influenceRegion}{gpc.poly, which is the intersection of \code{W}
	with a circle of radius \code{eps.s} centered at the event location.} 
      \item{.sources}{potential sources of infection}
    }
    % TODO: more details
    }
    \item{stgrid}{See the description of the argument.}
    \item{W}{Observational region as \code{gpc.poly}.}
    \item{qmatrix}{See the description of the \code{qmatrix} argument.}
    %% ...
  }
}
\references{
  S. Meyer, J. Elias and M. Höhle (2010): A space-time conditional
  intensity model for infectious disease occurence, Department of
  Statistics: Technical Reports, No. 95, 27 pages, 2010.
  Available as http://epub.ub.uni-muenchen.de/11898/
  
  Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
  on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet} München.
  Available as http://epub.ub.uni-muenchen.de/11703/
}
\author{
  Sebastian Meyer, documentation contributions by Michael H\enc{ö}{oe}hle
}
\seealso{
  \code{\link{animate.epidataCS}} for the animation of such an epidemic.
  There are also \code{\link[=summary.epidataCS]{summary}} and
  \code{\link[=update.epidataCS]{update}} methods for the
  \code{"epidataCS"} class.
  Models for \code{"epidataCS"} can be fitted with \code{\link{twinstim}}.
  It is also possible to convert the data to \code{\link{epidata}}
  objects (discrete space) for analysis with \code{\link{twinSIR}}
  (see \code{\link{as.epidata.epidataCS}}).
}
%\examples{
%#No examples are currently available
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{classes}
\keyword{manip}
