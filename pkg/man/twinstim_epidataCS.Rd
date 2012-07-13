\encoding{latin1}
\name{twinstim_epidataCS}
\alias{epidataCS}
\alias{as.epidataCS} 
\alias{print.epidataCS}
\alias{head.epidataCS}
\alias{tail.epidataCS}
\alias{[.epidataCS}
\alias{marks}              % to make R CMD check happy (the generic
                           % marks is imported from spatstat and then
                           % exported (unchanged) -> documentation required)
\alias{marks.epidataCS}
\alias{summary.epidataCS}
\alias{print.summary.epidataCS}

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
\method{[}{epidataCS}(x, i, j, drop = FALSE)

\method{marks}{epidataCS}(x, ...)

\method{summary}{epidataCS}(object, ...)
\method{print}{summary.epidataCS}(x, ...)
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
    However, note that some auxiliary columns will be added at conversion
    whose names are reserved: \code{"ID"}, \code{".obsInfLength"},
    \code{".bdist"}, \code{".influenceRegion"}, and \code{".sources"},
    as well as \code{"start"}, \code{"BLOCK"}, and all endemic
    covariates' names from \code{stgrid}.
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
  \item{x}{an object of class \code{"epidataCS"} or
    \code{"summary.epidataCS"}, respectively.}
  \item{n}{a single integer. If positive, the first (\code{head}, \code{print})
    / last (\code{tail}) \code{n} events are extracted. If negative,
    all but the \code{n} first/last events are extracted.
  }
  \item{digits}{minimum number of significant digits to be printed in
    values.}
  \item{drop, \dots}{unused (arguments of the generics).
  However, the \code{print} method for class \code{"epidataCS"} passes
  \dots to the \code{\link{print.data.frame}} method.}
  \item{i,j}{
    arguments passed to the
    \code{\link[=[,SpatialPointsDataFrame-method]{[-method}} for
    \code{SpatialPointDataFrame}s for subsetting the \code{events}.
    Only epidemic covariates can be removed from \code{events} by the
    \code{j} index. The other components of \code{x} (\code{stgrid},
    \ldots) are retained.}
  \item{object}{an object of class \code{"epidataCS"}.}
}

\details{
  The function \code{as.epidataCS} is used to generate objects of class
  \code{"epidataCS"}, which is the data structure that
  \code{\link{twinstim}} models work on.
  
  The \code{head} and \code{tail} methods subset the epidemic data using
  the extraction method (\code{[}), i.e. they return an object of class
  \code{"epidataCS"}, which only contains (all but) the first/last
  \code{n} events. The extraction method for class \code{"epidataCS"}
  ensures that the subsetted object will be valid, for instance, it
  updates the auxiliary list of potential transmission paths stored
  in the object.
  
  The \code{marks.epidataCS} method (of the generic function
  \code{\link[spatstat]{marks}} defined by the package \pkg{spatstat})
  returns a \code{data.frame} of the event marks (actually also
  including time and location of the events), disregarding the auxiliary
  columns from the \code{events} component of the \code{"epidataCS"}
  object.

  The \code{print} method for \code{"epidataCS"} prints some metadata
  of the epidemic, e.g., the observation period, the dimensions of the
  spatio-temporal grid, the types of events, and the total number of
  events. By default, it also prints the first \code{n = 6} rows of the
  \code{events}.
  The \code{summary} method (or its \code{print} method) instead returns
  the metadata along with a \code{summary} of the \code{events} (by
  column as for \code{data.frame}s), a summary of the number of
  potential sources of transmission for each event, and the
  \code{str}ucture of the \code{counter} element of the \code{summary}
  output, which is a step function of the number of infectious
  individuals over time.
}

\value{
  An object of class \code{epidataCS} contains the following components:
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
  \code{\link{plot.epidataCS}} for plotting, and
  \code{\link{animate.epidataCS}} for the animation of such an epidemic.
  There is also an \code{\link[=update.epidataCS]{update}} method for the
  \code{"epidataCS"} class.
  Models for \code{"epidataCS"} can be fitted with \code{\link{twinstim}}.
  It is also possible to convert the data to \code{\link{epidata}}
  objects (discrete space) for analysis with \code{\link{twinSIR}}
  (see \code{\link{as.epidata.epidataCS}}).
}

\examples{
# TODO: ...
}

\keyword{spatial}
\keyword{classes}
\keyword{manip}
