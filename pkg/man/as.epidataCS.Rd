%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Michael Höhle based on the in-code documentation of
%         Sebastian Meyer.
% $Date: 2010-04-23 10:49:46 +0200 (Fri, 23 Apr 2010) $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\encoding{latin1}
\name{twinstim_epidataCS}
\alias{as.epidataCS} 
\alias{as.epidata.epidataCS}
\alias{print.epidataCS}
\alias{[.epidataCS}
\alias{epidataCS}
\alias{update.epidataCS}
\title{
  Class for Representing Space-Time Process Data Continuous in Space and
  Continuous in Time 
}

%\name{as.epidataCS}
%\alias{as.epidataCS}
%\alias{checkEvents}
%\alias{checkstgrid}
%\alias{.influenceRegions}

%- Also NEED an '\alias' for EACH other topic documented here.
%\title{
%  Create objects representing spatio-temporal data for analysis with twinstim.
%}

\description{
  Data structure for (c)ontinuous (s)patio-temporal event data, typically
  cases of an infectious disease. The data structure simultaneously
  contains a spatio-temporal grid of endemic covariates.

  Objects of this class are a collection of specific data frames
  containing the event history of an epidemic together with some
  additional attributes such as a spatio-temporal grid of endemic
  covariates. These objects are the basis for fitting 
  spatio-temporal epidemic intensity models with the function
  \code{\link{twinstim}}. Similarly, the function
  \code{update.epidataCS} allows one to quickly update certain
  parameters of an \code{epidataCS} object without having to reconstruct
  it again.
  
  For \code{epidataCS} objects there exist \code{head}, \code{print},
  \code{summary}, \code{animate} and \code{update} functions.
  It is also possible to convert the data to \code{epidata} objects
  for analysis with \code{twinSIR}.
}
\usage{
as.epidataCS(events, stgrid, W, qmatrix = diag(nTypes), nCircle2Poly = 32L)
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
    the event, e.g. sex, age of infected person (-> epidemic
    covariates).

    Note that the column names "ID", ".obsInfLength", ".bdist",
    ".influenceRegion", and ".sources" are reserved.
    \describe{
      \item{ID}{unique chronological ID for the events.}
      \item{.obsInfLength}{observed length of the infectious period
	(being part [0,T])}  
      \item{.bdist}{minimal distance of the event locations to the boundary}
      \item{.influenceRegion}{gpc.poly, which is the intersection of W
	with b(s,eps.s), centered at s} 
      \item{.sources}{potential sources of infection}
    }
  }
  \item{stgrid}{
    \code{data.frame} with obligatory columns
    \describe{
      \item{tile}{ID of spatial unit (e.g. id of municipality)}
      \item{start, stop}{temporal interval}
      \item{area}{area of the spatial unit (tile)}
    }
    The remaining columns are endemic covariates. Note that the column
    name "BLOCK" is reserved (indexing the time intervals of stgrid). 
  }
  \item{W}{
    An object of class \code{SpatialPolygons} indicating the observation
    region. Must have same \code{proj4string} as \code{events} and all
    events must be within W.  The function \code{\link{simplify.owin}} from the
    package \bold{spatstat} may be useful if polygonal operations take too long
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
}
\details{
  The function is used to generate objects of the class \code{epidataCS}
  which is the data structure that \code{twinstim} models work on.
}
\value{
  An object of class \code{epidataCS} containing th following components
  \describe{
    \item{events}{See above description. Additionally for each event
      possible sources are saved in the attribute \code{.sources}. --> Sebastian!}
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
  Sebastian Meyer, Documentation by Michael H\enc{ö}{oe}hle
}
\note{
  The column name \code{"BLOCK"} is a reserved name.  This column will be 
  added automatically at conversion and the resulting data frame will be 
  sorted by this column and by id.  Also the names \code{"id"}, \code{"start"},
  \code{"stop"}, \code{"event"} and \code{"Revent"} are
  reserved for the respective columns only.
}
\seealso{
See also \code{\link{twinstim}}, ~~~
}
\examples{
#No examples are currently available
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{classes}
\keyword{manip}
