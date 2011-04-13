\name{twinstim_epidataCS}
\alias{as.epidataCS}
\alias{print.epidataCS}
\alias{[.epidataCS}
\alias{epidataCS}

\title{
Class for Epidemic Data Continuous in Space and Continuous in Time
}

\description{ The function \code{as.epidataCS} converts a matrix or a
  data frame into an object of \code{\link{class}} \code{"epidataCS"}.
  Objects of this class are a collection of specific data frames
  containing the event history of an epidemic together with some
  additional attributes.  These objects are the basis for fitting
  spatio-temporal epidemic intensity models with the function
  \code{\link{twinstim}}. Similarly, the function
  \code{update.epidataCS} allows one to quickly update certain
  parameters of an \code{epidataCS} object without having to reconstruct
  it again.
}

\usage{
as.epidataCS(events, stgrid, W, qmatrix = diag(nTypes), nCircle2Poly = 32)
update.epidataCS(object, eps.t, eps.s, qmatrix, nCircle2Poly, ...)
}

\arguments{
  \item{events}{\code{SpatialPointsDataFrame} of event locations with the
    following obligatory columns
    \describe{
      \item{time}{time point of event}
      \item{tile}{reference to spatial unit (tile) in stgrid, where the
	event is located}
      \item{type}{optional type of event (-> marked twinstim). will be
	converted to a factor variable.}
      \code{eps.t}{maximal temporal influence radius (e.g. length of
	infectious period, time to culling, etc.), may be \code{Inf}}
      \code{eps.s}{maximal spatial influence radius (e.g. 100 [km]), may
	be Inf}
    }
    The remaining columns are further marks of the event, e.g. sex, age of
    infected person (i.e. epidemic covariates) . The column names
    \code{"ID"}, \code{".obsInfLength"}, \code{".bdist"},
    \code{".influenceRegion"}, and \code{".sources"} are reserved:
    \describe{
      \item{"ID"}{unique chronological ID for the events}
      \item{".obsInfLength"}{observed length of the infectious period (being part [0,T])}
      \item{".bdist"}{minimal distance of the event locations to the boundary}
      \item{".influenceRegion"}{object of class "owin", the intersection of
	\code{W} with b(s,eps.s), with origin ats}
      \item{".sources"}{d potential sources of infection}
    }
  }
  \item{stgrid}{\code{data.frame} with obligatory columns
    \describe{
      \item{tile}{ID of spatial unit (e.g. id of municipality)}
      \item{start, stop}{temporal interval}
      \item{area}{area of the spatial unit (tile)}
      The remaining columns are endemic covariates. The column name
      \code{"BLOCK"} is reserved (indexing the time intervals of stgrid).
    }
  }
  \item{W}{SpatialPolygons. Observation region. Must have same
    proj4string as events. 
  }
  \item{qmatrix}{square indicator matrix (0/1 or TRUE/FALSE) for possible
  transmission between the event types. will be internally converted to
  logical. Defaults to an independent spread of the event types. 
  }
  \item{nCircle2Poly}{
  }
  \item{object}{
  }
  \item{eps.t}{
  }
  \item{eps.s}{
  }
}

\details{ 
}

\note{
  The column name \code{"BLOCK"} is a reserved name.  This column will be 
  added automatically at conversion and the resulting data frame will be 
  sorted by this column and by id.  Also the names \code{"id"}, \code{"start"},
  \code{"stop"}, \code{"event"} and \code{"Revent"} are
  reserved for the respective columns only.
}

\value{
}

\author{
Sebastian Meyer, Documentation by Michael H\enc{ö}{oe}hle
}

\seealso{
}

\examples{
}
}

\keyword{spatial}
\keyword{classes}
\keyword{manip}
