\name{isoWeekYear}
\alias{isoWeekYear}
\alias{formatDate}  % FIXME: missing usage entry

\title{Find ISO week and ISO year of Date objects}
\description{
  The function \code{isoWeekYear} extracts the year and week of a
  \code{Date} according to the ISO 8601 specification. It simply
  calls \code{\link{strftime}} with format strings \code{"\%G"}
  and \code{"\%V"}, respectively.

  A small internal wrapper for \code{format.Date} (called
  \code{formatDate}) thus directs all calls having one of these format
  strings to this function, if the \code{.Platform$OS.type}
  information reveals a Windows system. The function also provides three
  additional \code{strptime} formatting strings: "\%Q" (the quarter of a
  date as a numeric), "\%OQ" (the quarter of a date in roman) and "\%q"
  (day within the quarter). These are, e.g., used by \code{linelist2sts}.
}
\usage{
isoWeekYear(Y, M=NULL, D=NULL)
}
\arguments{
  \item{Y}{Date object (POSIX) or the year. Can be a vector.}
  \item{M}{month, \code{NULL} if Y is a Date object)}
  \item{D}{day, \code{NULL} if Y is a Date object)}
}
\value{
  A list with entries \code{ISOYear} and \code{ISOWeek} containing the
  corresponding results.
}
\examples{
dates <- as.Date(c("2002-12-31","2003-01-01","2003-01-06"))
isoWeekYear(dates)
}

\keyword{chron}
