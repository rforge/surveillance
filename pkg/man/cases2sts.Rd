\encoding{latin1}
\name{cases2sts}
\alias{cases2sts}
%Internal functions

\title{
  Convert case information based on dates to an aggregated time series
}

\description{
  The function is used to convert an individual line list of cases to an
  aggregated time series based on date information of the cases.
}

\usage{
cases2sts(cases,dateCol,aggregate.by="1 week",dRange=NULL,
                      startYearFormat=switch(aggregate.by,"1 day"="%V","7 day"="%V","1 week"="%V","1 month"="%Y"),
                      startEpochFormat=switch(aggregate.by,"1 day"="%j","7 day"="%V","1 week"="%V","1 month"="%d")
)
}

\arguments{
  \item{cases}{
    A \code{data.frame} containing the line list of cases.
  }
  \item{dateCol}{A character string stating the column name in
    \code{cases} which contain the case data which are to be
    temporally aggregated.
  }
  \item{aggregate.by}{Temporal aggregation level given as a string, see
  the \code{by} variable of the \link{seq.date} function for further details. 
  }
  \item{dRange}{A vector containing the minimum and maximum data to
    use. If not specified these dates are extracted automatically by
    taking \code{range(D[,dateCol])}.
  }
  \item{startYearFormat}{Strptime compatible format string to use for
    determining how the date string is generated. Usually the provided
  options are sufficient.}
  \item{statEpochFormat}{Strptime compatible format string to use for
    determining how the date string is generated. Usually the provided
  options are sufficient.}
}

\details{
  In case aggregation occurs by week the date range is automatically
  extended such that the starting and ending dates are mondays. This
  might not be an appropriate way in all situations this function is
  to be used.
}

\value{
  The function returns an object of \code{\link{sts}}. The \code{freq}
  slot might not be appropriate.
}

\author{
Michael \enc{Höhle}{Hoehle}
}

\note{
  This implementation is still experimental.
}

\seealso{
  See also \code{\link{seq.date}}.
}

\examples{
#Load simulated outbreak data. 
D <- read.table("http://www.stat.uni-muenchen.de/~hoehle/teaching/moid2011/tutorials/cast-backnow/outbreak.txt",header=TRUE,colClasses=c("integer",rep("Date",3)))

debug("cases2sts")
sts <- cases2sts(D, dateCol="dOnset", aggregate.by="1 day")
plot(sts,xaxis.years=FALSE,legend.opts=NULL)

}

\keyword{models}
\keyword{optimize}
