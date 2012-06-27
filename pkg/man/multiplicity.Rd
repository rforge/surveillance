\name{multiplicity}
\alias{multiplicity}
\alias{multiplicity.matrix}
\alias{multiplicity.Spatial}

\title{
Count Number of Instances of Points
}

\description{
  The \code{multiplicity} S3-generic function defined in
  \pkg{surveillance} is intended as a simple auxiliary function, which
  counts the number of instances of each row in a \code{matrix}, or of
  each point in a \code{Spatial} object.
  Indeed, this just computes the distance matrix of the rows (points),
  and then counts the number of zeroes in each row.
}

\usage{
multiplicity(x, ...)

\method{multiplicity}{matrix}(x, ...)
\method{multiplicity}{Spatial}(x, ...)
}

\arguments{
  \item{x}{
    an object, for which a \code{multiplicity} method is defined, e.g.
    a matrix or an object of class \code{"\linkS4class{SpatialPoints}"}.
  }
  \item{\dots}{currently unused.}
}

\value{
  an integer vector containing the number of instances at each point of the object.
}

\seealso{
  Examples of the \code{\link{hagelloch}} data for a specific use of this function.
}

\examples{
foo <- matrix(c(1,2,3,
                2,3,4,
                1,2,3,
                4,5,6), byrow=TRUE, ncol=3)
multiplicity(foo)

# the following function determines the multiplicity of rows of a
# numeric matrix and returns unique rows with appended multiplicity
countunique <- function (matrix) {
    count <- multiplicity(matrix)
    unique(cbind(matrix, count))
}
countunique(foo)
}
\keyword{utilities}
\keyword{spatial}

