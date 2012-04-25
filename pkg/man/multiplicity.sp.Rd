\name{multiplicity.sp}
\alias{multiplicity.sp}
\title{
Count Number of Instances for Each Location of a \code{SpatialPoints} Object
}
\description{
This simple auxiliary function just extracts the \code{\link[sp]{coordinates}}
of the object, computes the distance matrix, and then counts the numbers of
zeroes in each row.
}
\usage{
multiplicity.sp(x)
}
\arguments{
  \item{x}{
    an object, for which the \code{\link[sp]{coordinates}} method
    returns a coordinate matrix, typically an object of class
    \code{"\linkS4class{SpatialPoints}"}.
}
}
\value{
an integer vector containing the number of instances at each point of the object.
}
\seealso{
examples of the \code{\link{hagelloch}} data for a specific use of this function.
}
\keyword{utilities}
\keyword{spatial}

