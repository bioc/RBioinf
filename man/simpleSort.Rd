\name{simpleSort}
\alias{simpleSort}
\title{ A simple interface to sorting routines in R }
\description{
 The function demonstrates how to access R's internal sorting routines
 via the \code{\link{.Call}} interface.
}
\usage{
simpleSort(x)
}
\arguments{
  \item{x}{ The vector to be sorted. }
}
\details{
 A simple interface to sorting routines in R. It is intended to be modified
following exercises in the accompanying monograph.
}
\value{
 The sorted vector.
}
\author{ R. Gentleman }
\seealso{ \code{\link{simpleRand}}, \code{\link{.Call}}}
\examples{
 simpleSort(c(4,2,6))
}
\keyword{ manip }
