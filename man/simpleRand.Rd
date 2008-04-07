\name{simpleRand}
\alias{simpleRand}
\title{ A simple interface to C level random number generation. }
\description{
 A simple interface function to R's C level rng code. Primarily a
pedagogical tool to accompany the monograph R for Bioinformatics.
}
\usage{
simpleRand(x, y = "notused")
}
\arguments{
  \item{x}{ The number of random numbers to generate. }
  \item{y}{ Not used in the base implementation, but
   various exercises involve making use of this. }
}
\details{
 An interface, via \code{\link{.Call}} to R's underlying RNG cod.
}
\value{
 The random numbers generated, plus information about the RNG used.
}
\author{ R. Gentleman }

\seealso{ \code{\link{simpleSort}}, \code{\link{.Call}} }
\examples{
 simpleRand(4)
}
\keyword{ manip }
