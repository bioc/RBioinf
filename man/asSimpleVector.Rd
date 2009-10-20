\name{asSimpleVector}
\alias{asSimpleVector}
\alias{subsetAsCharacter}
\alias{setVNames}
\alias{convertMode}
\title{ Example functions for the Chapter on Debugging }
\description{
These functions are used to demonstrate some of the debugging
facilities in R.
}
\usage{
asSimpleVector(x, mode = "logical")
convertMode(from, to)
setVNames(x, nm)
subsetAsCharacter(x, i, j)
}
\arguments{
  \item{x}{ input }
  \item{mode}{ the mode of \code{x} }
  \item{from}{a parameter}
  \item{to}{another parameter}
  \item{nm}{names for \code{x}}
  \item{i}{an index}
  \item{j}{another index} 
}
\details{
 A set of functions that can be used to demonstrate debugging principles 
and practices.

 \code{asSimpleVector}  converts the argument \code{x} to a simple R
vector of the given mode preserving names, dimension and dimnames.

 \code{subsetAsCharacter} calculates either a vector or a matrix subset of the
argument \code{x} and returns the subset after converting it to be of
mode character. It uses \code{asSimpleVector} to do the conversion to
character and thus also preserves any names, dimension or dimnames in the
subset.

 \code{setVNames} sets the names of the given vector \code{x} to
the argument \code{nm} and then converts \code{x} to numeric
using \code{asSimpleVector}.

 \code{convertMode} converts its first argument to
the mode of the second argument.
}

\value{
 Various values are returned.
}
\author{ S. DebRoy} 

\seealso{ \code{\link{browser}} }
\examples{
  asSimpleVector(list(a = 1, b = 2), "character")
}
\keyword{ manip}
