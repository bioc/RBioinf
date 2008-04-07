\name{simplePVect}
\alias{simplePVect}
\title{ A function to print a vector at the C level. }
\description{
This function takes as input any vector, matrix
or array of numeric values and passes that array out to C, where it is
printed, in order from the first value stored to the last, regardless of
the dimensioning information. And information about the location is
printed as well.
}
\usage{
simplePVect(iV)
}
\arguments{
  \item{iV}{ The input vector. }
}
\details{
As above.
}
\value{
\code{NULL} is returned. The function is called only for its side effects.
}
\author{R. Gentleman }
\examples{
simplePVect(1:3)
}
