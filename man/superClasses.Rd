\name{superClasses}
\alias{superClasses}
\title{ Return a list of super classes. }
\description{
This function computes and returns a list of all super classes
given a \code{classRepresentation}.
}
\usage{
superClasses(x)
}
\arguments{
  \item{x}{A \code{classRepresentation} object }
}
\details{
This function needs to be rationalized with \code{superClassNames}.
}
\value{
A list of the super classes.
}
\author{ R. Gentleman }
\seealso{ \code{\link{superClassNames}}}
\examples{
superClassNames(getClass("graphNEL"))
}
