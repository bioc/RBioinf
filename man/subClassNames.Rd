\name{subClassNames}
\alias{subClassNames}
\alias{superClassNames}
\title{ Functions to return the names of either subclasses or superclasses. }
\description{
 Given the name of a S4 class, or a S4 classRepresentation object, these
functions return either the names of the direct subclasses or of the
direct superclasses. 
}
\usage{
subClassNames(x)
superClassNames(x)
}
\arguments{
  \item{x}{ Either the name of a class, or an instance of 
 classRepresentation. }
}
\details{
 If a name is given then \code{getClass} is used to get the 
 class representation object.
}
\value{
 A character vector, listing either the direct subclasses or the 
 direct superclasses, depending on which function was called. 

}
\author{ R. Gentleman }
\seealso{ \code{\link{getClass}} }
\examples{
 subClassNames("matrix")
 superClassNames("matrix")
}
\keyword{ manip }
