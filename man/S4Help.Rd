\name{S4Help}
\alias{S4Help}
\title{A function to find help for S4 classes and generics}
\description{
This function takes the name of either a class or a generic function
and finds a set of related manual pages. The user then selects which
manual page they want.
}
\usage{
S4Help(name, signature)
}
\arguments{
  \item{name}{The name of the S4 class or generic function. }
  \item{signature}{ Currently not used. }
}
\details{
For S4 classes, the class and any superclasses are found and the user
can select which manual page they want.  If the supplied name corresponds 
to a generic function, then that function, or any of its methods can be 
selected.
}
\value{
 \code{NULL} is returned, invisibly. The function is called purely for side
effect.
}
\author{R. Gentleman}
\seealso{ \code{\link{help}}}
\examples{
\dontrun{
   S4Help("classRepresentation")
   S4Help("coerce")
}
}
