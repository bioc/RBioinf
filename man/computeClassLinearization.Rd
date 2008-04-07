\name{computeClassLinearization}
\alias{computeClassLinearization}
\alias{LPO}
\title{Compute the Class Linearization }
\description{
There are a number of different methods for computing the class linearization.
The methods described here are discussed in more detail in the vignette
for this package. \code{LPO} and \code{computeClassLinearization} are 
aliases, the former being easier to type, the latter more descriptive.
}
\usage{
computeClassLinearization(inClass, C3 = FALSE)
LPO(inClass, C3 = FALSE)
}
\arguments{
  \item{inClass}{ The class for which a linearization of its  is wanted. }
  \item{C3}{ Whether or not to use the C3 method in computing the linearization.}
}
\details{
For many different computations, especially method dispatch, an algorithm
for specifying a linear order of the class inheritance tree is needed.
All object oriented programming languages support the computation of
a linearization.  Local precedence order (LPO) uses the order of
the direct superclasses, given in the class definition, in computing the
linearization, with earlier superclasses considered more specific than
later ones.  If there are no duplicate class labels in the hierarchy
then this is then simply a bread-first search of the superclass
definitions. But when one or more classes are inherited from different
superclasses this definition becomes more complicated, and can in
fact not be satisfied.
}
\value{
A vector with the class linearization, the.
}
\references{Monotonic Superclass Linearization for Dylan, 
K. Barrett and others, 1996, OOPSLA}
\author{ R. Gentleman }
\examples{
setClass("object")
setClass("grid-layout", contains="object")
setClass("horizontal-grid", contains="grid-layout")
setClass("vertical-grid", contains="grid-layout")
setClass("hv-grid", contains=c("horizontal-grid", "vertical-grid"))
LPO("hv-grid")
}
