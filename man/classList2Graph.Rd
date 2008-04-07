\name{classList2Graph}
\alias{classList2Graph}
\alias{class2Graph}
\title{Functions to produce graphs from S4 class definitions }
\description{
  Given either a list of classes, or a single class, these functions
  produce a graph, with the classes as nodes and edges representing
  subclass/superclass relationships.
}
\usage{
classList2Graph(class, fullNames=TRUE)
class2Graph(class, fullNames=TRUE)
}
\arguments{
  \item{class}{Either the name of a single class, \code{class2Graph}, or the
    names of classes, \code{classList2Graph} }
  \item{fullNames}{Indicates whether to use fully qualified (by package)
    names for the class.}
}
\details{
   Edges are directed, and go from superclasses to subclasses (more
   specialized to less specialized).
}
\value{
A graph, typically an instance of the \code{graphNEL} class.
}
\author{R. Gentleman }
\examples{
 graphClasses = getClasses("package:graph")
 classList2Graph(graphClasses)
}
