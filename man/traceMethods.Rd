\name{traceMethods}
\alias{traceMethods}
\title{ A function to turn on tracing for all methods of a S4 generic
 function.}
\description{
 This function can turn on tracing for all methods (or a subset of
the methods) of a generic function.  It is useful when debugging,
as it can help see how the methods are being traversed.
}
\usage{
traceMethods(generic, traceStrings)
}
\arguments{
  \item{generic}{ The name of the generic function, quoted or not. }
  \item{traceStrings}{ A string to print when each method is entered. }
}
\details{
 Currently it simply traces using a print methods.  Ideally this will
be expanded to support different tracers, such as \code{browser}.
}
\value{
  A vector of method signatures is returned.  This could be then 
used to untrace the methods (something else to automate).
}
\author{ R. Gentleman }
\seealso{ \code{\link{showMethods}}, \code{\link{trace}} }
\examples{
##none right now
\dontrun{
   traceMethods{slice}
}
}
