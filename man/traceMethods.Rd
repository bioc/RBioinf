\name{traceMethods}
\alias{traceMethods}
\alias{untraceMethods}
\title{ A function to turn on tracing for all methods of a S4 generic
 function.}
\description{
 This function can turn on tracing for all methods (or a subset of
the methods) of a generic function.  It is useful when debugging,
as it can help see how the methods are being traversed.
}
\usage{
traceMethods(generic, traceStrings, tracer)
untraceMethods(generic, methodSigs)
}
\arguments{
  \item{generic}{ The name of the generic function, quoted or not. }
  \item{traceStrings}{ A string to print when each method is entered. }
  \item{tracer}{ A function to insert as the tracer, if missing a function
      that prints the methods signature is used.}
  \item{methodSigs}{A set of method signatures, as a character vector, 
    that tracing will be turned off for.}
}
\details{
  \code{traceMethods} uses \code{showMethods} to figure out what methods 
exist, and what the signatures are. It then uses \code{trace} to set a trace on
 all methods.

  \code{untraceMethods} uses the returned value of \code{traceMethods},
or any other similar construct to untrace methods for a generic.
}
\value{
  A vector of method signatures is returned.  This could be then 
used to untrace the methods (something else to automate).
}
\author{ R. Gentleman }
\seealso{ \code{\link{showMethods}}, \code{\link{trace}} }
\examples{
\dontrun{
   traceMethods{slice}
   untraceMethods{slice}
}
}
