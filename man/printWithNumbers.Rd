\name{printWithNumbers}
\alias{printWithNumbers}
\title{ Print a function with line numbers }
\description{
  A function to print a function together with relevant line
  numbers. These can be used to decide where to set \code{trace}
  functions etc.
}
\usage{
printWithNumbers(f)
}
\arguments{
  \item{f}{ Any R function. }
}
\details{
 The function is printed, all lines that correspond to potential break
 points in the code are numbered.
}
\value{
 The function is invoked primarily for its side effect; the printing of
 the function with line numbers. However, the character vector is
 returned and could be used as input for other tools.
}
\author{R. Gentleman }

\seealso{ \code{\link{trace}},  \code{\link{capture.output}} }
\examples{
 printWithNumbers(convertMode)
}
\keyword{manip}

