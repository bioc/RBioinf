\name{Rcal}
\alias{Rcal}
\title{ A function to print a calendar. }
\description{
 A function that prints the requested monthly calendar. The function
 relies on \code{pipe} and the Unix routine \code{cal}.
}
\usage{
Rcal(month, year)
}
\arguments{
  \item{month}{ An optional argument, if supplied a number between 1 and 12. }
  \item{year}{ An optional argument, if supplied a year. }
}
\details{
 By default this prints the calendar for the current month and year. Otherwise
 if a month and year are given and then the calendar for that month and year
 is printed.
}
\value{
  No value is returned.
}
\author{ R. Gentleman }

\seealso{ \code{\link{pipe}} }
\examples{
  if (.Platform$OS.type != "windows") {
      Rcal()
      Rcal(6, 1970)
  }
}
\keyword{ manip }
