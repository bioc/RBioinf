\name{randDNA}
\alias{randDNA}
\title{ A function to generate random DNA sequences. }
\description{
 This function generates random DNA sequences, nucleotides are 
 sampled with frequency 0.25 each.
}
\usage{
randDNA(n)
}
\arguments{
  \item{n}{ The length of the sequence desired.}
}
\details{
 This function generates random sequences of A, C, T and G.
 Real DNA is quite different, so one should not use these sequences
 for much other than pedagogical purposes.
}
\value{
 A length one character vector, with \code{n} characters randomly 
chosen from A, C, T and G.
}
\author{ R. Gentleman }
\examples{
randDNA(100)
}
