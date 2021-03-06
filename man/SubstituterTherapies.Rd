% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Parser.R
\name{SubstituterTherapies}
\alias{SubstituterTherapies}
\title{Substitutes equations into eachother to find the therapeutic sets}
\usage{
SubstituterTherapies(eqns)
}
\arguments{
\item{eqns}{a string with equations delimited by '\n'}
}
\value{
a vector of string outputs
}
\description{
Substitutes equations into eachother to find the therapeutic sets
}
\details{
substitutes in equations into eachother the number of times that there are equations.
Then, the terms involving defined terms are removed and the resulting minimal ones are returned.
}
\examples{
SubstituterTherapies("C = !A&B\\nB = D&C+E&A\\nA = F+C")
SubstituterTherapies("P = P&!C + A")
}

