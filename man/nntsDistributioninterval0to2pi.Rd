\name{nntsDistributioninterval0to2pi}
\Rdversion{2.1}
\alias{nntsDistributioninterval0to2pi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS distribution function for data defined in the interval [0,2*pi)}
\description{Computes the distribution function for the data at theta}
\usage{nntsDistributioninterval0to2pi(theta, cpars = 1/sqrt(2 * pi), M = 0)}

\arguments{
  \item{theta}{Value at which the distribution function is computed}
  \item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive 
number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
  \item{M}{Number of components in the NNTS}
}

\value{The function returns the Value of distribution function at theta}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
cpars<-nntsrandominitial(3)
nntsDistributioninterval0to2pi(0, cpars, 3)
nntsDistributioninterval0to2pi(pi, cpars, 3)
nntsDistributioninterval0to2pi(2*pi-0.00000001, cpars, 3)
nntsDistributioninterval0to2pi(2*pi, cpars, 3)
nntsDistributioninterval0to2pi(3*pi, cpars, 3) }
