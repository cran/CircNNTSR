\name{nntsDistributioninterval0to1}
\Rdversion{2.1}
\alias{nntsDistributioninterval0to1}

\title{NNTS distribution function for the incidence data defined in the interval [0,1)}
\description{Computes the distribution function at theta for the incidence data (number of observed values in certain intervals 
defined in the interval [0,1))}
\usage{nntsDistributioninterval0to1(theta, cpars = 1/sqrt(2 * pi), M = 0)}

\arguments{
\item{theta}{Value at which the distribution function is computed}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the squared moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}
}

\value{The function returns the value of the distribution function at theta}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
cpars<-nntsrandominitial(2)
nntsDistributioninterval0to1(pi, cpars, 2)
}
