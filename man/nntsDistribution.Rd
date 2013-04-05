\name{nntsDistribution}
\Rdversion{2.1}
\alias{nntsDistribution}
\title{NNTS Distribution function}
\description{Cumulative distribution function in terms of the c parameters at theta, measured in radians [0,2*pi).}
\usage{nntsDistribution(theta, cpars = 1/sqrt(2 * pi), M = 0)}
\arguments{
\item{theta}{Vector of angles in radians at which the distribution is computed}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}}

\value{The function returns the value of the distribution function evaluated at each component of theta}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
ccoef<-nntsrandominitial(3)
nntsDistribution(c(0,pi/2,pi,2*pi-0.00000001,2*pi),ccoef,3)
}

