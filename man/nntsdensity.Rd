\name{nntsdensity}
\Rdversion{2.1}
\alias{nntsdensity}
\title{NNTS density function}
\description{Density function for the NNTS model}
\usage{nntsdensity(data, cpars = 1/sqrt(2 * pi), M = 0)}
\arguments{
\item{data}{Vector of angles in radians}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the squared moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}
}

\value{The function returns the density function evaluated at each point in data}

\references{Fernandez-Duran, J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 60(2), 499-503.}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
ccoef<-nntsrandominitial(3)
nntsdensity(1,ccoef,3)
nntsdensity(1+pi,ccoef,3)
nntsdensity(c(1,1+pi),ccoef,3)
}
