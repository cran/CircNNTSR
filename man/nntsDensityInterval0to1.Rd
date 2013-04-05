\name{nntsDensityInterval0to1}
\Rdversion{2.1}
\alias{nntsDensityInterval0to1}
\title{NNTS density function for a variable defined in the interval [0,1)}
\description{Computes the density function at theta for a variable defined in the interval [0,1)) 
}

\usage{nntsDensityInterval0to1(S, cpars = 1/sqrt(2 * pi), M = 0)}

\arguments{
\item{S}{Vector of values defined in the interval [0,1) at which the density function is computed}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of 
the squared moduli of the c parameters must be equal to 1/(2*pi) }
\item{M}{Number of components in the NNTS}
}
\details{This function computes the density function of a variable S (S in the interval [0,1)). If theta 
is defined in radians (theta in the interval [0,2*pi)), the relation between S and theta is theta=2*pi*S. 
}
\value{Value of density function at each component of S}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
ccoef<-nntsrandominitial(3)
nntsDensityInterval0to1(c(.8,1.8),ccoef,3)
}
