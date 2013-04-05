\name{nntsloglik}
\Rdversion{2.1}
\alias{nntsloglik}
\title{NNTS log-likelihood function}
\description{Computes the log-likelihood function with NNTS density for data}
\usage{nntsloglik(data, cpars = 1/sqrt(2 * pi), M = 0)}

\arguments{
  \item{data}{Vector with observed angles in radians. }
 \item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the squared moduli of the c parameters must be equal to 1/(2*pi). }
  \item{M}{Number of components in the NNTS}
}

\value{The function returns the value of the log-likelihood function for the data}
\references{Fernandez-Duran, J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 60(2), 499-503.}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
a<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
est<-nntsmanifoldnewtonestimation(a,2)
ccoef<-est$cestimates[,2]
nntsloglik(a,ccoef,2)
}
