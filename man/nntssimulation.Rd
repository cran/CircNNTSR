\name{nntssimulation}
\Rdversion{2.1}
\alias{nntssimulation}
\title{NNTS density simulation function}
\description{Simulation for the density function for the NNTS model}
\usage{nntssimulation(nsim=1, cpars = 1/(2 * pi), M = 0)}

\arguments{
\item{nsim}{Number of simulations}
\item{cpars}{Parameters of the model. A vector of complex numbers of dimension M+1. The sum of the squared moduli of the c parameters must be 
equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS model}
}

\value{\item{simulations}{The function generates nsim random values from the MNNTS density function}
\item{conteo}{Number of uniform random numbers used for simulations}}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-3
ccoef<-nntsrandominitial(M)
data<-nntssimulation(10,ccoef,M)
data
}
