\name{snntssimulation}
\Rdversion{2.1}
\alias{snntssimulation}
\title{SNNTS density simulation function}
\description{Simulation for the density function for the SNNTS model}
\usage{snntssimulation(nsim=1, cpars =(1/(2*pi))^2, M = c(0,0))}

\arguments{
\item{nsim}{Number of simulations}
\item{cpars}{Vector of complex numbers of dimension prod(M+1). The first element is a real and positive 
number. The first M[1]+1 elements correspond to longitude, the next M[2]+1 elements correspond to latitude.
The sum of the squared moduli of the c parameters must be equal to one }
\item{M}{Vector with the number of components in the SNNTS for each dimension}
}

\value{\item{simulations}{The function generates nsim random values from the SNNTS density function}
\item{conteo}{Number of uniform random numbers used for simulations}}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-c(2,3)
R<-length(M)
ccoef<-mnntsrandominitial(M,R)
data<-mnntssimulation(10,ccoef,M,R)
data
}
