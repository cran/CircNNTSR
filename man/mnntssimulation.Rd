\name{mnntssimulation}
\Rdversion{2.1}
\alias{mnntssimulation}
\title{MNNTS density simulation function}
\description{Simulation for the density function for the MNNTS model}
\usage{mnntssimulation(nsim=1, cpars = 1/(2 * pi), M = c(0,0), R=2)}

\arguments{
\item{nsim}{Number of simulations}
\item{cpars}{Parameters of the model. A vector of complex numbers of dimension prod(M+1). The first 
element is a real and positive number. The first M[1]+1 elements correspond to dimension one, next M[2]+1  
elements correspond to dimension two, and so on. The sum of the SQUARED moduli of the c parameters must be 
equal to 1/(2*pi). }
\item{M}{Vector of length R with number of components in the MNNTS for each dimension}
\item{R}{Number of dimensions}
}

\value{\item{simulations}{The function generates nsim random values from the MNNTS density function}
\item{conteo}{Number of uniform random numbers used for simulations}}
\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-c(2,3)
R<-length(M)
ccoef<-mnntsrandominitial(M,R)
data<-mnntssimulation(10,ccoef,M,R)
data
}
