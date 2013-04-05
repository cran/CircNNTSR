\name{snntsloglik}
\Rdversion{2.1}
\alias{snntsloglik}
\title{SNNTS log-likelihood function for spherical data}
\description{Computes the log-likelihood function with SNNTS density for spherical data}
\usage{snntsloglik(data, cpars = 1, M = c(0,0))}

\arguments{
\item{data}{Matrix of angles in radians. The first column contains longitude data (between zero and 2*pi), and the 
second column containslatitude data (between zero and pi), with one row for each data point}
\item{cpars}{Vector of complex numbers of dimension prod(M+1). The first element is a real and positive 
number. The first M[1]+1 elements correspond to longitude, the next M[2]+1 elements correspond to latitude.
The sum of the squared moduli of the c parameters must be equal to 1}
  \item{M}{Vector with number of components in the SNNTS for each dimension}
}

\value{The function returns the value of the log-likelihood function for the data}

\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
\note{The parameters cpars used by this function are the transformed parameters of the SNNTS density
function, which lie on the surface of the unit hypersphere}


\examples{
data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(4,4)
cpars<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpars[1]<-Re(cpars[1])
cpars<- cpars/sqrt(sum(Mod(cpars)^2))
snntsdensity(data, cpars, M)
snntsloglik(data, cpars, M)
}
