\name{snntsdensity}
\Rdversion{2.1}
\alias{snntsdensity}
\title{SNNTS density function for spherical data}
\description{Density function for the SNNTS model for spherical data}
\usage{snntsdensity(data, cpars = 1, M = c(0,0))}
\arguments{
\item{data}{Matrix of angles in radians. The first column contains longitude data (between zero and 2*pi), and second 
column contains latitude data (between zero and pi), with one row for each data point}
\item{cpars}{Vector of complex numbers of dimension prod(M+1). The first element is a real and 
positive number. The sum of the squared moduli of the c parameters must be equal to one. }
  \item{M}{Vector with the number of components in the SNNTS for each dimension}
}

\value{The function returns the density function evaluated for each row in the data}

\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
\note{The parameters cinitial and cestimates used by this function are the transformed parameters of the SNNTS density
function, which lie on the surface of the unit hypersphere}

\examples{
data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(2,3)
cpars<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpars[1]<-Re(cpars[1])
cpars<- cpars/sqrt(sum(Mod(cpars)^2))
snntsdensity(data, cpars, M)

}


%\keyword{spherical data }
%\keyword{SNNTS}