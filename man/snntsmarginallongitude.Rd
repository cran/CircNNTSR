\name{snntsmarginallongitude}
\Rdversion{2.1}
\alias{snntsmarginallongitude}
\title{Marginal density function for the longitude of the SNNTS model for spherical data}
\description{Marginal density function for the longitude of the SNNTS model for spherical data}
\usage{snntsmarginallongitude(data, cpars = 1, M = c(0,0))}

\arguments{
  \item{data}{Vector of angles in radians, with one row for each data point. The data must be between zero and 2*pi}
  \item{cpars}{Vector of complex numbers of dimension prod(M+1). The first element is a real and positive 
number. The first M[1]+1 elements correspond to longitude, and the next M[2]+1 elements correspond to latitude.
The sum of the squared moduli of the c parameters must be equal to one. }
  \item{M}{Vector with number of components in the SNNTS for each dimension}
}

\value{The function returns the density function evaluated for the data}
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
M<-c(1,2)
cest<-snntsmanifoldnewtonestimation(data, M)
long<-snntsmarginallongitude(seq(0,2*pi,.1),cest$cestimates[,3],M)
plot(seq(0,2*pi,.1),long,type="l")
}

