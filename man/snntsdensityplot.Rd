\name{snntsdensityplot}
\Rdversion{2.1}
\alias{snntsdensityplot}
\title{Plots a SNNTS density for spherical data}
\description{Computes the points needed to plot the SNNTS density function for spherical data }
\usage{snntsdensityplot(long, lat, cpars = 1, M = c(0,0))}
\arguments{
  \item{long}{Grid for longitude. Vector with values between zero and 2*pi}
  \item{lat}{Grid for latitude. Vector with values between zero and pi}
  \item{cpars}{Vector of complex numbers of dimension prod(M+1). The sum of the squared moduli of 
the c parameters must be equal to one}  
  \item{M}{Vector with the number of components in the SNNTS for each dimension}
}

\value{The points needed to plot the SNNTS density function}
\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
\note{The parameters cpars used by this function are the transformed parameters of the SNNTS density
function, which lie on the surface of the unit hypersphere}

\examples{
	set.seed(200)
data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(4,4)
cest<-snntsmanifoldnewtonestimation(data, M, iter=150)
cpars<-cest$cestimates[,3]
longitud<-seq(0,360,10)*(pi/180)
latitud<-seq(0,180,5)*(pi/180)
z<-outer(longitud,latitud,FUN="snntsdensityplot",cpars,M)
persp(longitud,latitud,z,theta=45,phi=30)
contour(longitud,latitud,z)
points(data[,1],data[,2])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
