\name{snntsmarginallatitude}
\Rdversion{2.1}
\alias{snntsmarginallatitude}
\title{Marginal density function for latitude of the SNNTS model for spherical data}
\description{Marginal density function for latitude of the SNNTS model for spherical data}
\usage{snntsmarginallatitude(data, cpars = 1, M = c(0,0))}
\arguments{
\item{data}{Vector of angles in radians, with one row for each data point. The data must be between zero and pi.}
\item{cpars}{Vector of complex numbers of dimension prod(M+1). The first element is a real and positive 
number. The sum of the squared moduli of the c parameters must be equal to one }
\item{M}{Vector with the number of components in the SNNTS for each dimension}
}

\value{The function returns the SNNTS marginal density function for latitude evaluated at data}
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
M<-c(1,2)
cest<-snntsmanifoldnewtonestimation(data, M,iter=150)
lat<-snntsmarginallatitude(seq(0,pi,.1),cest$cestimates[,3],M)
plot(seq(0,pi,.1),lat,type="l")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
