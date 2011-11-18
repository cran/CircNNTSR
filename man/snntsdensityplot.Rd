\name{snntsdensityplot}
\Rdversion{2.0}
\alias{snntsdensityplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a SNNTS density for spherical data}
\description{Computes the points needed to plot the SNNTS density function for spherical data 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
snntsdensityplot(long, lat, cpars = 1, M = c(0,0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{Grid for longitude. Vector with values between 0 and 2*pi}
  \item{lat}{Grid for latitude. Vector with values between 0 and pi}
  \item{cpars}{vector of complex numbers of dimension prod(M+1). The first element is a real and 
positive number. First M[1]+1 elements correspond to longitude, next M[2]+1 elements correspond to 
latitude. The sum of the SQUARED moduli of the c parameters must be equal to 1}  
  \item{M}{vector with number of components in the SNNTS for each dimension}
  \item{...}{arguments passed to function plot}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{The points needed to plot the SNNTS density function
}
\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
%%  ~~who you are~~
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(4,4)
cest<-snntsmanifoldnewtonestimation(data, M)
cpars<-cest$cestimates[,3]
longitud<-seq(0,360,2)*(pi/180)
latitud<-seq(0,180,1)*(pi/180)
z<-outer(longitud,latitud,FUN="snntsdensityplot",cpars,M)
persp(longitud,latitud,z,theta=45,phi=30)
contour(longitud,latitud,z)
points(data[,1],data[,2])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
