\name{snntsmarginallatitude}
\Rdversion{2.0}
\alias{snntsmarginallatitude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Marginal density function for latitude of the SNNTS model for spherical data}
\description{Marginal density function for latitude of the SNNTS model for spherical data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
snntsmarginallatitude(data, cpars = 1, M = c(0,0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a vector of angles in radians, a row for each data point. must be between 0 and pi.}
  \item{cpars}{
vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
First M[1]+1 elements correspond to longitude, next M[2]+1 elements correspond to latitude.
The sum of the SQUARED moduli of the c parameters must be equal to 1. 
}
  \item{M}{
vector with number of components in the SNNTS for each dimension
}
  
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{The function gives the density function evaluated at data}
\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
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
M<-c(1,2)
cest<-snntsmanifoldnewtonestimation(data, M)
lat<-snntsmarginallatitude(seq(0,pi,.1),cest$cestimates[,3],M)
plot(seq(0,pi,.1),lat,type="l")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
