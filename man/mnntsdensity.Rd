\name{mnntsdensity}
\Rdversion{2.0}
\alias{mnntsdensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MNNTS density function}
\description{Density function for the MNNTS model
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mnntsdensity(data, cpars = 1/sqrt(2 * pi), M = 0, R=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a matrix of angles in radians, a column for each dimension, a row for each data point
}
  \item{cpars}{parameters of  the model. Vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
First M[1]+1 elements correspond to dimension 1, next M[2]+1 elements correspond to dimension 2, and so on.
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
}
  \item{M}{
vector of length R with number of components in the MNNTS for each dimension
}
  \item{R}{
number of dimensions
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{The function gives the density function evaluated at each row in data
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
M<-c(2,3)
R<-length(M)
data<-c(0,pi,pi/2,pi,pi,3*pi/2,pi,2*pi,2*pi,pi)
data<-matrix(data,ncol=2,byrow=TRUE)
data
ccoef<-mnntsrandominitial(M,R)
mnntsdensity(data,ccoef,M,R)

M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,1000)
est
ccoef<-est$cestimates[,3]
mnntsdensity(data,ccoef,M,R)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
