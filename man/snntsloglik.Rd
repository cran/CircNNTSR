\name{snntsloglik}
\Rdversion{2.0}
\alias{snntsloglik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SNNTS log-likelihood function for spherical data}
\description{Computes the log-likelihood function with SNNTS density for spherical data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
snntsloglik(data, cpars = 1, M = c(0,0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a matrix of angles in radians, first column for longitude (between 0 and 2*pi), second column for 
latitude (between 0 and pi), a row for each data point}
  \item{cpars}{
vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
First M[1]+1 elements correspond to longitude, next M[2]+1 elements correspond to latitude.
The sum of the SQUARED moduli of the c parameters must be equal to 1}
  \item{M}{vector with number of components in the SNNTS for each dimension}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{The function gives the value of the log-likelihood function for the data
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
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
M<-c(4,4)
cpars<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpars[1]<-Re(cpars[1])
cpars<- cpars/sqrt(sum(Mod(cpars)^2))
snntsdensity(data, cpars, M)
snntsloglik(data, cpars, M)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
