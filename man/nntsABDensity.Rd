\name{nntsABDensity}
\Rdversion{1.0-1}
\alias{nntsABDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Density function with AB coefficients}
\description{Density function expressed in terms of ab parameters at theta
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsABDensity(theta, cpars = 1/sqrt(2 * pi), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{theta}{a vector of angles in radians
}
  \item{cpars}{vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
}
  \item{M}{number of components in the NNTS
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Density function in terms of the ab coefficients evaluated at theta
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ccoef<-nntsrandominitial(3)
nntsABDensity(1,ccoef,3)
nntsABDensity(1+2*pi,ccoef,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
