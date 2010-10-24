\name{nntsSymmetricDensity}
\Rdversion{1.0-1}
\alias{nntsSymmetricDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Symmetric NNTS density function}
\description{Density function for the Symmetric NNTS
}
\usage{
nntsSymmetricDensity(cparsym=0, M = 0, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cparsym}{
%%     ~~Describe \code{cpars} here~~
vector of length M+1, the first M components are the squared moduli of the c  parameters the last number is the angle of symmetry
}
  \item{M}{
%%     ~~Describe \code{M} here~~
number of components in the NNTS
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
an angle in radians
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{The function gives the density function evaluated at theta
%%  ~Describe the value returned  
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}
\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
}
\note{
The default values give the Uniform circular density
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
nntsSymmetricDensity(c(.01,.02,2),2,pi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
