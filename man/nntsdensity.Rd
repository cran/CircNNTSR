\name{nntsdensity}
\Rdversion{0.1}
\alias{nntsdensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NNTS density function
}
\description{
Density function for the NNTS model
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsdensity(cpars = c(0, 0), M = 0, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last M numbers are the arguments of the c parameters
}
  \item{M}{
number of components in the NNTS
}
  \item{theta}{
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
Fernandez-Duran, J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 60(2), 499-503.
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

nntsdensity(c(.01,.02,1,2),2,pi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
