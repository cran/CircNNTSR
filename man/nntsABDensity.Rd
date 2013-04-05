\name{nntsABDensity}
\Rdversion{1.0}
\alias{nntsABDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Density function with AB coefficients}
\description{Density function expressed in terms of the ab parameters at theta}
\usage{nntsABDensity(theta, cpars = 1/sqrt(2 * pi), M = 0)}
\arguments{
\item{theta}{Vector of angles in radians}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of 
the SQUARED moduli of the c parameters must be equal to 1/(2*pi)}
\item{M}{Number of components in the NNTS}}
\value{Returns the density function in terms of the ab coefficients evaluated at theta}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
ccoef<-nntsrandominitial(3)
nntsABDensity(1,ccoef,3)
nntsABDensity(1+2*pi,ccoef,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
