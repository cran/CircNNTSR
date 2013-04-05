\name{nntsSymmetricDensity}
\Rdversion{0.1}
\alias{nntsSymmetricDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Symmetric NNTS density function}
\description{Density function for the Symmetric NNTS}
\usage{nntsSymmetricDensity(cpars = c(0, 0), M = 0, theta)}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{cpars}{Vector of real numbers of dimension 2*M. The first M numbers are the squared moduli of the c parameters. 
The sum must be less than 1/(2*pi). The last number is the mean of symmetry}
\item{M}{Number of components in the NNTS}
\item{theta}{Angle in radians}
}

\value{The function returns the density function evaluated at theta}
\references{
Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}
\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
}
\note{The default values provide the uniform circular density}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%nntsSymmetricDensity(c(.01,.02,2),2,pi)
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
