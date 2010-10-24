\name{nntsloglikSymmetric}
\Rdversion{1.0-1}
\alias{nntsloglikSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NNTS Symmetric log-likelihood function
}
\description{
Computes the log-likelihood function using a NNTS symmetric density for data
}
\usage{
nntsloglikSymmetric(cparsym = 0, M = 0, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cparsym}{
vector of real numbers of dimension M+1. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last number is the angle of symmetry
}
  \item{M}{
number of components in the NNTS
}
  \item{data}{
vector with angles in radians. If data is a matrix the first column is used
}
}
%\details{
%%  ~~ If necessary, more details than the description above 
%}
\value{The function gives the value of the log-likelihood function for data
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
The default values give the Uniform circular log-likelihood for data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

nntsloglikSymmetric(c(.01,.02,2),2,t(c(pi,pi/2,2*pi,pi)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
