\name{nntsloglikSymmetric}
\Rdversion{0.1}
\alias{nntsloglikSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS symmetric log-likelihood function}
\description{Computes the log-likelihood function with NNTS symmetric density for the data}
\usage{nntsloglikSymmetric(cpars = c(0, 0), M = 0, data)}
\arguments{
\item{cpars}{Vector of real numbers of dimension M+1. The first M numbers are the squared moduli of the c parameters. 
The sum must be less than 1/(2*pi). The last number is the mean of symmetry}
  \item{M}{Number of components in the NNTS}
  \item{data}{Vector with angles in radians. The first column  is used if data are a matrix}
}
%\details{
%%  ~~ If necessary, more details than the description above 
%}
\value{The function returns the value of the log-likelihood function for the data}
\references{
Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}
\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
}
\note{The default values provide the Uniform circular log-likelihood for the data
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
