\name{nntsestimationSymmetric}
\Rdversion{0.1}
\alias{nntsestimationSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS Symmetric Coefficient estimation}
\description{Computes the maximum likelihood estimates of the symmetric NNTS parameters}
\usage{nntsestimationSymmetric(M = 0, data, maxit = 500)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{Number of components in the NNTS}
  \item{data}{Vector of angles in radians}
  \item{maxit}{Maximum number of iterations in the optimization algorithm}
}
\value{
  \item{coef}{Vector of length M+1. The first M components are the squared moduli of the c parameters, and the last 
number is the mean of symmetry}
 \item{loglik}{Optimum log-likelihood value}
  \item{AIC}{Value of Akaike's Information Criterion}
  \item{BIC}{Value of Bayesian Information Criterion}
 \item{convergence}{An integer code: zero indicates successful convergence; error codes are the following: one
indicates that the iteration limit maxit has been reached, and 10 indicates degeneracy of the
Nelder-Mead simplex}
}
\references{
Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}
\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez
}
\note{
For the maximization of the loglikelihood function the function constrOptim from the package stats is used 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
b<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
estS<-nntsestimationSymmetric(2,b)
nntsplotSymmetric(estS$coef,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
