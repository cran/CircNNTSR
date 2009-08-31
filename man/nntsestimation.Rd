\name{nntsestimation}
\Rdversion{0.1}
\alias{nntsestimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS Coefficient Estimation}
\description{
Computes the maximum likelihood estimates of the NNTS parameters
}
\usage{
nntsestimation(M = 0, data, maxit = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{Number of components in the NNTS}
  \item{data}{a vector of angles in radians}
  \item{maxit}{maximum number of iterations of the optimization algorithm}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{coef}{vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last M numbers are the arguments of the c parameters}
  \item{loglik}{Optimum loglikelihood value}
  \item{AIC}{Value of Akaike's Information Criterion}
  \item{BIC}{Value of Bayesian Information Criterion}
  \item{convergence}{An integer code: 0 indicates successful convergence; error codes are: 1
indicates that the iteration limit maxit has been reached, 10 indicates degeneracy of the
Nelder-Mead simplex}
}
\references{
Fernandez-Duran. J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 60(2), 499-503.
}
\author{
Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez
}
\note{
For the maximization of the loglikelihood function the function constrOptim from the package stats is used 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

a<-c(runif(10,3*pi/2,2*pi-0.00000000000001),runif(10,pi/2,pi-0.00000000000001))
est<-nntsestimation(2,a)
nntsplot(est$coef,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
