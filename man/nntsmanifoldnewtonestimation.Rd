\name{nntsmanifoldnewtonestimation}
\Rdversion{2.1}
\alias{nntsmanifoldnewtonestimation}
\title{Parameter estimation for NNTS distributions}
\description{Computes the maximum likelihood estimates of the NNTS parameters, using a Newton algorithm 
on the hypersphere}
\usage{nntsmanifoldnewtonestimation(data, M=0, iter=1000, initialpoint = FALSE, cinitial)}

\arguments{
  \item{data}{Vector of angles in radians}
  \item{M}{Number of components in the NNTS}
  \item{iter}{Number of iterations}
  \item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
  \item{cinitial}{Vector of size M+1. The first element is real and the next M elements are complex 
(values for $c_0$ and $c_1, \ldots,c_M$). The sum of the squared moduli of the parameters must be equal 
to 1/(2*pi)}
}
\value{
\item{cestimates }{Matrix of (M+1)x2. The first column is the parameter numbers, and the second column is 
the c parameter's estimators}
\item{loglik}{Optimum log-likelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient error after the last iteration}
}
\references{Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2010). Maximum Likelihood Estimation
of Nonnegative Trigonometric Sums Models by Using a Newton-like Algorithm on Manifolds, 
Working Paper, Department of Statistics, ITAM, DE-C10.8}
\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez}

\examples{
set.seed(200)
a<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
#Estimation of the NNTSdensity with 2 components for data and 200 iterations
nntsmanifoldnewtonestimation(a,2,iter=200)

data(Turtles_radians)
#Empirical analysis of data
Turtles_hist<-hist(Turtles_radians,breaks=10,freq=FALSE)
#Estimation of the NNTS density with 3 componentes for data
nntsmanifoldnewtonestimation(Turtles_radians,3,iter=200)

}
