\name{snntsmanifoldnewtonestimation}
\Rdversion{2.1}
\alias{snntsmanifoldnewtonestimation}
\title{Parameter estimation for SNNTS distributions for spherical data}
\description{Computes the maximum likelihood estimates of the SNNTS model parameters using a 
Newton algorithm on the hypersphere
}
\usage{snntsmanifoldnewtonestimation(data, M = c(0,0), iter = 1000,
initialpoint = FALSE, cinitial)}

\arguments{
\item{data}{Matrix of angles in radians, with one row for each data point. The first column contains longitude data
(between zero and 2*pi), and second column contains latitude data (between zero and pi), with one row for each data 
point}
\item{M}{Vector with number of components in the SNNTS for each dimension}
  \item{iter}{Number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{Initial value for cpars for the optimization algorithm, avector of complex numbers of dimension 
prod(M+1). The first element is a real and positive number. 
The sum of the squared moduli of the c parameters must be equal to one. 
}
}


\value{\item{cestimates}{Matrix of prod(M+1)*(3). The first two columns are the parameter numbers, and the 
last column is the c parameter's estimators}
\item{loglik}{Optimum log-likelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient error after the last iteration}
}
\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
\note{The parameters cinitial and cestimates used by this function are the transformed parameters of the SNNTS density
function, which lie on the surface of the unit hypersphere}

\examples{
set.seed(200)
data(Datab6fisher_ready)
data<-Datab6fisher_ready

M<-c(4,4)
cpar<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpar[1]<-Re(cpar[1])
cpar<- cpar/sqrt(sum(Mod(cpar)^2))

cest<-snntsmanifoldnewtonestimation(data,c(4,4),100,TRUE,cpar) 
cest
cest<-snntsmanifoldnewtonestimation(data,c(1,2),100)
cest

}


