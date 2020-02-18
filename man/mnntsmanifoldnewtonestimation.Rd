\name{mnntsmanifoldnewtonestimation}
\Rdversion{2.1}
\alias{mnntsmanifoldnewtonestimation}
\title{Parameter estimation for the MNNTS distributions}
\description{Computes the maximum likelihood estimates of the MNNTS parameters using a Newton 
algorithm on the hypersphere}
\usage{mnntsmanifoldnewtonestimation(data,M=0,R=1,iter=1000,initialpoint=FALSE,cinitial)}
\arguments{
\item{data}{Matrix of angles in radians, a column for each dimension, a row for each data point}
\item{M}{Vector of length R with number of components in the MNNTS for each dimension}
\item{R}{Number of dimensions}
\item{iter}{Number of iterations for the Newton algorithm}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{Initial value for cpars (parameters of the model) for the optimization algorithm.
Vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
The first M[1]+1 elements correspond to dimension one, the next M[2]+1 elements correspond to dimension two, and so on. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
}

\value{\item{cestimates }{Matrix of prod(M+1)*(R+1). The first R columns are the parameter number, and 
the last column is the c parameter's estimators}
\item{loglik}{Optimum log-likelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient error after the last iteration}
}

\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
	set.seed(200)
M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,100)
est
}


