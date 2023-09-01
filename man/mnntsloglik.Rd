\name{mnntsloglik}
\Rdversion{2.1}
\alias{mnntsloglik}
\title{MNNTS log-likelihood function}
\description{Computes the log-likelihood function with MNNTS density for data}
\usage{mnntsloglik(data, cpars = 1/sqrt(2 * pi), M = 0, R = 1)}
\arguments{\item{data}{Matrix of angles in radians, a column for each dimension, a row for each data point.}
\item{cpars}{Parameters of the model. A vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
The first M[1]+1 elements correspond to dimension one, next M[2]+1 elements correspond to dimension two, and so on.
The sum of the SQUARED moduli of the c parameters must be equal to \eqn{\left(\frac{1}{2*pi}\right)^R}. }
\item{M}{Vector of length R with number of components in the MNNTS for each dimension.}
\item{R}{Number of dimensions.}
}

\value{The function returns the value of the log-likelihood function for the data.}

\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-c(2,3)
R<-length(M)
data<-c(0,pi,pi/2,pi,pi,3*pi/2,pi,2*pi,2*pi,pi)
data<-matrix(data,ncol=2,byrow=TRUE)
data
ccoef<-mnntsrandominitial(M,R)
mnntsdensity(data,ccoef,M,R)
mnntsloglik(data,ccoef,M,R)
}

