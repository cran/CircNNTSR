\name{nntsmanifoldnewtonestimationinterval0to1}
\Rdversion{2.1}
\alias{nntsmanifoldnewtonestimationinterval0to1}
\title{Parameter estimation for grouped data defined in [0,1)}
\description{Parameter estimation for incidence data (number of observed values in certain intervals 
defined over [0,1))}
\usage{nntsmanifoldnewtonestimationinterval0to1(data, cutpoints, subintervals, M = 0, iter=1000, 
initialpoint = FALSE, cinitial)}

\arguments{
\item{data}{Frequency of data on each interval}
\item{cutpoints}{Vector with the limits of intervals. The length of cutpoints must be one plus the length of the data}
\item{subintervals}{Number of intervals}
\item{M}{Number of components in the NNTS}
\item{iter}{Number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{Vector of size M+1. The first element is real and the next M elements are complex 
(values for $c_0$ and $c_1, \ldots,c_M$).The sum of the squared moduli of the parameters must be equal 
to 1/(2*pi)}
}

\value{
\item{cestimates}{Matrix of M+1 * 2. The first column is the parameter numbers and the second column is the c 
parameter's estimators}
\item{loglik}{Optimum loglikelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{Gradient error after the last iteration}
}

\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez}

\examples{
data<-c(1,2,4,6,1)
cutpoints<-c(0,0.2,0.4,0.6,0.8,0.999999999)
nntsmanifoldnewtonestimationinterval0to1(data, cutpoints, length(data), 1)
}

