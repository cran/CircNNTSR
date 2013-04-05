\name{nntsmanifoldnewtonestimationinterval0to2pi}
\Rdversion{2.1}
\alias{nntsmanifoldnewtonestimationinterval0to2pi}
\title{Parameter estimation for grouped data defined in [0,2*pi)}
\description{Parameter estimation for incidence data (number of observed values in certain intervals 
defined over [0,2*pi))}
\usage{nntsmanifoldnewtonestimationinterval0to2pi(data, cutpoints, 
subintervals,M = 0, iter=1000, initialpoint = FALSE, cinitial)}

\arguments{
\item{data}{Frequency of data on each interval}
\item{cutpoints}{Vector with the limits of intervals. The length of cutpoints has to be one plus the 
length of the data}
\item{subintervals}{Number of intervals}
\item{M}{Number of components in the NNTS}
\item{iter}{Number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{A vector of size M+1. The first element is real, and the next M elements are complex 
(values for $c_0$ and $c_1, \ldots,c_M$).The sum of the squared moduli of the parameters must be equal 
to 1/(2*pi)}
}

\value{
\item{cestimates }{Matrix of M+1 * 2. The first column is the parameter numbers, and the second column is the c 
parameter's estimators}
\item{loglik}{Optimum log-likelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{Gradient error after last iteration}
}
\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez}

\examples{
data<-c(1,2,6,4)
cutpoints<-c(0,pi/2,pi,3*pi/2,2*pi-0.00000001)
nntsmanifoldnewtonestimationinterval0to2pi(data, cutpoints, length(data),1)
}




% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
