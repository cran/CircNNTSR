\name{nntsloglikInterval0to1}
\Rdversion{2.1}
\alias{nntsloglikInterval0to1}
\title{NNTS log-likelihood function for the incidence data defined in the interval [0,1)}
\description{Computes the log-likelihood function for incidence data (number of observed values in 
certain intervals defined in the interval [0,1))}
\usage{nntsloglikInterval0to1(data, cutpoints, cpars = 1/sqrt(2 * pi), M = 0)}

\arguments{
\item{data}{Number of observations in each interval}
\item{cutpoints}{Vector of size length(data)+1 with the limits of the intervals}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}
}

\value{The function returns the value of the log-likelihood function for data}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
data<-c(1,2,6,4,1)
cutpoints<-c(0,0.2,0.4,0.6,0.8,0.9999999)
cpars<-nntsrandominitial(1)
nntsloglikInterval0to1(data, cutpoints, cpars, 1)
}
