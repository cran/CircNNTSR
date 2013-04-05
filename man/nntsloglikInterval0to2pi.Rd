\name{nntsloglikInterval0to2pi}
\Rdversion{2.1}
\alias{nntsloglikInterval0to2pi}
\title{NNTS log-likelihood function for the incidence data defined in the interval [0,2*pi)}
\description{Computes the log-likelihood function for incidence data (number of observed values in certain 
intervals defined in the interval [0,2*pi))}
\usage{nntsloglikInterval0to2pi(data, cutpoints, cpars = 1/sqrt(2 * pi), M = 0)}
\arguments{
\item{data}{Number of observations in each interval}
\item{cutpoints}{Vector of size length(data)+1 with the limits of the exhaustive and mutually exclusive 
intervals in which the interval [0,2*pi) is divided.}
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS density}
}
\value{The function returns the value of the log-likelihood function for the data}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}


\examples{
data<-c(2,3,6,4)
cutpoints<-c(0,pi/2,pi,3*pi/2,2*pi-0.00000001)
est<-nntsmanifoldnewtonestimationinterval0to2pi(data,cutpoints,M=1)
cpars<-est$cestimates[,2]
nntsloglikInterval0to2pi(data,cutpoints,cpars,M=1)
}

