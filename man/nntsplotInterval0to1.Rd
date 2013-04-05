\name{nntsplotInterval0to1}
\Rdversion{2.1}
\alias{nntsplotInterval0to1}
\title{Plots an NNTS density for a variable defined in the interval [0,1)}
\description{Plots the NNTS density for a variable defined in the interval [0,1)
}
\usage{nntsplotInterval0to1(cpars = 1/sqrt(2 * pi), M = 0, ...)}

\arguments{
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}
\item{\dots}{Arguments passed to the function curve}
}

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
data<-c(1,2,4,6,2)
cutpoints<-c(0,0.2,0.4,0.6,0.8,0.9999999)
est<-nntsmanifoldnewtonestimationinterval0to1(data,cutpoints,5,1)
cpars<-est$cestimates[,2]
nntsplotInterval0to1(cpars, 1)
}
