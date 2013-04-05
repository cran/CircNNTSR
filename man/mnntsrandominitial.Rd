\name{mnntsrandominitial}
\Rdversion{2.1}
\alias{mnntsrandominitial}
\title{Initial random point}
\description{This function generates a random point on the surface of the prod(M+1)-dimensional unit hypersphere}
\usage{mnntsrandominitial(M = 1, R = 1)}

\arguments{
\item{M}{Vector of length R with number of components in the MNNTS for each dimension}
\item{R}{Number of dimensions}
}

\value{Returns a valid initial point for estimation functions}

\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-c(2,3)
R<-length(M)
mnntsrandominitial(M,R)
}

