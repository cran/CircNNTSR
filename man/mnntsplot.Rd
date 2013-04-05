\name{mnntsplot}
\Rdversion{2.1}
\alias{mnntsplot}
\title{Plots a MNNTS bivariate density}
\description{Plots the MNNTS bivariate density function }
\usage{mnntsplot(cestimates, M, ...)}

\arguments{
\item{cestimates}{Matrix of prod(M+1)*(R+1). The first R columns are the parameter number, and the last 
column is the c parameter's estimators. R=2 for a bivariate distribution}  
\item{M}{Vector with the number of components in the MNNTS for each dimension}
\item{...}{Arguments passed to the function plot}
}

%%\value{
%%}
\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,1000)
est
cest<-est$cestimates
mnntsplot(cest, M)
}


