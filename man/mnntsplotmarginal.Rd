\name{mnntsplotmarginal}
\Rdversion{2.1}
\alias{mnntsplotmarginal}
\title{Plots a MNNTS marginal density}
\description{Plots the MNNTS marginal density function }
\usage{mnntsplotmarginal(cestimates, M, component, ...)}

\arguments{
\item{cestimates}{Matrix of prod(M+1)*(R+1). The first R columns are the parameter number, and 
the last column the c parameter's estimators. The matrix could be the output of mnntsmanifoldnewtonestimation 
$cestimates}  
\item{M}{Vector with number of components in the MNNTS for each dimension}
\item{component}{Number of the dimension for computing the marginal density}
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
mnntsplotmarginal(cest, M, 1)
mnntsplotmarginal(cest, M, 2)
}

