\name{mnntsdensity}
\Rdversion{2.1}
\alias{mnntsdensity}
\title{MNNTS density function}
\description{Density function for the MNNTS model}
\usage{mnntsdensity(data, cpars = 1/sqrt(2 * pi), M = 0, R=1)}

\arguments{
\item{data}{Matrix of angles in radians, a column for each dimension, a row for each data point}
\item{cpars}{Parameters of the model. A vector of complex numbers of dimension prod(M+1). The first 
element is a real and positive number. The first M[1]+1 elements correspond to dimension one, the next M[2]+1 
elements correspond to dimension two, and so on. The sum of the SQUARED moduli of the c parameters must be 
equal to 1/(2*pi). }
\item{M}{Vector of length R with number of components in the MNNTS for each dimension}
\item{R}{Number of dimensions}
}

\value{The function returns the density function evaluated at each row in data}
    
\references{
Juan Jose Fernandez-Duran, Maria Mercedes Gregorio-Dominguez (2016). CircNNTSR: An R Package for the Statistical Analysis of Circular, Multivariate Circular, and Spherical Data Using Nonnegative Trigonometric Sums. Journal of Statistical Software, 70(6), 1-19. doi:10.18637/jss.v070.i06
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
	set.seed(200)
M<-c(2,3)
R<-length(M)
data<-c(0,pi,pi/2,pi,pi,3*pi/2,pi,2*pi,2*pi,pi)
data<-matrix(data,ncol=2,byrow=TRUE)
data
ccoef<-mnntsrandominitial(M,R)
mnntsdensity(data,ccoef,M,R)

M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,100)
est
ccoef<-est$cestimates[,3]
mnntsdensity(data,ccoef,M,R)
}
