\name{mnntsmarginal}
\Rdversion{2.1}
\alias{mnntsmarginal}
\title{Marginal density function of the MNNTS model}
\description{Marginal density function for one dimension of the MNNTS model evaluated at a point}
\usage{mnntsmarginal(cestimatesarray, M, component, theta)}

\arguments{
\item{cestimatesarray}{Matrix of prod(M+1)*(R+1). The first R columns are the parameter number, and the 
last column is the c parameter's estimators}  
\item{M}{Vector of length R with number of components in the MNNTS for each dimension}
\item{component}{Number of the dimension for computing the marginal}
\item{theta}{An angle in radians (or a vector of angles)}
}

\value{The function returns the density function evaluated at theta}
\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1
    
    Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2014). Modeling Angles in Proteins and Circular Genomes Using Multivariate Angular Distributions Based on Nonnegative Trigonometric Sums. Statistical Applications in Genetics and Molecular Biology, 13(1), 1-18. doi:10.1515/sagmb-2012-0012
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
	set.seed(200)
M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,100)
est
cest<-est$cestimates
mnntsmarginal(cest,M,1,pi)
}
