  \name{nntsplot}
\Rdversion{2.1}
\alias{nntsplot}
\title{Plots the NNTS density}
\description{Plots the NNTS density}
\usage{nntsplot(cpars = 1/sqrt(2 * pi), M = 0, ...)}
\arguments{
\item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Number of components in the NNTS}
\item{\dots}{Arguments passed to the function curve}
}

\examples{
	set.seed(200)
data(Turtles_radians)
#Empirical analysis of data
Turtles_hist<-hist(Turtles_radians,breaks=10,freq=FALSE)
#Estimation of the NNTS density with 3 componentes for data
est<-nntsmanifoldnewtonestimation(Turtles_radians,3,iter=200)
est
#plot the estimated density
nntsplot(est$cestimates[,2],3)
#add the histogram to the estimated density plot
plot(Turtles_hist, freq=FALSE, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
