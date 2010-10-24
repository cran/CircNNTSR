\name{nntsmanifoldnewtonestimation}
\Rdversion{1.0-1}
\alias{nntsmanifoldnewtonestimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parameter estimation for NNTS distributions}
\description{Computes the maximum likelihood estimates of the NNTS parameters, through a Newton algorithm on the hypersphere
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsmanifoldnewtonestimation(data, M=0, iter=1000, 
initialpoint = FALSE, cinitial)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector of angles in radians}
  \item{M}{Number of components in the NNTS}
  \item{iter}{Number of iterations}
  \item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
  \item{cinitial}{A vector of size M+1, the first element is a nonnegative real number (\eqn{c_0}{c_0}) and the next M elements are complex (\eqn{c_1, \ldots,c_M}{c_1, ...,c_M}). The sum of the squared moduli of the parameters must be equal to \eqn{\frac{1}{2\pi}}{1/(2*pi)}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{cestimates }{A matrix with (M+1) rows and 2 columns, the first column are the parameter index numbers and the second column the parameter c estimates}
\item{loglik}{Optimum loglikelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient's error after last iteration}
%% ...
}
\references{Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2010). Maximum Likelihood Estimation
of Nonnegative Trigonometric Sum Models Using a Newton-like Algorithm on Manifolds, 
arXiv:1010.6098v1
}
\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with 
%\section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
a<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
#Estimation of the NNTSdensity with 2 components for data and 1000 iterations
nntsmanifoldnewtonestimation(a,2,1000)

data(Turtles_radians)
#Empirical analysis of data
Turtles_hist<-hist(Turtles_radians,breaks=10,freq=FALSE)
#Estimation of the NNTS density with 3 componentes for data
nntsmanifoldnewtonestimation(Turtles_radians,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line