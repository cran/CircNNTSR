\name{snntsmanifoldnewtonestimation}
\Rdversion{2.0}
\alias{snntsmanifoldnewtonestimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parameter estimation for SNNTS distributions for spherical data}
\description{Computes the maximum likelihood estimates of the SNNTS model parameters, through a 
Newton algorithm on the hypersphere
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
snntsmanifoldnewtonestimation(data, M = c(0,0), iter = 1000,
initialpoint = FALSE, cinitial)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a matrix of angles in radians, a row for each data point. Angles in first column (longitude) must be in the 
interval (0,2*pi), angles in second column (latitude) must be in the interval (0,pi)
}
  
  \item{M}{
vector with number of components in the SNNTS for each dimension
}
  
\item{iter}{Number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{
initial value for cpars for the optimization algorithm.
vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
First M[1]+1 elements correspond to longitude, next M[2]+1 elements correspond to latitude.
The sum of the SQUARED moduli of the c parameters must be equal to 1. 
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{\item{cestimates }{A matrix of prod(M+1)*(3), the first 2 columns are the parameter number and the last column 
the parameter's c estimators}
\item{loglik}{Optimum loglikelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient's error after last iteration}
%% ...
}
\references{Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008)
    Spherical Distributions Based on Nonnegative Trigonometric Sums, Working Paper, Statistics Department,
   ITAM, DE-C08.6
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(Datab6fisher_ready)
data<-Datab6fisher_ready

M<-c(4,4)
cpar<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpar[1]<-Re(cpar[1])
cpar<- cpar/sqrt(sum(Mod(cpar)^2))

cest<-snntsmanifoldnewtonestimation(data,c(4,4),5000,TRUE,cpar) 
cest
cest<-snntsmanifoldnewtonestimation(data,c(1,2),5000)
cest
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
