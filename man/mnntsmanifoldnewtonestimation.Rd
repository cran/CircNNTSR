\name{mnntsmanifoldnewtonestimation}
\Rdversion{2.0}
\alias{mnntsmanifoldnewtonestimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parameter estimation for MNNTS distributions}
\description{Computes the maximum likelihood estimates of the MNNTS parameters, through a Newton algorithm on the hypersphere
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mnntsmanifoldnewtonestimation(data, M = 0, R = 1, iter = 1000,
initialpoint = FALSE, cinitial)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a matrix of angles in radians, a column for each dimension, a row for each data point
}
  
  \item{M}{
vector of length R with number of components in the MNNTS for each dimension
}
  \item{R}{
number of dimensions
}
\item{iter}{number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{
initial value for cpars (parameters of the model) for the optimization algorithm.
vector of complex numbers of dimension prod(M+1). The first element is a real and positive number. 
First M[1]+1 elements correspond to dimension 1, next M[2]+1 elements correspond to dimension 2, and so on.
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{\item{cestimates }{A matrix of prod(M+1)*(R+1), the first R columns are the parameter number and the last column the parameter's c estimators}
\item{loglik}{Optimum loglikelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{ Gradient's error after last iteration}
%% ...
}
\references{Fernandez-Duran, J.J. and Gregorio-Dominguez, M.M. (2009) 
    Multivariate Angular Distributions Based on Multiple Nonnegative Trigonometric Sums, 
    Working Paper, Statistics Department, ITAM, DE-C09.1}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,1000)
est
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
