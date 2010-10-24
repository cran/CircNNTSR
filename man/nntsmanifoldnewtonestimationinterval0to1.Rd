\name{nntsmanifoldnewtonestimationinterval0to1}
\Rdversion{1.0-1}
\alias{nntsmanifoldnewtonestimationinterval0to1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parameter estimation for incidence data defined in [0,1]}
\description{Parameter estimation for incidence data (number of observed values in certain intervals defining a partition of the interval [0,1])
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsmanifoldnewtonestimationinterval0to1(data, cutpoints, 
subintervals, M = 0, iter=1000, initialpoint = FALSE, cinitial)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Number of observations in each interval}
  \item{cutpoints}{Vector with the limits of intervals. The length of cutpoints has to be 1 plus the length of data (in increasing order)
}
\item{subintervals}{Number of intervals (Length of cutpoints minus one)}
\item{M}{Number of components in the NNTS}
\item{iter}{Number of iterations}
\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}
\item{cinitial}{A vector of size M+1, the first element is a nonnegative real number (\eqn{c_0}{c_0}) and the next M elements are complex (\eqn{c_1, \ldots,c_M}{c_1, ...,c_M}). The sum of the squared moduli of the parameters must be equal to \eqn{\frac{1}{2\pi}}{1/(2*pi)}
}}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{cestimates }{A matrix with (M+1) rows and 2 columns,, the first column parameter number and the second column the parameter's c estimators}
\item{loglik}{Optimum loglikelihood value}
\item{AIC}{Value of Akaike's Information Criterion}
\item{BIC}{Value of Bayesian Information Criterion}
\item{gradnormerror}{Gradient's error after last iteration}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Juan Jose Fernandez-Duran y Maria Mercedes Gregorio-Dominguez
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data<-c(3,5,5,6,7,6,7,9,5,4,3,3)
cutpoints<-seq(0,1,1/12)
cpars<-nntsrandominitial(1)
nntsmanifoldnewtonestimationinterval0to1(data,cutpoints,12,1,1000,
TRUE,nntsrandominitial(1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
