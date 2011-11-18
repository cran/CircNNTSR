\name{mnntsplotwithmarginals}
\Rdversion{2.0}
\alias{mnntsplotwithmarginals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a MNNTS bivariate density together with the marginals}
\description{Plots the MNNTS bivariate density function together with the marginals 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mnntsplotwithmarginals(cestimates, M, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cestimates}{
A matrix of prod(M+1)*(R+1), the first R columns are the parameter number and the last column the 
parameter's c estimators. It is the output of mnntsmanifoldnewtonestimation $cestimates.
}  
  \item{M}{
vector of length R with number of components in the MNNTS for each dimension
}
\item{...}{arguments passed to function plot}
}  
  

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%}
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
cest<-est$cestimates
mnntsplotwithmarginals(cest, M)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
