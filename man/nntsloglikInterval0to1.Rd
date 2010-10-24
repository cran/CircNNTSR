\name{nntsloglikInterval0to1}
\Rdversion{1.0-1}
\alias{nntsloglikInterval0to1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS log-likelihood function for incidence data defined in the interval [0,1]}
\description{Computes the log-likelihood function for incidence data (number of observed values in certain intervals defining a partition of the interval [0,1])
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsloglikInterval0to1(data, cutpoints, cpars = 1/sqrt(2 * pi), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{number of observations in each interval
%%     ~~Describe \code{data} here~~
}
  \item{cutpoints}{vector of size length(data)+1 with the
limits of the intervals (in increasing order)
%%     ~~Describe \code{cutpoints} here~~
}
  \item{cpars}{vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
%%     ~~Describe \code{cpars} here~~
}
  \item{M}{number of components in the NNTS
%%     ~~Describe \code{M} here~~
}
  
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{The function gives the value of the log-likelihood function for data
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
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
nntsloglikInterval0to1(data,cutpoints,cpars,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line