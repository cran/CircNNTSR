\name{nntsloglikInterval}
\Rdversion{0.1}
\alias{nntsloglikInterval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NNTS log-likelihood funtion for incidence data
}
\description{
Computes the log-likelihood function for incidence data (number of observed values in certain intervals)
}
\usage{
nntsloglikInterval(cpars = c(0, 0), M = 0, data, limits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last M numbers are the arguments of the c parameters
}
  \item{M}{
number of components in the NNTS
}
  \item{data}{
number of observations in each interval
}
  \item{limits}{
limits of the intervals}
}
\details{ The length of limits has to be 1 plus the length of data
}
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
\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
