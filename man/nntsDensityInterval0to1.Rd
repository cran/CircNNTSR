\name{nntsDensityInterval0to1}
\Rdversion{1.0}
\alias{nntsDensityInterval0to1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NNTS density function for incidence data defined in the interval [0,1]
%%  ~~function to do ... ~~
}
\description{Computes the density function for incidence data (number of observed values in certain intervals defined in the interval [0,1]) at theta
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsDensityInterval0to1(theta, cpars = 1/sqrt(2 * pi), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{Vector of values defined in the interval [0,1] at which the distribution function is computed
%%     ~~Describe \code{theta} here~~
}
  \item{cpars}{vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
%%     ~~Describe \code{cpars} here~~
}
  \item{M}{number of components in the NNTS
%%     ~~Describe \code{M} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Value of density function at each component of theta
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ccoef<-nntsrandominitial(3)
nntsDensityInterval0to1(c(.8,1.8),ccoef,3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
