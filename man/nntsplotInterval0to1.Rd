\name{nntsplotInterval0to1}
\Rdversion{1.0}
\alias{nntsplotInterval0to1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a NNTS density for incidence data defined in the interval [0,1]
%%  ~~function to do ... ~~
}
\description{Plots the NNTS density  for incidence data defined in the interval [0,1]
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsplotInterval0to1(cpars = 1/sqrt(2 * pi), M = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
%%     ~~Describe \code{cpars} here~~
}
  \item{M}{number of components in the NNTS
%%     ~~Describe \code{M} here~~
}
  \item{\dots}{arguments passed for function curve
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
