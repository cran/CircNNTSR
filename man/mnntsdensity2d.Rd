\name{mnntsdensity2d}
\Rdversion{2.0}
\alias{mnntsdensity2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MNNTS density function (particular case 2 dimensions) }
\description{Density function for the MNNTS model (particular cas 2 dimensions). This is an internal function used by mnntsplot
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mnntsdensity2d(x,y, cpars,M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of angles in radians
}
\item{y}{
a vector of angles in radians
}
  \item{cpars}{parameters of  the model. Vector of complex numbers of dimension prod(M+1). The 
first element is a real and positive number. First M[1]+1 elements correspond to dimension 1,
next M[2]+1 elements correspond to dimension 2.
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
}
  \item{M}{
vector of length 2 with number of components in the MNNTS for each dimension
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{The function gives the density function evaluated at each row in data
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
