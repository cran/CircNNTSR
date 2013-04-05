\name{mnntsdensity2d}
\Rdversion{2.1}
\alias{mnntsdensity2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MNNTS density function (the particular case has two dimensions) }
\description{Density function for the MNNTS model (the particular case has two dimensions). This is an internal function used by mnntsplot
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{mnntsdensity2d(x,y, cpars,M)}
\arguments{\item{x}{Vector of angles in radians}
\item{y}{Vector of angles in radians}
\item{cpars}{Parameters of the model. A vector of complex numbers of dimension prod(M+1). The 
first element is a real and positive number. The first M[1]+1 elements correspond to dimension one,
the next M[2]+1 elements correspond to dimension two.
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
\item{M}{Vector of length two with the number of components in the MNNTS for each dimension}
}
\value{The function returns the density function evaluated at each row in data}
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
