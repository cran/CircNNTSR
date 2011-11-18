\name{mnntsmarginal}
\Rdversion{2.0}
\alias{mnntsmarginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Marginal density function of the MNNTS model}
\description{Marginal density function for one dimension of the MNNTS model
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mnntsmarginal(cestimatesarray, M, component, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cestimatesarray}{
A matrix of prod(M+1)*(R+1), the first R columns are the parameter number and the last column the parameter's c estimators
}  
  \item{M}{
vector of length R with number of components in the MNNTS for each dimension
}
  \item{component}{number of the dimension for computing the marginal
}
  \item{theta}{an angle in radians (or a vector of angles)
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{The function gives the density function evaluated at theta}
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
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
