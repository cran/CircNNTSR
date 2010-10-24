\name{nntsABDensitySymmetric}
\Rdversion{1.0-1}
\alias{nntsABDensitySymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Density function with AB coefficients
}
\description{Density function expressed in terms of ab parameters at theta
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsABDensitySymmetric(cparsym = 0, M = 0, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{theta}{a vector of angles in radians
}
  \item{cparsym}{vector of length M+1, the first M components are the squared moduli of the c  parameters the last number is the angle of symmetry 
}
  \item{M}{number of components in the NNTS
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Density function in terms of the ab coefficients evaluated at theta
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
\note{This function is required for compatibility in the Symmetric case
%%  ~~further notes~~
}

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