\name{nntsABcoefficients}
\Rdversion{0.1}
\alias{nntsABcoefficients}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AB coefficients}
\description{
This function transforms the complex parameters c to the parameters ab, for a
reparameterization of the density function
}
\usage{
nntsABcoefficients(cpars = c(0, 0), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last M numbers are the arguments of the c parameters
}
  \item{M}{
number of components in the NNTS
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{The function returns the parameters ab associated with the pararameters cpars
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
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
}
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
