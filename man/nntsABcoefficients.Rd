\name{nntsABcoefficients}
\Rdversion{1.0}
\alias{nntsABcoefficients}
\title{AB coefficients}
\description{This function transforms the complex parameters c to the parameters ab for a
reparameterization of the density function
}
\usage{
nntsABcoefficients(cpars = 1/sqrt(2 * pi), M = 0)
}

\arguments{
  \item{cpars}{Vector of complex numbers of dimension M+1. The first element is a real and positive number. 
The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). }
  \item{M}{Number of components in the NNTS}
}

\value{The function returns the parameters ab associated with the parameters cpars and returns a vector of real numbers
of size 2*M, where the first M elements are the a_k, k=1,...,M, and the next M elements are the b_k, k=1,...,M}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
#random generation of c parameters
ccoef<-nntsrandominitial(3)
ccoef
ab<-nntsABcoefficients(ccoef,3)
ab
}

%\keyword{ }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
