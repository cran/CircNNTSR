\name{nntsABcoefficients}
\Rdversion{1.0-1}
\alias{nntsABcoefficients}
\title{AB coefficients}
\description{This function transforms the complex parameters c to the parameters ab, for a
reparameterization of the density function

}
\usage{
nntsABcoefficients(cpars = 1/sqrt(2 * pi), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of complex numbers of dimension \eqn{M+1}{M+1}. The first element is a real and positive number. The sum of the SQUARED moduli of the \eqn{c}{c} parameters must be equal to \eqn{\frac{1}{2\pi}}{1/(2*pi)}. 
}
  \item{M}{
number of components in the NNTS
}
}

\value{The function returns the parameters ab associated with the pararameters cpars. Returns a vector of size \eqn{2M}{2*M},
the first \eqn{M}{M} elements are the \eqn{a_k, k=1,\ldots,M}{a_k, k=1,...,M}, and the next \eqn{M}{M} elements are the \eqn{b_k, k=1,\dots,M}{b_k, k=1,...,M}
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
#random generation of c parameters
ccoef<-nntsrandominitial(3)
ab<-nntsABcoefficients(ccoef,3)
ab
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
