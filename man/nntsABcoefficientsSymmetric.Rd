\name{nntsABcoefficientsSymmetric}
\Rdversion{1.0-1}
\alias{nntsABcoefficientsSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AB coefficients}
\description{This function transforms \eqn{c}{c} parameters of the NNTS symmetric density to the parameters ab, for a reparameterization of the density function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsABcoefficientsSymmetric(cparsym = 0, M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cparsym}{vector of length \eqn{M+1}{M+1}, the first \eqn{M}{M} components are the squared moduli of the \eqn{c}{c}  parameters the last number is the angle of symmetry.
}
  \item{M}{
number of components in the NNTS
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{The function returns the parameters ab associated with the pararameters cpars. 
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
\examples{
#random generation of c parameters
ccoef<-nntsrandominitialSymmetric(3)
ab<-nntsABcoefficientsSymmetric(ccoef,3)
ab
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
