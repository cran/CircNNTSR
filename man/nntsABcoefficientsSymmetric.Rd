\name{nntsABcoefficientsSymmetric}
\Rdversion{1.0}
\alias{nntsABcoefficientsSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AB coefficients}
\description{This function transforms the complex parameters c to the parameters ab, for a
reparameterization of the density function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsABcoefficientsSymmetric(cpars = c(0, 0), M = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of complex numbers of dimension 2*M. 
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
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
%%\note{This function is required for compatibility in the Symmetric case
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
