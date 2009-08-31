\name{nntsplot}
\Rdversion{0.1}
\alias{nntsplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a NNTS density
}
\description{
Plots the NNTS density
}
\usage{
nntsplot(cpars = c(0, 0), M = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last M numbers are the arguments of the c parameters
}
  \item{M}{
number of components in the NNTS
}
  \item{\dots}{arguments passed for function curve}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
