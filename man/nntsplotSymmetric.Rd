\name{nntsplotSymmetric}
\Rdversion{0.1}
\alias{nntsplotSymmetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a Symmetric NNTS density function
}
\description{
Plots the Symmetric NNTS density function
}
\usage{
nntsplotSymmetric(cpars = c(0, 0), M = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cpars}{
vector of real numbers of dimension 2*M. The first M numbers are the SQUARED moduli of the c parameters, the sum must be less than 1/(2*pi). The last number is the mean of symmetry
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
\references{
Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}
\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}
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
