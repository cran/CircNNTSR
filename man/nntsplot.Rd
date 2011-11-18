\name{nntsplot}
\Rdversion{1.0}
\alias{nntsplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a NNTS density}
\description{Plots the NNTS density
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsplot(cpars = 1/sqrt(2 * pi), M = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{cpars}{
vector of complex numbers of dimension M+1. The first element is a real and positive number. The sum of the SQUARED moduli of the c parameters must be equal to 1/(2*pi). 
}
  \item{M}{
number of components in the NNTS
}
  \item{\dots}{arguments passed for function curve}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
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
data(Turtles_radians)
#Empirical analysis of data
Turtles_hist<-hist(Turtles_radians,breaks=10,freq=FALSE)
#Estimation of the NNTS density with 3 componentes for data
est<-nntsmanifoldnewtonestimation(Turtles_radians,3)
est
#plot the estimated density
nntsplot(est$cestimates[,2],3)
#add the histogram to the estimated density plot
plot(Turtles_hist, freq=FALSE, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
