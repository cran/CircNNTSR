\name{nntsuniformitytestlikelihoodratio}
\alias{nntsuniformitytestlikelihoodratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the statistic and critical values of the circular uniformity test}

\description{Computes the statistic and critical values at 10\%, 5\% and 1\% of the circular uniformity test based on the NNTS likelihood ratio for M values from 1 to 7 and any sample size.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nntsuniformitytestlikelihoodratio(data,M=1, iter=1000, initialpoint = FALSE,
cinitial,gradientstop=1e-10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{data}{Vector of angles in radians}

\item{M}{Number of components in the NNTS}

\item{iter}{Number of iterations}

\item{initialpoint}{TRUE if an initial point for the optimization algorithm will be used}

\item{cinitial}{Vector of size M+1. The first element is real and the next M elements are complex (values for \eqn{c_0} and \eqn{c_1, \ldots ,c_M}). The sum of the squared moduli of the parameters must be equal to \eqn{\frac{1}{2\pi}}}.

\item{gradientstop}{The value of the gradient of the Newton algorithm at which the algorithms stops}

}

\value{

\item{gradient}{Gradient error after the last iteration}	

\item{likratiounifstat}{Value of the likelihood ratio NNTS circular uniformity test statistic}

\item{criticalvalue10percent}{Critical value at a 10\% significance level of the likelihood ratio NNTS circular uniformity test}

\item{criticalvalue05percent}{Critical value at a 5\% significance level of the likelihood ratio NNTS circular uniformity test}

\item{criticalvalue01percent}{Critical value at a 1\% significance level of the likelihood ratio NNTS circular uniformity test}
}

\references{
Fernandez-Duran and J. J. and Gregorio-Dominguez and M. M (2022). Sums of Independent Circular Random Variables and Maximum Likelihood Circular Uniformity Tests Based on Nonnegative Trigonometric Sums Distributions,
arXiv preprint arXiv:2212.01416
}

\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez
}

\examples{
set.seed(200)
a<-2*pi*runif(50)
#NNTS likelihood ratio circular uniformity test for M=2 and gradientstop at 1e-09
nntsuniformitytestlikelihoodratio(data=a,M=2,gradientstop=1e-09)
data(Turtles_radians)
#NNTS likelihood ratio circular uniformity test for M=5 and gradientstop at 1e-12
nntsuniformitytestlikelihoodratio(data=Turtles_radians,M=5,gradientstop=1e-09)
}