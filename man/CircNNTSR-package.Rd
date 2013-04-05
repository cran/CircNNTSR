\name{CircNNTSR-package}
\Rdversion{2.1}
\alias{CircNNTSR-package}
\alias{CircNNTSR}
\docType{package}
\title{
CircNNTSR: An R package for the statistical analysis of circular data using nonnegative trigonometric 
sums (NNTS) models
}
\description{
A collection of utilities for the statistical analysis of circular and spherical data using nonnegative 
trigonometric sum (NNTS) models
}
\details{
\tabular{ll}{
Package: \tab CircNNTSR\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2011-12-27\cr
License: \tab GLP (>=2)\cr
LazyLoad: \tab yes\cr
}
Fernandez-Duran, J.J. (2004) proposed a new family of distributions for circular
random variables based on nonnegative trigonometric sums. This package provides 
functions for working with circular distributions based on nonnegative trigonometric
sums, including functions for estimating the parameters and plotting the 
densities.

The distribution function in this package is a circular distribution based on 
nonnegative trigonometric sums (Fernandez-Duran, 2004). Fejer (1915)
expressed a univariate nonnegative trigonometric (Fourier) sum (series), for a variable
\eqn{\theta}{theta}, as the squared modulus of a sum of complex numbers, i.e.,
\deqn{\left\|\sum_{k=0}^M c_ke^{ik\theta}\right\|^2\;\;\; (1)}{||sum_{k=0}^M c_k*exp{i*k*theta}||^2        (1)}
where \eqn{i=\sqrt{-1}}{i=sqrt(-1)}. From this result, the parameters \eqn{(a_k,b_k)} for \eqn{k=1,\ldots, M} of the trigonometric sum of order \eqn{M},\eqn{T(\theta)}{T(theta)}, 
\deqn{T(\theta)=a_0 + \sum_{k=1}^M(a_kcos(k\theta) + b_ksin(k\theta))}{T(theta)=a_0 + sum_{k=1}^M(a_k*cos(k*theta) + b_k*sin(k*theta))}
are expressed in terms of the complex parameters in Equation 1
, \eqn{c_k}, for \eqn{k=0,\ldots, M}, as \eqn{a_k - ib_k=
2\sum_{\nu=0}^{n-k}c_{\nu + k}\overline{c}_{\nu}}{a_k - i*b_k=
2*sum_{\nu=0}^{n-k}c_{nu + k}*\overline{c}_{\nu}}. The additional constraint, \eqn{\sum_{k=0}^n\left\|c_k\right\|^2=\frac{1}{2\pi}=a_0}{sum_{k=0}^n||c_k||^2=1/(2*pi)=a_0}, is imposed 
to make the trigonometric sum to integrate one. Thus, \eqn{c_0} must be real and positive, and there are 2*M free parameters. Then, the probability 
density function for a circular (angular) random variable is defined 
as (Fernandez-Duran, 2004) 
\deqn{f(\theta; \underline{a},\underline{b},M)=\frac{1}{2\pi} + \frac{1}{\pi}\
\sum_{k=1}^M(a_kcos(k\theta) + b_ksin(k\theta)).}{f(theta; \underline{a},\underline{b},M)=1/(2*pi) + 1/pi*sum_{k=1}^M(a_k*cos(k*theta) + b_k*sin(k*theta)).}
Note that Equation 1 
can also be expressed as a double sum as
\deqn{\sum_{k=0}^{M}\sum_{m=0}^{M}c_k\bar{c}_me^{i(k-m)\theta}}{sum_{k=0}^{M}sum_{m=0}^{M}c_k*underline{c}_m*exp(i*(k-m)*theta).}

The \eqn{\underline{c}}{underline{c}} parameters can also be expressed in polar coordinates as \eqn{c_k=\rho_k e^{i\phi_k}}{c_k=rho_k*exp(i*phi_k)} for \eqn{\rho_k \geq 0}{rho_k >= 0} and \eqn{\phi_k \in [0,2\pi)}{phi_k en [0,2*pi)}; where \eqn{\rho_k}{rho_k} is the modulus of \eqn{c_k} and \eqn{\phi_k}{phi_k} is the argument of \eqn{c_k} for \eqn{k=1,\ldots,M}.
Many functions of the packages use as parameters the squared moduli and the arguments of \eqn{c_k}, \eqn{\rho_k^2}{rho_k^2} and \eqn{\phi_k}{phi_k}, for \eqn{k=1,\ldots,M}. We refer to the parameter \eqn{M} as the number of components in the NNTS.
}

\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez

Maintainer: Maria Mercedes Gregorio Dominguez <mercedes@itam.mx>

}
\references{
Fernandez-Duran, J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 
60(2), 499-503.

Fernandez-Duran J. J. y Gregorio Dominguez, M. M. (2008). Spherical Distributions Based on Nonnegative 
Trigonometric Sums, Working Paper, Statistics Department, ITAM, DE-C08.6

Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on 
Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
a<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
#Estimation of the NNTSdensity with 2 components for data and 1000 iterations
est<-nntsmanifoldnewtonestimation(a,2,1000)
#plot the estimated density
nntsplot(est$cestimates[,2],2)

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

b<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
estS<-nntsestimationSymmetric(2,b)
nntsplotSymmetric(estS$coef,2)

M<-c(2,3)
R<-length(M)
data(Nest)
data<-Nest
est<-mnntsmanifoldnewtonestimation(data,M,R,1000)
est
cest<-est$cestimates
mnntsplot(cest, M)

data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(4,4)
cpars<-rnorm(prod(M+1))+rnorm(prod(M+1))*complex(real=0,imaginary=1)
cpars[1]<-Re(cpars[1])
cpars<- cpars/sqrt(sum(Mod(cpars)^2))
snntsdensity(data, cpars, M)
snntsloglik(data, cpars, M)

data(Datab6fisher_ready)
data<-Datab6fisher_ready
M<-c(1,2)
cest<-snntsmanifoldnewtonestimation(data, M)
lat<-snntsmarginallatitude(seq(0,pi,.1),cest$cestimates[,3],M)
plot(seq(0,pi,.1),lat,type="l")


}
