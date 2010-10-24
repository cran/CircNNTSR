\name{CircNNTSR-package}
\Rdversion{1.0-1}
\alias{CircNNTSR-package}
\alias{CircNNTSR}
\docType{package}
\title{
CircNNTSR: An R package for the statistical analysis of circular data using nonnegative trigonometric sums (NNTS) models
}
\description{
A collection of utilities for the statistical analysis of circular data using nonnegative trigonometric sums (NNTS) models
}
\details{
\tabular{ll}{
Package: \tab CircNNTSR\cr
Type: \tab Package\cr
Version: \tab 1.0-1\cr
Date: \tab 2010-10-10\cr
License: \tab GLP (>=2)\cr
LazyLoad: \tab yes\cr
}
Fernandez-Duran, J.J. (2004) proposed a new family of distributions for circular
random variables based on nonnegative trigonometric sums. This package provides 
functions for working with circular distributions based on nonnegative trigonometric
sums, including functions for the estimation of parameters and plotting the 
densities.

The distribution function in this package is a circular distribution based on 
nonnegative trigonometric sums (Fernandez-Duran, 2004). Fejer (1915)
expressed a univariate nonnegative trigonometric (Fourier) sum (series), for a variable
\eqn{\theta}{theta}, as the squared modulus of a sum of complex numbers, i.e.,
\deqn{\left\|\sum_{k=0}^M c_ke^{ik\theta}\right\|^2\;\;\; (1)}{||sum_{k=0}^M c_k*exp{i*k*theta}||^2        (1)}
where \eqn{i=\sqrt{-1}}{i=sqrt(-1)}. From this result, the parameters \eqn{(a_k,b_k)} for 
\eqn{k=1,\ldots, M} of the trigonometric sum of order \eqn{M}, 
\deqn{T(\theta)=a_0 + \sum_{k=1}^M(a_kcos(k\theta) + b_ksin(k\theta))}{T(theta)=a_0 + sum_{k=1}^M(a_k*cos(k*theta) + b_k*sin(k*theta))}
are expressed in terms of the complex parameters in equation (1)
, \eqn{c_k}, for \eqn{k=0,\ldots, M}, as \eqn{a_k - ib_k=
2\sum_{\nu=0}^{n-k}c_{\nu + k}\overline{c}_{\nu}}{a_k - i*b_k=
2*sum_{\nu=0}^{n-k}c_{nu + k}*\overline{c}_{\nu}}. The additional 
constraint, \eqn{\sum_{k=0}^n\left\|c_k\right\|^2=\frac{1}{2\pi}=a_0}{sum_{k=0}^n||c_k||^2=1/(2*pi)=a_0}, is imposed 
to make the trigonometric sum to integrate 1. Thus, \eqn{c_0} must be real and positive and there are 2*M free parameters. Then, the probability 
density function for a circular (angular) random variable is defined 
as (Fernandez-Duran, 2004) 
\deqn{f(\theta; \underline{a},\underline{b},M)=\frac{1}{2\pi} + \frac{1}{\pi}\
\sum_{k=1}^M(a_kcos(k\theta) + b_ksin(k\theta)).}{f(theta; \underline{a},\underline{b},M)=1/(2*pi) + 1/pi*sum_{k=1}^M(a_k*cos(k*theta) + b_k*sin(k*theta)).}
Note that Equation (1) 
can also be expressed as a double sum as
\deqn{\sum_{k=0}^{M}\sum_{m=0}^{M}c_k\bar{c}_me^{i(k-m)\theta}}{sum_{k=0}^{M}sum_{m=0}^{M}c_k*underline{c}_m*exp(i*(k-m)*theta).}.

The \eqn{\underline{c}}{underline{c}} parameters can also be expressed in polar coodinates as \eqn{c_k=\rho_k e^{i\phi_k}}{c_k=rho_k*exp(i*phi_k)} for \eqn{\rho_k \geq 0}{rho_k >= 0} and \eqn{\phi_k \in [0,2\pi)}{phi_k in [0,2*pi)}; where \eqn{\rho_k}{rho_k} is the modulus of \eqn{c_k} and \eqn{\phi_k}{phi_k} is the argument of \eqn{c_k} for \eqn{k=1,\ldots,M}. We refer to the parameter \eqn{M} as the number of components in the NNTS.

The NNTS density is symmetric if \eqn{c_1,\ldots,c_m}{c_1,...,c_m} are real numbers and it is necessary to include a location parameter which we call the angle of symmetry.

For the general NNTS case, the \eqn{\underline{c}}{underline{c}} parameter is a vector of \eqn{M+1}{M+1} complex numbers such that the sum of the squared moduli is equal to \eqn{\frac{1}{2\pi}}{1/(2*pi)}. For the symmetric NNTS case, the parameter is a vector of \eqn{M+1}{M+1} real numbers with the first \eqn{M}{M} components being the squared moduli of \eqn{c_1,\ldots,c_M}{c_1,...,c_M} (\eqn{\rho^2_1,\ldots,\rho^2_M}{rho^2_1,...,rho^2_M}) and such that their sum is less than or equal to \eqn{\frac{1}{2\pi}}{1/(2*pi)}; the element \eqn{M+1}{M+1} is the angle of symmetry.
}

\author{
Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez

Maintainer: Maria Mercedes Gregorio Dominguez <mercedes@itam.mx>

}
\references{
Fernandez-Duran, J.J. (2004). Circular Distributions Based on Nonnegative Trigonometric Sums, Biometrics, 60(2), 499-503.

Fernandez-Duran, J.J., Gregorio-Dominguez, M.M. (2009) Symmetric Circular Distributions Based on Nonnegative Trigonometric Sums. Working Paper, DE-C09.12, Department of Statistics, ITAM, Mexico
}

\keyword{ package }

\examples{
a<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
#Estimation of the NNTSdensity with 2 components for data and 1000 iterations
est<-nntsmanifoldnewtonestimation(a,2,1000)
#plot the estimated density
nntsplot(est$cestimates[,2],2)

data(Turtles_radians)
#Empirical analysis of data
Turtles_hist<-hist(Turtles_radians,breaks=10,freq=FALSE)
#Estimation of the NNTS density with 3 components for data
est<-nntsmanifoldnewtonestimation(Turtles_radians,3)
est
#plot the estimated density
nntsplot(est$cestimates[,2],3)
#add the histogram to the estimated density plot
plot(Turtles_hist, freq=FALSE, add=TRUE)

b<-c(runif(10,3*pi/2,2*pi-0.00000001),runif(10,pi/2,pi-0.00000001))
estS<-nntsestimationSymmetric(2,b)
nntsplotSymmetric(estS$coef,2)

#Plots of randomly generated NNTS models with different values of M
nntsplot(nntsrandominitial(1),1)
nntsplot(nntsrandominitial(2),2)
nntsplot(nntsrandominitial(3),3)
nntsplot(nntsrandominitial(4),4)
nntsplot(nntsrandominitial(5),5)
nntsplot(nntsrandominitial(6),6)
nntsplot(nntsrandominitial(7),7)
nntsplot(nntsrandominitial(8),8)
#and for symmetric NNTS models
nntsplotSymmetric(nntsrandominitialSymmetric(1),1)
nntsplotSymmetric(nntsrandominitialSymmetric(2),2)
nntsplotSymmetric(nntsrandominitialSymmetric(3),3)
nntsplotSymmetric(nntsrandominitialSymmetric(4),4)
nntsplotSymmetric(nntsrandominitialSymmetric(5),5)
nntsplotSymmetric(nntsrandominitialSymmetric(6),6)
nntsplotSymmetric(7,nntsrandominitialSymmetric(7))
nntsplotSymmetric(8,nntsrandominitialSymmetric(8))
}
