\name{Datab6fisher_ready}
\Rdversion{2.1}
\alias{Datab6fisher_ready}
\docType{data}
\title{Data transformed from datab6fisher}
\description{Data transformed from datab6fisher}
\usage{data(Datab6fisher_ready)}
\format{Datab6fisher_ready}
\details{
dataaux <- datab6fisher
datab6fisher[,1] <- dataaux[,2]
datab6fisher[,2] <- dataaux[,1]
datab6fisher[,1] <- 360 - datab6fisher[,1];
datab6fisher[,2] <- 90 + datab6fisher[,2];
datab6fisher_ready <- datab6fisher*(pi/180)}
%\source{
%Fisher (1993)
%}

%\examples{
%data(datab6fisher_ready)
%## maybe str(datab6fisher_ready) ; plot(datab6fisher_ready) ...
%}
\keyword{datasets}
