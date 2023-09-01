\name{nntsrandominitial}
\Rdversion{2.1}
\alias{nntsrandominitial}
\title{Initial random point}
\description{This function generates a random point on the surface of the (M+1)-dimensional unit 
hypersphere}
\usage{nntsrandominitial(M=1)}

\arguments{
\item{M}{Number of components in the NNTS}
}

\value{Returns a valid initial point for the estimation functions }

\author{Juan Jose Fernandez-Duran and Maria Mercedes Gregorio-Dominguez}

\examples{
nntsrandominitial(3)
nntsrandominitial(7)
}
