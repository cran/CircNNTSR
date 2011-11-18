nntsdensity <-
function (data, cpars = 1/sqrt(2*pi),M = 0) 
{
data<-as.matrix(data)
n<-nrow(data)
R<-1
if (R != length(M))
return("Error: Dimensions of M and vector of observations are not equal")

        if (abs(sum(Mod(cpars)^2) - (1/(2 * pi))) > 0.0000000001) 
            return("sum of the squared norm of componentes greater than condition")

if (sum(M) == 0) 
            return(rep(1/(2 * pi),n))

gridcomb<-0:M

statisticsmatrix<-matrix(0,nrow=M+1,ncol=n)

statisticsmatrix<-exp(1i*gridcomb%*%t(data))

        aux <- t(cpars)%*%statisticsmatrix
        res <- aux * Conj(aux)
        return(t(Re(res)))
}

