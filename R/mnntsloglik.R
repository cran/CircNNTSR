mnntsloglik<-function (data, cpars = 1/sqrt(2 * pi), M = 0, R=1) 
{
    if (sum(M) == 0) 
        return(-nrow(data) * R * log(2 * pi))
    size <- length(cpars)
    if (size != prod(M + 1)) 
        return("Length of cpars must be equal to prod(M+1)")
    if (abs(sum(Mod(cpars)^2) - ((1/(2 * pi))^R)) > 1e-10) 
        return("sum of the squared norm of componentes greater than condition")
    y <- sum(log(mnntsdensity(data, cpars, M, R)))
    return(y)
}