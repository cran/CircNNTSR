nntsrandominitialSymmetric <- function (M) 
{
    res <- rep(0, M + 1)
    aux <- rnorm(M + 1)
    aux <- sqrt(1/(2 * pi)) * (aux/sqrt(sum(aux^2)))
    res[1:M] <- aux[1:M]^2
    res[(M + 1)] <- runif(1, 0, 2 * pi - 1e-14)
    return(res)
}
