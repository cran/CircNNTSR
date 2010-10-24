nntsloglikSymmetric <- function (cparsym = 0, M = 0, data) 
{
    size <- length(cparsym)
    if (size != M + 1) 
        return("Length of cpar must be equal to M + 1")
    if (1/(2 * pi) - sum(cparsym[1:M]) < 0) 
        return("sum of componentes greater than condition")
    y <- 0
    for (k in 1:length(data)) {
        y <- y - log(nntsSymmetricDensity(cparsym, M, data[k]))
    }
    res <- Re(y)
    return(res)
}
