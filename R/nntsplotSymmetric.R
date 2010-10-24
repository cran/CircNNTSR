nntsplotSymmetric <- function (cparsym = 0, M = 0, ...) 
{
    if (M == 0) {
        x <- rep(1/(2 * pi), 2)
        return(plot(c(0, 2 * pi), x, type = "l", xlab = "theta"))
    }
    size <- length(cparsym)
    if (size != M + 1) 
        return("Length of cpar must be equal to M + 1")
    if (1/(2 * pi) - sum(cparsym[1:M]) < 0) 
        return("sum of componentes greater than condition")

    nntsplotint <- function(theta) {
        res <- nntsABDensitySymmetric(cparsym, M, theta)
    }
    return(curve(nntsplotint, 0, 2 * pi, xlab = "theta", ...))
}
