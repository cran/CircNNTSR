nntsSymmetricDensity <- function (cparsym = 0, M = 0, theta) 
{
    if (M == 0) 
        return(1/(2 * pi))
    else {
	size <- length(cparsym)
    	if (size != M + 1) 
        	return("Length of cpar must be equal to M + 1")
        if (1/(2 * pi) - sum(cparsym[1:M]) < 0) 
            return("sum of componentes greater than condition")
        else {
            cparsym0 <- sqrt(1/(2 * pi) - sum(cparsym[1:M]))
            cparsymnew <- c(cparsym0, sqrt(cparsym[1:M]))
            aux <- complex(M + 1)
            for (k in 0:M) {
                aux[k + 1] <- exp((0+1i) * k * (theta - cparsym[(M + 
                  1)]))
            }
            aux <- cparsymnew * aux
            res <- Re(sum(aux) * Conj(sum(aux)))
            return(res)
        }
    }
}




