nntsABDensitySymmetric <- function (cparsym = 0, M = 0, theta) 
{
    size <- length(cparsym)
    if (size != M+1) 
        return("Length of cpar must be equal to M+1")
    ab <- nntsABcoefficientsSymmetric(cparsym, M)
    y <- 1/(2 * pi)
    if (M > 0){	
    	for (k in 1:M) {
        	y <- y + ab[k] * cos(k * (theta - cparsym[M+1])) + ab[(k + M)] * sin(k * 
            	(theta - cparsym[M + 1]))
    	}
    }
    return(y)
}
