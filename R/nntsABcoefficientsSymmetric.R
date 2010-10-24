nntsABcoefficientsSymmetric <- function (cparsym = 0, M = 0) 
{
    if (M == 0) 
        return("Uniform case: No coefficients AB")


    size <- length(cparsym)
    if (size != M + 1) 
        return("Length of cpar must be equal to M + 1")


    if (M > 0) {
        if (1/(2 * pi) - sum(cparsym[1:M]) < 0) 
            return("sum of componentes greater than condition")
        else {
            aux <- complex(M + 1)
            aux[1] <- sqrt(1/(2 * pi) - sum(cparsym[1:M]))
            for (k in 1:M) {
                aux[k + 1] <- sqrt(cparsym[k])
            }
            aux2 <- complex(M)
            for (j in 1:M) {
                for (k in 1:(M - j + 1)) {
                  aux2[j] <- aux2[j] + 2 * aux[(k + j)] * Conj(aux[k])
                }
            }
            ab <- rep(0, 2 * M)
            for (k in 1:M) {
                ab[k] = Re(aux2[k])
                ab[(k + M)] = -Im(aux2[k])
            }
        }
    }
    return(ab)
}