snntsdensityplot <- function (long,lat, cpars = 1, M = c(0,0)) 
{
    n <- length(long)
    R <- 2
    if (R != length(M)) 
        return("Error: Dimensions of M and vector of observations are not equal")
    auxcond1 <- sum(long > 2*pi) + sum(long < 0)
    auxcond2 <- sum(lat > pi) + sum(lat < 0)
    if (auxcond1>0)
	return("Longitude vector must have values between 0 and 2*pi")
    if (auxcond2>0)
	return("Latitude vector must have values between 0 and pi")
    if (abs(sum(Mod(cpars)^2) - 1) > 1e-10) 
        return("sum of the squared norm of componentes greater than condition")
    if (sum(M) == 0)
#	return(sin(data[,2])/(4*pi))
	return(sin(lat)/(4*pi))

    statisticsmatrix <- matrix(0, nrow = (M[1] + 1)*(M[2] + 1), ncol = n)

    for (j in 1:length(long)){
    	for (k1 in 0:M[1]){
		for (k2 in 0:M[2]){ 
			statisticsmatrix[k1*(M[2]+1) + k2 + 1,j] <- sqrt(sin(lat[j])) * exp((0+1i) * (k1 * long[j] + k2 * lat[j]))
		}
	}
    }

    A <- matrix(0,nrow=M[2]+1,ncol=M[2]+1)

    for (k2 in 0:M[2]){
    	for (m2 in 0:M[2]){
        	if (abs(k2 - m2) != 1){
            		A[k2+1,m2+1] <- (2*pi)*((1 + cos((k2-m2)*pi))/(1 - ((k2-m2)^2)));
 		}
	}
    }

    Ac<-chol(A)

    Acinv<-solve(Ac)	

    for (j in 1:length(long)){
    	for (k1 in 0:M[1]){
		statisticsmatrix[(k1*(M[2]+1)+1):((k1+1)*(M[2]+1)),j] <- t(Acinv) %*% statisticsmatrix[(k1*(M[2]+1)+1):((k1+1)*(M[2]+1)),j]
	}
    }

    aux <- t(cpars)%*%statisticsmatrix
    res <- aux * Conj(aux)
    return(t(Re(res)))
}