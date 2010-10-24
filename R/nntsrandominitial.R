nntsrandominitial <-
function(M=1) 
{
    res <- complex(M + 1)
    aux <- rnorm(2*(M + 1))
    aux <- sqrt(1/(2 * pi)) * (aux/sqrt(sum(aux^2)))
    res[1]<-Mod(aux[1]+1i*aux[M+2])
if (M>0){
    for (k in 2:(M+1)){
	res[k] <- aux[k] + 1i*aux[k+M+1]
    }}
    return(res)

}

